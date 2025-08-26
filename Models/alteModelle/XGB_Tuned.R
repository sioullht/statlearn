packages <- c("tidyverse","caret","pROC","xgboost","scales","Matrix","doParallel","ggplot2")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg, dependencies = TRUE)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)


# Daten laden & Zielvariable
train_data <- readr::read_csv("Step3/train_data.csv", show_col_types = FALSE)
test_data  <- readr::read_csv("Step3/test_data.csv",  show_col_types = FALSE)
train_data$y <- factor(train_data$y, levels=c(0,1), labels=c("neg","pos"))
test_data$y  <- factor(test_data$y,  levels=c(0,1), labels=c("neg","pos"))


# Ausgabepfade
out_dir  <- "Models/XGB_Tuned"
plot_dir <- file.path(out_dir, "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(out_dir, "train_log.txt")

n_cores <- max(1, parallel::detectCores()-1)
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

# Cross-Validation
control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",     
  allowParallel = TRUE
)


xgb_grid <- expand.grid(
  nrounds = c(300, 600, 900),
  max_depth = c(2, 3, 4),
  eta = c(0.03, 0.05, 0.1),
  gamma = c(0, 1, 2),
  colsample_bytree = c(0.6, 0.8),
  min_child_weight = c(3, 5, 7),
  subsample = c(0.7, 0.9)
)

# Class balance 
pos_rate <- mean(train_data$y == "pos"); neg_rate <- 1 - pos_rate
scale_pos_w <- neg_rate / pos_rate
if (!is.finite(scale_pos_w) || abs(scale_pos_w - 1) < 0.2) scale_pos_w <- 1


# Logging
grid_combos <- nrow(xgb_grid); cv_folds <- control$number
est_fits <- grid_combos * cv_folds
cat(
  "========================\n",
  "Run gestartet: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
  "Cores: ", n_cores, "\n",
  "CV-Folds: ", cv_folds, "\n",
  "Grid-Kombinationen: ", grid_combos, "\n",
  "Fits (Grid x Folds): ", est_fits, "\n",
  "pos_rate: ", round(pos_rate, 4), " | scale_pos_weight: ", round(scale_pos_w, 4), "\n",
  "------------------------\n",
  file = log_file, append = TRUE, sep = ""
)

# Training
start_time <- Sys.time()
message("Training gestartet um: ", format(start_time, "%Y-%m-%d %H:%M:%S"))

model_xgb_tuned <- caret::train(
  y ~ ., data = train_data,
  method    = "xgbTree",
  trControl = control,
  tuneGrid  = xgb_grid,
  metric    = "ROC",
  verbose   = FALSE,
  preProcess = c("YeoJohnson","center","scale","nzv"),
  scale_pos_weight = scale_pos_w
)

end_time <- Sys.time()
duration_min <- as.numeric(difftime(end_time, start_time, units = "mins"))
message("Training beendet um: ", format(end_time, "%Y-%m-%d %H:%M:%S"))
message("Gesamtdauer (Minuten): ", round(duration_min, 2))

bt <- model_xgb_tuned$bestTune
cat(
  "Training gestartet um: ", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n",
  "Training beendet  um: ", format(end_time,   "%Y-%m-%d %H:%M:%S"), "\n",
  "Gesamtdauer (Minuten): ", round(duration_min, 2), "\n",
  "BestTune: ", paste0(capture.output(print(bt)), collapse=" "), "\n",
  "========================\n",
  file = log_file, append = TRUE, sep = ""
)

print(model_xgb_tuned)

# Platt Scaling (OOF Kalibrierung)
oof_full <- model_xgb_tuned$pred
oof_best <- subset(
  oof_full,
  nrounds==bt$nrounds & max_depth==bt$max_depth & eta==bt$eta &
  gamma==bt$gamma & colsample_bytree==bt$colsample_bytree &
  min_child_weight==bt$min_child_weight & subsample==bt$subsample
)

oof_df <- data.frame(
  y = factor(oof_best$obs, levels=c("neg","pos")),
  oof = oof_best$pos
)
cal_glm <- glm(I(y=="pos") ~ oof, data=oof_df, family=binomial())

calibrate_probs <- function(p) predict(cal_glm, newdata = data.frame(oof = p), type = "response")

auc_oof_raw <- as.numeric(pROC::auc(pROC::roc(oof_df$y, oof_df$oof, levels=c("neg","pos"), direction="<", quiet=TRUE)))
auc_oof_cal <- as.numeric(pROC::auc(pROC::roc(oof_df$y, calibrate_probs(oof_df$oof), levels=c("neg","pos"), direction="<", quiet=TRUE)))
cat(sprintf("OOF AUC raw=%.4f | calibrated=%.4f\n", auc_oof_raw, auc_oof_cal), file=log_file, append=TRUE)

# Cutoff-Tuning (F1 optimal auf OOF)
ths <- seq(0.05, 0.95, by=0.01)
f1s <- sapply(ths, function(t){
  pr <- factor(ifelse(oof_df$oof >= t, "pos","neg"), levels=c("neg","pos"))
  caret::confusionMatrix(pr, oof_df$y, positive="pos")$byClass["F1"]
})
best_thr <- ths[which.max(f1s)]
message(sprintf("Gewählter Cutoff (F1-optimal, OOF): %.2f", best_thr))
cat("Gewählter Cutoff (F1-optimal, OOF): ", round(best_thr, 3), "\n", file = log_file, append = TRUE)

# Predictions: TRAIN & TEST (RAW + CAL)
pred_train_prob_raw <- predict(model_xgb_tuned, train_data, type="prob")[, "pos"]
pred_test_prob_raw  <- predict(model_xgb_tuned, test_data,  type="prob")[, "pos"]

pred_train_prob_cal <- calibrate_probs(pred_train_prob_raw)
pred_test_prob_cal  <- calibrate_probs(pred_test_prob_raw)

pred_train_class_raw <- factor(ifelse(pred_train_prob_raw >= best_thr, "pos","neg"), levels=c("neg","pos"))
pred_test_class_raw  <- factor(ifelse(pred_test_prob_raw  >= best_thr, "pos","neg"), levels=c("neg","pos"))

pred_train_class_cal <- factor(ifelse(pred_train_prob_cal >= best_thr, "pos","neg"), levels=c("neg","pos"))
pred_test_class_cal  <- factor(ifelse(pred_test_prob_cal  >= best_thr, "pos","neg"), levels=c("neg","pos"))

# ROC / AUC
roc_train_raw <- pROC::roc(train_data$y, pred_train_prob_raw, levels=c("neg","pos"), direction="<", quiet=TRUE)
roc_test_raw  <- pROC::roc(test_data$y,  pred_test_prob_raw,  levels=c("neg","pos"), direction="<", quiet=TRUE)
auc_train_raw <- as.numeric(pROC::auc(roc_train_raw)); auc_test_raw <- as.numeric(pROC::auc(roc_test_raw))

roc_train_cal <- pROC::roc(train_data$y, pred_train_prob_cal, levels=c("neg","pos"), direction="<", quiet=TRUE)
roc_test_cal  <- pROC::roc(test_data$y,  pred_test_prob_cal,  levels=c("neg","pos"), direction="<", quiet=TRUE)
auc_train_cal <- as.numeric(pROC::auc(roc_train_cal)); auc_test_cal <- as.numeric(pROC::auc(roc_test_cal))

# Confusion Matrices
cm_train_raw <- caret::confusionMatrix(pred_train_class_raw, train_data$y, positive="pos")
cm_test_raw  <- caret::confusionMatrix(pred_test_class_raw,  test_data$y,  positive="pos")
cm_train_cal <- caret::confusionMatrix(pred_train_class_cal, train_data$y, positive="pos")
cm_test_cal  <- caret::confusionMatrix(pred_test_class_cal,  test_data$y,  positive="pos")

# Metriken
eps <- 1e-15; clip <- function(p) pmin(pmax(p, eps), 1 - eps)
y01_tr <- as.numeric(train_data$y == "pos")
y01_te <- as.numeric(test_data$y == "pos")

mk_metrics <- function(cm_tr, cm_te, auc_tr, auc_te, p_tr, p_te) {
  logloss_tr <- -mean(y01_tr * log(clip(p_tr)) + (1 - y01_tr) * log(clip(1 - p_tr)))
  logloss_te <- -mean(y01_te * log(clip(p_te)) + (1 - y01_te) * log(clip(1 - p_te)))
  brier_tr   <- mean((y01_tr - p_tr)^2)
  brier_te   <- mean((y01_te - p_te)^2)
  list(
    Train = c(
      Accuracy    = cm_tr$overall["Accuracy"],
      Precision   = cm_tr$byClass["Precision"],
      Recall      = cm_tr$byClass["Recall"],
      F1_Score    = cm_tr$byClass["F1"],
      AUC         = auc_tr,
      Log_Loss    = logloss_tr,
      Brier_Score = brier_tr
    ),
    Test  = c(
      Accuracy    = cm_te$overall["Accuracy"],
      Precision   = cm_te$byClass["Precision"],
      Recall      = cm_te$byClass["Recall"],
      F1_Score    = cm_te$byClass["F1"],
      AUC         = auc_te,
      Log_Loss    = logloss_te,
      Brier_Score = brier_te
    )
  )
}

met_raw <- mk_metrics(cm_train_raw, cm_test_raw, auc_train_raw, auc_test_raw, pred_train_prob_raw, pred_test_prob_raw)
met_cal <- mk_metrics(cm_train_cal, cm_test_cal, auc_train_cal, auc_test_cal, pred_train_prob_cal, pred_test_prob_cal)

metrics_raw_df <- rbind(Train=met_raw$Train, Test=met_raw$Test) %>% as.data.frame()
metrics_cal_df <- rbind(Train=met_cal$Train, Test=met_cal$Test) %>% as.data.frame()

cat("\n--- Metriken RAW ---\n"); print(round(metrics_raw_df, 4))
cat("\n--- Metriken CAL ---\n"); print(round(metrics_cal_df, 4))

# Speicheren der Metriken
capture.output(cm_train_raw, file = file.path(out_dir, "confusion_matrix_train_RAW.txt"))
capture.output(cm_test_raw,  file = file.path(out_dir, "confusion_matrix_test_RAW.txt"))
capture.output(cm_train_cal, file = file.path(out_dir, "confusion_matrix_train_CAL.txt"))
capture.output(cm_test_cal,  file = file.path(out_dir, "confusion_matrix_test_CAL.txt"))

metrics_raw_df %>% tibble::rownames_to_column("Split") %>% readr::write_csv(file.path(out_dir, "metrics_train_vs_test_RAW.csv"))
metrics_cal_df %>% tibble::rownames_to_column("Split") %>% readr::write_csv(file.path(out_dir, "metrics_train_vs_test_CAL.csv"))

# ROC Plots
p_train_raw <- pROC::ggroc(roc_train_raw) + ggplot2::ggtitle(sprintf("ROC RAW (TRAIN) — AUC = %.3f", auc_train_raw)) + ggplot2::theme_minimal()
p_test_raw  <- pROC::ggroc(roc_test_raw)  + ggplot2::ggtitle(sprintf("ROC RAW (TEST) — AUC = %.3f",  auc_test_raw))  + ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train_RAW.pdf"), plot = p_train_raw, width = 6, height = 5)
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Test_RAW.pdf"),  plot = p_test_raw,  width = 6, height = 5)

# ROC Plots
p_train_cal <- pROC::ggroc(roc_train_cal) + ggplot2::ggtitle(sprintf("ROC CAL (TRAIN) — AUC = %.3f", auc_train_cal)) + ggplot2::theme_minimal()
p_test_cal  <- pROC::ggroc(roc_test_cal)  + ggplot2::ggtitle(sprintf("ROC CAL (TEST) — AUC = %.3f",  auc_test_cal))  + ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train_CAL.pdf"), plot = p_train_cal, width = 6, height = 5)
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Test_CAL.pdf"),  plot = p_test_cal,  width = 6, height = 5)

# Feature Importance (Gain)
final_booster <- model_xgb_tuned$finalModel
feature_names <- setdiff(names(train_data), "y")

imp <- xgboost::xgb.importance(feature_names = feature_names, model = final_booster)
readr::write_csv(imp, file.path(out_dir, "xgb_feature_importance_gain.csv"))

pdf(file.path(plot_dir, "XGB_Feature_Importance_Gain.pdf"), width=7, height=6)
xgboost::xgb.plot.importance(imp, top_n = min(30, nrow(imp)))
dev.off()

# Modell speichern 
saveRDS(model_xgb_tuned, file.path(out_dir, "model_xgb_tuned.rds"))

parallel::stopCluster(cl)
doParallel::registerDoSEQ()

message("Fertig. Alle Ergebnisse unter: ", normalizePath(out_dir))