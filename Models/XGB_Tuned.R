# -------------------------------------------------------
# Pakete
# -------------------------------------------------------
packages <- c("tidyverse","caret","pROC","xgboost","scales","Matrix","doParallel","ggplot2")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

# -------------------------------------------------------
# Daten laden & Zielvariable
# -------------------------------------------------------
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")
train_data$y <- factor(train_data$y, levels=c(0,1), labels=c("neg","pos"))
test_data$y  <- factor(test_data$y,  levels=c(0,1), labels=c("neg","pos"))

# -------------------------------------------------------
# Ausgabepfade
# -------------------------------------------------------
out_dir  <- "Models/XGB_Tuned"
plot_dir <- file.path(out_dir, "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(out_dir, "train_log.txt")

# -------------------------------------------------------
# Parallelisierung
# -------------------------------------------------------
n_cores <- max(1, parallel::detectCores()-1)
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

# -------------------------------------------------------
# TrainControl
# -------------------------------------------------------
control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  allowParallel = TRUE
)

# -------------------------------------------------------
# Kleines Anti-Overfitting-Grid
# -------------------------------------------------------
xgb_grid <- expand.grid(
  nrounds = c(300, 600, 900),
  max_depth = c(2, 3, 4),
  eta = c(0.03, 0.05, 0.1),
  gamma = c(0, 1, 2),
  colsample_bytree = c(0.6, 0.8),
  min_child_weight = c(3, 5, 7),
  subsample = c(0.7, 0.9)
)

# Optional: scale_pos_weight (nur wenn unbalanciert)
pos_rate <- mean(train_data$y == "pos")
neg_rate <- 1 - pos_rate
scale_pos_w <- neg_rate / pos_rate  # ~1 bei Balance

# -------------------------------------------------------
# Logging: Setup-Infos
# -------------------------------------------------------
grid_combos <- nrow(xgb_grid)
cv_folds    <- control$number
est_fits    <- grid_combos * cv_folds

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

# -------------------------------------------------------
# Zeitmessung starten
# -------------------------------------------------------
start_time <- Sys.time()
message("Training gestartet um: ", format(start_time, "%Y-%m-%d %H:%M:%S"))

# -------------------------------------------------------
# Training
# -------------------------------------------------------
model_xgb_tuned <- train(
  y ~ ., data = train_data,
  method = "xgbTree",
  trControl = control,
  tuneGrid = xgb_grid,
  metric = "ROC",
  verbose = FALSE,
  preProcess = c("YeoJohnson","center","scale","nzv"),
  scale_pos_weight = scale_pos_w
)

# -------------------------------------------------------
# Zeitmessung stoppen & Dauer loggen
# -------------------------------------------------------
end_time <- Sys.time()
duration_min <- as.numeric(difftime(end_time, start_time, units = "mins"))
message("Training beendet um: ", format(end_time, "%Y-%m-%d %H:%M:%S"))
message("Gesamtdauer (Minuten): ", round(duration_min, 2))

# Log schreiben (inkl. bestTune)
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

# -------------------------------------------------------
# Cutoff-Tuning auf OOF-Predictions
# -------------------------------------------------------
best <- model_xgb_tuned$bestTune
oof <- subset(model_xgb_tuned$pred,
              nrounds==best$nrounds & max_depth==best$max_depth & eta==best$eta &
              gamma==best$gamma & colsample_bytree==best$colsample_bytree &
              min_child_weight==best$min_child_weight & subsample==best$subsample)

ths <- seq(0.05, 0.95, by=0.01)
f1s <- sapply(ths, function(t){
  pr <- factor(ifelse(oof$pos >= t, "pos","neg"), levels=c("neg","pos"))
  caret::confusionMatrix(pr, oof$obs, positive="pos")$byClass["F1"]
})
best_thr <- ths[which.max(f1s)]
message(sprintf("Gewählter Cutoff (F1-optimal, OOF): %.2f", best_thr))
cat("Gewählter Cutoff (F1-optimal, OOF): ", round(best_thr, 3), "\n", file = log_file, append = TRUE)

# -------------------------------------------------------
# Predictions: TRAIN & TEST
# -------------------------------------------------------
pred_train_prob  <- predict(model_xgb_tuned, train_data, type="prob")[, "pos"]
pred_train_class <- factor(ifelse(pred_train_prob >= best_thr, "pos","neg"), levels=c("neg","pos"))

pred_test_prob   <- predict(model_xgb_tuned, test_data,  type="prob")[, "pos"]
pred_test_class  <- factor(ifelse(pred_test_prob >= best_thr, "pos","neg"), levels=c("neg","pos"))

# -------------------------------------------------------
# ROC / AUC
# -------------------------------------------------------
roc_train <- pROC::roc(train_data$y, pred_train_prob, levels=c("neg","pos"), direction="<")
auc_train <- as.numeric(pROC::auc(roc_train))

roc_test  <- pROC::roc(test_data$y,  pred_test_prob,  levels=c("neg","pos"), direction="<")
auc_test  <- as.numeric(pROC::auc(roc_test))

# -------------------------------------------------------
# Confusion Matrices
# -------------------------------------------------------
cm_train <- caret::confusionMatrix(pred_train_class, train_data$y, positive="pos")
cm_test  <- caret::confusionMatrix(pred_test_class,  test_data$y,  positive="pos")

# -------------------------------------------------------
# Metriken
# -------------------------------------------------------
eps <- 1e-15
# TRAIN
pp_tr  <- pmin(pmax(pred_train_prob, eps), 1 - eps)
y01_tr <- as.numeric(train_data$y == "pos")
logloss_tr <- -mean(y01_tr * log(pp_tr) + (1 - y01_tr) * log(1 - pp_tr))
brier_tr   <- mean((y01_tr - pred_train_prob)^2)

# TEST
pp_te  <- pmin(pmax(pred_test_prob, eps), 1 - eps)
y01_te <- as.numeric(test_data$y == "pos")
logloss_te <- -mean(y01_te * log(pp_te) + (1 - y01_te) * log(1 - pp_te))
brier_te   <- mean((y01_te - pred_test_prob)^2)

metrics_train <- c(
  Accuracy    = cm_train$overall["Accuracy"],
  Precision   = cm_train$byClass["Precision"],
  Recall      = cm_train$byClass["Recall"],
  F1_Score    = cm_train$byClass["F1"],
  AUC         = auc_train,
  Log_Loss    = logloss_tr,
  Brier_Score = brier_tr
)

metrics_test <- c(
  Accuracy    = cm_test$overall["Accuracy"],
  Precision   = cm_test$byClass["Precision"],
  Recall      = cm_test$byClass["Recall"],
  F1_Score    = cm_test$byClass["F1"],
  AUC         = auc_test,
  Log_Loss    = logloss_te,
  Brier_Score = brier_te
)

metrics_train_test <- rbind(Train = metrics_train, Test = metrics_test) %>% as.data.frame()
print(round(metrics_train_test, 4))

# -------------------------------------------------------
# Speicherung
# -------------------------------------------------------
# Confusion Matrices als TXT
capture.output(cm_train, file = file.path(out_dir, "confusion_matrix_train.txt"))
capture.output(cm_test,  file = file.path(out_dir, "confusion_matrix_test.txt"))

# Metriken speichern
metrics_train_test %>%
  tibble::rownames_to_column("Split") %>%
  readr::write_csv(file.path(out_dir, "metrics_train_vs_test.csv"))

# -------------------------------------------------------
# ROC-Plots
# -------------------------------------------------------
p_train <- pROC::ggroc(roc_train) +
  ggplot2::ggtitle(sprintf("ROC (TRAIN) — AUC = %.3f", auc_train)) +
  ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train.pdf"), plot = p_train, width = 6, height = 5)

p_test <- pROC::ggroc(roc_test) +
  ggplot2::ggtitle(sprintf("ROC (TEST) — AUC = %.3f", auc_test)) +
  ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Test.pdf"), plot = p_test, width = 6, height = 5)

p_both <- pROC::ggroc(list(Train = roc_train, Test = roc_test)) +
  ggplot2::ggtitle(sprintf("ROC — Train (AUC=%.3f) vs. Test (AUC=%.3f)", auc_train, auc_test)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(linetype = "Split", color = "Split")
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train_vs_Test.pdf"), plot = p_both, width = 6, height = 5)

# -------------------------------------------------------
# Modell speichern
# -------------------------------------------------------
saveRDS(model_xgb_tuned, file.path(out_dir, "model_xgb_tuned.rds"))

# Cluster stoppen
parallel::stopCluster(cl); doParallel::registerDoSEQ()

message("Fertig. Alle Ergebnisse unter: ", normalizePath(out_dir))
