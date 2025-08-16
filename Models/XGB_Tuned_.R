# -------------------------------------------------------
# Pakete
# -------------------------------------------------------
packages <- c("tidyverse","pROC","xgboost","ggplot2")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

# -------------------------------------------------------
# Daten laden & Zielvariable
# -------------------------------------------------------
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

y_train <- train_data$y
y_test  <- test_data$y
train_data$y <- NULL
test_data$y  <- NULL

# Faktor -> numerisch
y_train <- as.numeric(y_train == 1)
y_test  <- as.numeric(y_test == 1)

# Matrix für xgboost
X_train <- as.matrix(train_data)
X_test  <- as.matrix(test_data)

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

# -------------------------------------------------------
# Ausgabepfade
# -------------------------------------------------------
out_dir  <- "Models/XGB_Tuned_"
plot_dir <- file.path(out_dir, "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------
# Parameter-Suchraum (klein & zufällig)
# -------------------------------------------------------
param_grid <- expand.grid(
  max_depth        = 2:6,
  eta              = c(0.01, 0.03, 0.05, 0.1),
  gamma            = c(0, 0.5, 1),
  colsample_bytree = c(0.6, 0.8, 1),
  min_child_weight = c(1, 3, 5),
  subsample        = c(0.7, 0.9, 1)
)

# Zufällige Auswahl
set.seed(123)
param_candidates <- param_grid[sample(1:nrow(param_grid), 30), ]  # nur 30 Kandidaten

# -------------------------------------------------------
# Cross Validation + Early Stopping
# -------------------------------------------------------
best_auc <- 0
best_model <- NULL
best_params <- NULL
best_nrounds <- NULL

for (i in 1:nrow(param_candidates)) {
  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = param_candidates$max_depth[i],
    eta = param_candidates$eta[i],
    gamma = param_candidates$gamma[i],
    colsample_bytree = param_candidates$colsample_bytree[i],
    min_child_weight = param_candidates$min_child_weight[i],
    subsample = param_candidates$subsample[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 1000,
    nfold = 5,
    early_stopping_rounds = 30,
    verbose = 0,
    maximize = TRUE,
    stratified = TRUE
  )
  
  mean_auc <- max(cv$evaluation_log$test_auc_mean)
  best_iter <- cv$best_iteration
  
  if (mean_auc > best_auc) {
    best_auc <- mean_auc
    best_params <- params
    best_nrounds <- best_iter
  }
}

cat("Beste AUC (CV):", best_auc, "\n")
cat("Beste Parameter:\n")
print(best_params)
cat("Beste Runden:", best_nrounds, "\n")

# -------------------------------------------------------
# Finales Modell trainieren
# -------------------------------------------------------
final_model <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 30,
  verbose = 1
)

# -------------------------------------------------------
# Predictions
# -------------------------------------------------------
pred_train_prob <- predict(final_model, dtrain)
pred_test_prob  <- predict(final_model, dtest)

# ROC / AUC
roc_train <- pROC::roc(y_train, pred_train_prob)
roc_test  <- pROC::roc(y_test,  pred_test_prob)
auc_train <- as.numeric(pROC::auc(roc_train))
auc_test  <- as.numeric(pROC::auc(roc_test))

# Cutoff optimieren (F1)
ths <- seq(0.05, 0.95, by=0.01)
f1s <- sapply(ths, function(t){
  pr <- ifelse(pred_train_prob >= t, 1, 0)
  cm <- caret::confusionMatrix(factor(pr, levels=c(0,1)),
                               factor(y_train, levels=c(0,1)),
                               positive="1")
  cm$byClass["F1"]
})
best_thr <- ths[which.max(f1s)]
cat("Optimaler Cutoff (Train, F1-optimal):", best_thr, "\n")

pred_train_class <- ifelse(pred_train_prob >= best_thr, 1, 0)
pred_test_class  <- ifelse(pred_test_prob  >= best_thr, 1, 0)

# Confusion Matrix
cm_train <- caret::confusionMatrix(factor(pred_train_class, levels=c(0,1)),
                                   factor(y_train, levels=c(0,1)),
                                   positive="1")
cm_test  <- caret::confusionMatrix(factor(pred_test_class, levels=c(0,1)),
                                   factor(y_test, levels=c(0,1)),
                                   positive="1")

# -------------------------------------------------------
# Metriken
# -------------------------------------------------------
eps <- 1e-15
logloss <- function(y, p) {
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}
brier <- function(y, p) mean((y - p)^2)

metrics_train <- c(
  Accuracy    = cm_train$overall["Accuracy"],
  Precision   = cm_train$byClass["Precision"],
  Recall      = cm_train$byClass["Recall"],
  F1_Score    = cm_train$byClass["F1"],
  AUC         = auc_train,
  Log_Loss    = logloss(y_train, pred_train_prob),
  Brier_Score = brier(y_train, pred_train_prob)
)
metrics_test <- c(
  Accuracy    = cm_test$overall["Accuracy"],
  Precision   = cm_test$byClass["Precision"],
  Recall      = cm_test$byClass["Recall"],
  F1_Score    = cm_test$byClass["F1"],
  AUC         = auc_test,
  Log_Loss    = logloss(y_test, pred_test_prob),
  Brier_Score = brier(y_test, pred_test_prob)
)
metrics_train_test <- rbind(Train = metrics_train, Test = metrics_test) %>% as.data.frame()
print(round(metrics_train_test, 4))

# -------------------------------------------------------
# Speicherung
# -------------------------------------------------------
saveRDS(final_model, file.path(out_dir, "model_xgb_tuned_cv.rds"))
readr::write_csv(
  tibble::rownames_to_column(metrics_train_test, "Split"),
  file.path(out_dir, "metrics_train_vs_test.csv")
)
capture.output(cm_train, file = file.path(out_dir, "confusion_matrix_train.txt"))
capture.output(cm_test,  file = file.path(out_dir, "confusion_matrix_test.txt"))

# ROC Plots
p_train <- pROC::ggroc(roc_train) +
  ggplot2::ggtitle(sprintf("ROC (TRAIN) — AUC = %.3f", auc_train)) +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(plot_dir, "ROC_Train.pdf"), p_train, width=6, height=5)

p_test <- pROC::ggroc(roc_test) +
  ggplot2::ggtitle(sprintf("ROC (TEST) — AUC = %.3f", auc_test)) +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(plot_dir, "ROC_Test.pdf"), p_test, width=6, height=5)

p_both <- pROC::ggroc(list(Train=roc_train, Test=roc_test)) +
  ggplot2::ggtitle(sprintf("ROC — Train (%.3f) vs. Test (%.3f)", auc_train, auc_test)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(linetype="Split", color="Split")
ggplot2::ggsave(file.path(plot_dir, "ROC_Train_vs_Test.pdf"), p_both, width=6, height=5)

message("Fertig. Ergebnisse unter: ", normalizePath(out_dir))