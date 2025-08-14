packages <- c("tidyverse", "caret", "pROC", "xgboost", "scales", "Matrix")

installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!pkg %in% installed) install.packages(pkg)
}
lapply(packages, library, character.only = TRUE)

# -------------------------------------------------------
# Daten laden und vorbereiten
# -------------------------------------------------------
train_data <- read_csv("Step3/train_data.csv")
test_data  <- read_csv("Step3/test_data.csv")

train_data$y <- factor(train_data$y, levels = c(0,1), labels = c("neg", "pos"))
test_data$y  <- factor(test_data$y,  levels = c(0,1), labels = c("neg", "pos"))

# -------------------------------------------------------
# Tuning & Modelltraining: XGBoost
# -------------------------------------------------------
set.seed(123)

control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 6),
  eta = c(0.05, 0.1),
  gamma = c(0, 1),
  colsample_bytree = c(0.6, 0.8),
  min_child_weight = c(1, 5),
  subsample = c(0.8)
)

model_xgb <- train(
  y ~ ., data = train_data,
  method = "xgbTree",
  trControl = control,
  tuneGrid = xgb_grid,
  metric = "ROC",
  verbose = FALSE
)

print(model_xgb)
plot(model_xgb)

# -------------------------------------------------------
# Evaluation XGBoost
# -------------------------------------------------------
pred_class <- predict(model_xgb, test_data)
pred_prob  <- predict(model_xgb, test_data, type = "prob")[, "pos"]

# ROC & Metriken
roc_obj <- roc(test_data$y, pred_prob)
auc_val <- auc(roc_obj)

cm <- confusionMatrix(pred_class, test_data$y, positive = "pos")
logloss <- -mean(as.numeric(test_data$y == "pos") * log(pred_prob) + (1 - as.numeric(test_data$y == "pos")) * log(1 - pred_prob))
brier <- mean((as.numeric(test_data$y == "pos") - pred_prob)^2)

metrics_xgb <- data.frame(
  Accuracy = cm$overall["Accuracy"],
  Precision = cm$byClass["Precision"],
  Recall = cm$byClass["Recall"],
  F1_Score = cm$byClass["F1"],
  AUC = auc_val,
  Log_Loss = logloss,
  Brier_Score = brier
)

dir.create("Models/XGB_Tuned", recursive = TRUE, showWarnings = FALSE)
saveRDS(model_xgb, "Models/XGB_Tuned/model_xgb_tuned.rds")
write.csv(metrics_xgb, "Models/XGB_Tuned/metrics_xgb.csv", row.names = FALSE)