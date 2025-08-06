packages <- c("tidyverse", "caret", "pROC", "lightgbm", "scales", "Matrix")

installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!pkg %in% installed) install.packages(pkg)
}
lapply(packages, library, character.only = TRUE)


library(lightgbm)
library(Matrix)

# -------------------------------------------------------
# Daten vorbereiten für LightGBM
# -------------------------------------------------------
train_data <- read_csv("Step3/train_data.csv")
test_data  <- read_csv("Step3/test_data.csv")

train_data$y <- as.numeric(train_data$y)
test_data$y  <- as.numeric(test_data$y)

X_train <- as.matrix(select(train_data, -y))
y_train <- train_data$y

X_test <- as.matrix(select(test_data, -y))
y_test <- test_data$y

dtrain <- lgb.Dataset(data = X_train, label = y_train)

# -------------------------------------------------------
# LightGBM-Parameter definieren
# -------------------------------------------------------
params <- list(
  objective = "binary",
  metric = "auc",
  boosting = "gbdt",
  learning_rate = 0.05,
  num_leaves = 31,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 5
)

# -------------------------------------------------------
# Training LightGBM
# -------------------------------------------------------
set.seed(123)
model_lgb <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  valids = list(test = lgb.Dataset(data = X_test, label = y_test)),
  early_stopping_rounds = 10,
  verbose = 1
)

# -------------------------------------------------------
# Evaluation LightGBM
# -------------------------------------------------------
pred_prob <- predict(model_lgb, X_test)
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

roc_obj <- roc(y_test, pred_prob)
auc_val <- auc(roc_obj)

logloss <- -mean(y_test * log(pred_prob) + (1 - y_test) * log(1 - pred_prob))
brier <- mean((y_test - pred_prob)^2)

# Confusion Matrix mit caret
cm <- confusionMatrix(factor(pred_class, levels = c(0,1)), factor(y_test, levels = c(0,1)), positive = "1")

metrics_lgb <- data.frame(
  Accuracy = cm$overall["Accuracy"],
  Precision = cm$byClass["Precision"],
  Recall = cm$byClass["Recall"],
  F1_Score = cm$byClass["F1"],
  AUC = auc_val,
  Log_Loss = logloss,
  Brier_Score = brier
)

dir.create("Models/LGB_Tuned", recursive = TRUE, showWarnings = FALSE)
saveRDS(model_lgb, "Models/LGB_Tuned/model_lgb_tuned.rds")
write.csv(metrics_lgb, "Models/LGB_Tuned/metrics_lgb.csv", row.names = FALSE)
