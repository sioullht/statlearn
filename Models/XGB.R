# Pakete
packages <- c("tidyverse", "caret", "pROC", "xgboost", "scales", "Matrix")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
lapply(packages, library, character.only = TRUE)

set.seed(123)

# -------------------------------------------------------
# Daten laden & Zielvariable
# -------------------------------------------------------
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

train_data$y <- factor(train_data$y, levels = c(0,1), labels = c("neg","pos"))
test_data$y  <- factor(test_data$y,  levels = c(0,1), labels = c("neg","pos"))

# -------------------------------------------------------
# Basismodell XGBoost OHNE Cross-Validation
#  - caret: method = "none" -> kein Tuning, keine CV
#  - tuneGrid: genau EINE Zeile = feste Hyperparameter
# -------------------------------------------------------
ctrl_none <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# Einfache, konservative Defaults (kannst du später anpassen)
base_grid <- data.frame(
  nrounds = 200,
  max_depth = 6,
  eta = 0.1,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

model_xgb_basic <- train(
  y ~ ., data = train_data,
  method = "xgbTree",
  trControl = ctrl_none,
  tuneGrid = base_grid,
  metric = "ROC",
  verbose = FALSE
)

print(model_xgb_basic)

# -------------------------------------------------------
# Evaluation (Testset)
# -------------------------------------------------------
pred_class <- predict(model_xgb_basic, test_data)
pred_prob  <- predict(model_xgb_basic, test_data, type = "prob")[, "pos"]

# ROC & AUC
roc_obj <- pROC::roc(test_data$y, pred_prob, levels = c("neg","pos"), direction = "<")
auc_val <- pROC::auc(roc_obj)

# Confusion Matrix (Standard-Cutoff 0.5)
cm <- caret::confusionMatrix(pred_class, test_data$y, positive = "pos")

# Stabiler LogLoss (Clipping verhindert log(0))
eps <- 1e-15
pp  <- pmin(pmax(pred_prob, eps), 1 - eps)
y01 <- as.numeric(test_data$y == "pos")
logloss <- -mean(y01 * log(pp) + (1 - y01) * log(1 - pp))

# Brier Score
brier <- mean((y01 - pred_prob)^2)

metrics_basic <- data.frame(
  Accuracy    = cm$overall["Accuracy"],
  Precision   = cm$byClass["Precision"],
  Recall      = cm$byClass["Recall"],
  F1_Score    = cm$byClass["F1"],
  AUC         = as.numeric(auc_val),
  Log_Loss    = logloss,
  Brier_Score = brier
)

print(metrics_basic)

# -------------------------------------------------------
# Speichern
# -------------------------------------------------------
dir.create("Models/XGB_Basic", recursive = TRUE, showWarnings = FALSE)
saveRDS(model_xgb_basic, "Models/XGB/model_xgb.rds")
readr::write_csv(metrics_basic, "Models/XGB/metrics_xgb.csv")