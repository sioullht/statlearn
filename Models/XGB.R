# -------------------------------------------------------
# Pakete
# -------------------------------------------------------
packages <- c("tidyverse", "caret", "pROC", "xgboost", "scales", "Matrix", "ggplot2")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

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
# -------------------------------------------------------
ctrl_none <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# Feste Hyperparameter
base_grid <- data.frame(
  nrounds = 200,
  max_depth = 6,
  eta = 0.1,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

model_xgb <- train(
  y ~ ., data = train_data,
  method = "xgbTree",
  trControl = ctrl_none,
  tuneGrid = base_grid,
  metric = "ROC",
  verbose = FALSE
)

print(model_xgb)

# -------------------------------------------------------
# Predictions: TRAIN & TEST
# -------------------------------------------------------
# TRAIN
pred_train_prob  <- predict(model_xgb, train_data, type = "prob")[, "pos"]
pred_train_class <- predict(model_xgb, train_data)

# TEST
pred_test_prob   <- predict(model_xgb, test_data,  type = "prob")[, "pos"]
pred_test_class  <- predict(model_xgb, test_data)

# -------------------------------------------------------
# ROC / AUC
# -------------------------------------------------------
roc_train <- pROC::roc(train_data$y, pred_train_prob, levels = c("neg","pos"), direction = "<")
auc_train <- as.numeric(pROC::auc(roc_train))

roc_test  <- pROC::roc(test_data$y,  pred_test_prob,  levels = c("neg","pos"), direction = "<")
auc_test  <- as.numeric(pROC::auc(roc_test))

# -------------------------------------------------------
# Confusion Matrices (Cutoff 0.5)
# -------------------------------------------------------
cm_train <- caret::confusionMatrix(pred_train_class, train_data$y, positive = "pos")
cm_test  <- caret::confusionMatrix(pred_test_class,  test_data$y,  positive = "pos")

# -------------------------------------------------------
# Metriken (mit stabilem LogLoss & Brier)
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
# Verzeichnisse & Speichern
# -------------------------------------------------------
out_dir  <- "Models/XGB"
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# Confusion Matrices als TXT sichern
capture.output(cm_train, file = file.path(out_dir, "confusion_matrix_train.txt"))
capture.output(cm_test,  file = file.path(out_dir,  "confusion_matrix_test.txt"))

# Metriken speichern (Train vs. Test)
metrics_train_test %>%
  tibble::rownames_to_column("Split") %>%
  readr::write_csv(file.path(out_dir, "metrics_train_vs_test.csv"))

# Zusätzlich: nur Testmetriken (wie bisher)
data.frame(Metric = names(metrics_test), Value = as.numeric(metrics_test)) %>%
  readr::write_csv(file.path(out_dir, "metrics_test_only.csv"))

# -------------------------------------------------------
# Plots: ROC/AUC als PDF speichern
# -------------------------------------------------------
# Einzelplots
p_train <- pROC::ggroc(roc_train) +
  ggplot2::ggtitle(sprintf("ROC (TRAIN) — AUC = %.3f", auc_train)) +
  ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train.pdf"), plot = p_train, width = 6, height = 5)

p_test <- pROC::ggroc(roc_test) +
  ggplot2::ggtitle(sprintf("ROC (TEST) — AUC = %.3f", auc_test)) +
  ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Test.pdf"), plot = p_test, width = 6, height = 5)

# Vergleichsplot (beide Kurven)
p_both <- pROC::ggroc(list(Train = roc_train, Test = roc_test)) +
  ggplot2::ggtitle(sprintf("ROC — Train (AUC = %.3f) vs. Test (AUC = %.3f)", auc_train, auc_test)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(linetype = "Split", color = "Split")
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train_vs_Test.pdf"), plot = p_both, width = 6, height = 5)

# -------------------------------------------------------
# Modell speichern
# -------------------------------------------------------
saveRDS(model_xgb, file.path(out_dir, "model_xgb.rds"))

message("Fertig. Outputs liegen unter: ", normalizePath(out_dir))