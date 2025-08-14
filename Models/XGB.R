# -------------------------------------------------------
# Pakete
# -------------------------------------------------------
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
# -------------------------------------------------------
ctrl_none <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# Feste Hyperparameter (einfach, konservativ)
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
# Plots: ROC/AUC als PDF speichern
# -------------------------------------------------------
out_dir <- "Models/XGB"
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# Einzelplots (Train & Test)
p_train <- ggplot2::ggroc(roc_train) +
  ggplot2::ggtitle(sprintf("ROC (TRAIN) — AUC = %.3f", auc_train)) +
  ggplot2::theme_minimal()

p_test <- ggplot2::ggroc(roc_test) +
  ggplot2::ggtitle(sprintf("ROC (TEST) — AUC = %.3f", auc_test)) +
  ggplot2::theme_minimal()

ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train.pdf"), plot = p_train, width = 6, height = 5)
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Test.pdf"),  plot = p_test,  width = 6, height = 5)

# Vergleichsplot (beide Kurven in einem PDF)
roc_df_train <- data.frame(sens = roc_train$sensitivities, spec = roc_train$specificities, split = "Train")
roc_df_test  <- data.frame(sens = roc_test$sensitivities,  spec = roc_test$specificities,  split = "Test")
roc_both <- bind_rows(roc_df_train, roc_df_test) %>%
  mutate(fpr = 1 - spec)

p_both <- ggplot2::ggplot(roc_both, aes(x = fpr, y = sens, linetype = split)) +
  ggplot2::geom_line() +
  ggplot2::geom_abline(slope = 1, intercept = 0, linewidth = 0.3) +
  ggplot2::labs(title = sprintf("ROC — Train (AUC=%.3f) vs. Test (AUC=%.3f)", auc_train, auc_test),
                x = "False Positive Rate", y = "True Positive Rate", linetype = "Split") +
  ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train_vs_Test.pdf"), plot = p_both, width = 6, height = 5)

# -------------------------------------------------------
# Confusion Matrices speichern (als TXT) + Metriken als CSV
# -------------------------------------------------------
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Confusion Matrices als Text sichern
capture.output(cm_train, file = file.path(out_dir, "confusion_matrix_train.txt"))
capture.output(cm_test,  file = file.path(out_dir, "confusion_matrix_test.txt"))

# Metriken speichern (nebeneinander)
readr::write_csv(
  metrics_train_test %>% rownames_to_column(var = "Split"),
  file.path(out_dir, "metrics_train_vs_test.csv")
)

# Zusätzlich: Einzel-Metriken (nur Test, wie bisher)
readr::write_csv(
  data.frame(t(metrics_test)) %>% rownames_to_column(var = "Metric") %>% rename(Value = `data.frame.t.metrics_test...`),
  file.path(out_dir, "metrics_test_only.csv")
)

# -------------------------------------------------------
# Modell speichern (wie bisher)
# -------------------------------------------------------
saveRDS(model_xgb, file.path(out_dir, "model_xgb.rds"))

message("Fertig. Outputs liegen unter: ", normalizePath(out_dir))