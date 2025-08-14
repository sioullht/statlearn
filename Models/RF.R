# -------------------------------------------------------
# Pakete
# -------------------------------------------------------
packages <- c("tidyverse","caret","pROC","randomForest","ggplot2")
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
# Basismodell Random Forest OHNE Cross-Validation
#  - caret: method = "none" -> kein Tuning, keine CV
#  - tuneGrid: genau EINE Zeile = feste Hyperparameter
# -------------------------------------------------------
p <- ncol(train_data) - 1L                    # Anzahl Features (ohne Zielvariable)
mtry_default <- max(1L, floor(sqrt(p)))       # übliche RF-Default-Heuristik

ctrl_none <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

rf_grid <- data.frame(mtry = mtry_default)

model_rf <- train(
  y ~ ., data = train_data,
  method = "rf",
  trControl = ctrl_none,
  tuneGrid  = rf_grid,
  metric    = "ROC",
  ntree     = 500,        # solide Basis
  importance = TRUE
)

print(model_rf)

# -------------------------------------------------------
# Predictions: TRAIN & TEST
# -------------------------------------------------------
# TRAIN
pred_train_prob  <- predict(model_rf, train_data, type = "prob")[, "pos"]
pred_train_class <- predict(model_rf, train_data)

# TEST
pred_test_prob   <- predict(model_rf, test_data,  type = "prob")[, "pos"]
pred_test_class  <- predict(model_rf, test_data)

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
# Metriken (stabiler LogLoss & Brier)
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
out_dir  <- "Models/RF"
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# Confusion Matrices als TXT
capture.output(cm_train, file = file.path(out_dir, "confusion_matrix_train.txt"))
capture.output(cm_test,  file = file.path(out_dir, "confusion_matrix_test.txt"))

# Metriken speichern
metrics_train_test %>%
  tibble::rownames_to_column("Split") %>%
  readr::write_csv(file.path(out_dir, "metrics_train_vs_test.csv"))

# -------------------------------------------------------
# ROC-Plots als PDF (Train, Test, Vergleich)
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
saveRDS(model_rf, file.path(out_dir, "model_rf.rds"))

message("Fertig. Alle Ergebnisse unter: ", normalizePath(out_dir))