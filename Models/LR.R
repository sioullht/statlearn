# -------------------------------------------------------
# Pakete
# -------------------------------------------------------
packages <- c("tidyverse","caret","pROC","ggplot2")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

# -------------------------------------------------------
# Daten laden & Zielvariable
# -------------------------------------------------------
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

# --- Änderung 1: Labels anpassen ---
train_data$y <- factor(train_data$y, levels = c(0,1), labels = c("Spieler2", "Spieler1"))
test_data$y  <- factor(test_data$y,  levels = c(0,1), labels = c("Spieler2", "Spieler1"))

# -------------------------------------------------------
# Modell: Logistische Regression
# -------------------------------------------------------
ctrl_none <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

model_lr <- train(
  y ~ ., data = train_data,
  method    = "glm",
  family    = "binomial",
  trControl = ctrl_none,
  metric    = "ROC"
)

print(model_lr)

# -------------------------------------------------------
# Predictions: TRAIN & TEST
# -------------------------------------------------------
# --- Änderung 2: Wahrscheinlichkeiten für "Spieler1" extrahieren ---
# TRAIN
pred_train_prob  <- predict(model_lr, train_data, type = "prob")[, "Spieler1"]
pred_train_class <- predict(model_lr, train_data)

# TEST
pred_test_prob   <- predict(model_lr, test_data,  type = "prob")[, "Spieler1"]
pred_test_class  <- predict(model_lr, test_data)

# -------------------------------------------------------
# ROC / AUC
# -------------------------------------------------------
# --- Änderung 3: Levels für ROC-Analyse anpassen ---
roc_train <- pROC::roc(train_data$y, pred_train_prob, levels = c("Spieler2", "Spieler1"), direction = "<")
auc_train <- as.numeric(pROC::auc(roc_train))

roc_test  <- pROC::roc(test_data$y,  pred_test_prob,  levels = c("Spieler2", "Spieler1"), direction = "<")
auc_test  <- as.numeric(pROC::auc(roc_test))

# -------------------------------------------------------
# Confusion Matrices (Cutoff 0.5)
# -------------------------------------------------------
# --- Änderung 4: Positive Klasse für Konfusionsmatrix definieren ---
cm_train <- caret::confusionMatrix(pred_train_class, train_data$y, positive = "Spieler1")
cm_test  <- caret::confusionMatrix(pred_test_class,  test_data$y,  positive = "Spieler1")

# -------------------------------------------------------
# Metriken (stabiler LogLoss & Brier)
# -------------------------------------------------------
eps <- 1e-15

# --- Änderung 5: Numerische Zielvariable für Metriken korrekt erstellen ---
# TRAIN
pp_tr  <- pmin(pmax(pred_train_prob, eps), 1 - eps)
y01_tr <- as.numeric(train_data$y == "Spieler1")
logloss_tr <- -mean(y01_tr * log(pp_tr) + (1 - y01_tr) * log(1 - pp_tr))
brier_tr   <- mean((y01_tr - pred_train_prob)^2)

# TEST
pp_te  <- pmin(pmax(pred_test_prob, eps), 1 - eps)
y01_te <- as.numeric(test_data$y == "Spieler1")
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
# Speicherung & Plots (Keine Änderungen an der Logik hier)
# -------------------------------------------------------
out_dir  <- "Models/LR" # Optional: anderer Ordnername
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

capture.output(cm_train, file = file.path(out_dir, "confusion_matrix_train.txt"))
capture.output(cm_test,  file = file.path(out_dir, "confusion_matrix_test.txt"))

metrics_train_test %>%
  tibble::rownames_to_column("Split") %>%
  readr::write_csv(file.path(out_dir, "metrics_train_vs_test.csv"))

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

saveRDS(model_lr, file.path(out_dir, "model_lr.rds"))

# ----------------------------------------------------------------
# NEUER ABSCHNITT: Finale Test-Metriken separat speichern
# ----------------------------------------------------------------
final_metrics <- metrics_train_test %>%
  tibble::rownames_to_column("Split") %>%
  dplyr::filter(Split == "Test") # Wählt nur die "Test"-Zeile aus

readr::write_csv(final_metrics, file.path(out_dir, "model_metrics.csv"))

message("Fertig. Alle Ergebnisse unter: ", normalizePath(out_dir))