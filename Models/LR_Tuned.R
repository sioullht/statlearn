
# -------------------------------------------------------
# Pakete
# -------------------------------------------------------
packages <- c("tidyverse", "caret", "pROC", "ggplot2", "MLmetrics")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

# -------------------------------------------------------
# Daten laden & Zielvariable
# -------------------------------------------------------
# Annahme: Die CSV-Dateien befinden sich im Unterordner "Step3"
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

# Labels für die Zielvariable anpassen
train_data$y <- factor(train_data$y, levels = c(0, 1), labels = c("Spieler2", "Spieler1"))
test_data$y  <- factor(test_data$y,  levels = c(0, 1), labels = c("Spieler2", "Spieler1"))

# ----------------------------------------------------------------
# Recursive Feature Elimination mit Kreuzvalidierung (RFECV)
# ----------------------------------------------------------------
# Definition der Steuerungsparameter für RFE
rfe_control <- rfeControl(
  functions = lrFuncs,      # Funktionen für logistische Regression
  method = "cv",             # Kreuzvalidierung
  number = 10,               # 10 Folds
  verbose = FALSE,           # Weniger Konsolenausgabe
  returnResamp = "final"
)

# Prädiktoren (alle Spalten außer 'y') und Zielvariable trennen
x_train <- train_data[, setdiff(names(train_data), "y")]
y_train <- train_data$y

# Durchführung der RFE
rfe_results <- rfe(
  x = x_train,
  y = y_train,
  sizes = c(2:(ncol(x_train))), # Zu testende Anzahl von Features
  rfeControl = rfe_control,
  metric = "ROC"               # Optimierungsmetrik
)

# Ausgabe der RFE-Ergebnisse
cat("--- Ergebnisse der Recursive Feature Elimination ---\n")
print(rfe_results)

# Extraktion der besten Prädiktoren
optimal_features <- predictors(rfe_results)
cat("\nOptimal gefundene Features (", length(optimal_features), "):\n", sep="")
print(optimal_features)

# -------------------------------------------------------
# RFECV-Leistungskurve
# -------------------------------------------------------
# KORREKTER & ROBUSTER PLOT-CODE
# Dieser Ansatz ist stabiler, da er direkt mit dem Ergebnis-DataFrame arbeitet
# und besser mit den durch Warnungen verursachten NA-Werten umgehen kann.

cat("\n--- Erstelle RFECV-Leistungskurve ---\n")

# Prüfen, ob Ergebnisse vorhanden und gültig sind
if (is.data.frame(rfe_results$results) && nrow(rfe_results$results) > 0 && "ROC" %in% names(rfe_results$results)) {
  
  p_rfe_performance <- ggplot(rfe_results$results, aes(x = Variables, y = ROC)) +
    geom_line(color = "gray50") +
    geom_point(size = 3, aes(color = ROC)) +
    # Punkt für das beste Ergebnis hervorheben
    geom_point(aes(x = optsize, y = max(ROC, na.rm = TRUE)), color = "red", size = 5, shape = 18) +
    scale_color_gradient(low = "lightblue", high = "darkblue") +
    theme_minimal(base_size = 12) +
    labs(
      title = "RFECV Leistungskurve",
      subtitle = paste("Optimum bei", rfe_results$optsize, "Features"),
      x = "Anzahl der Features",
      y = "ROC (AUC) via 10-facher Kreuzvalidierung",
      color = "AUC"
    ) +
    theme(legend.position = "bottom")
  
  print(p_rfe_performance)

} else {
  cat("Konnte die RFE-Leistungskurve nicht erstellen, da die Ergebnisse ungültig sind.\n")
  cat("Dies liegt wahrscheinlich an den Warnungen während der Modellanpassung.\n")
  # Erstellt einen leeren Plot, damit das Skript nicht abbricht
  p_rfe_performance <- ggplot() + theme_void() + ggtitle("RFE-Ergebnisse konnten nicht geplottet werden")
}

# -------------------------------------------------------
# Modelltraining mit optimierten Features & Bootstrapping
# -------------------------------------------------------
# Steuerungsparameter für das finale Modelltraining
ctrl_boot <- trainControl(
  method = "boot",
  number = 100, # Anzahl der Bootstrap-Wiederholungen
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Nur die von RFE ausgewählten Features für das Training verwenden
train_data_rfe <- train_data[, c("y", optimal_features)]

# Training des finalen Modells
model_lr_rfe <- train(
  y ~ ., data = train_data_rfe,
  method    = "glm",
  family    = "binomial",
  trControl = ctrl_boot,
  metric    = "ROC"
)

cat("\n--- Finales Modell (trainiert mit optimierten Features) ---\n")
print(model_lr_rfe)

# -------------------------------------------------------
# Predictions: TRAIN & TEST
# -------------------------------------------------------
# TRAIN
pred_train_prob  <- predict(model_lr_rfe, train_data, type = "prob")[, "Spieler1"]
pred_train_class <- predict(model_lr_rfe, train_data)

# TEST
pred_test_prob   <- predict(model_lr_rfe, test_data,  type = "prob")[, "Spieler1"]
pred_test_class  <- predict(model_lr_rfe, test_data)

# -------------------------------------------------------
# ROC / AUC
# -------------------------------------------------------
roc_train <- pROC::roc(train_data$y, pred_train_prob, levels = c("Spieler2", "Spieler1"), direction = "<")
auc_train <- as.numeric(pROC::auc(roc_train))

roc_test  <- pROC::roc(test_data$y,  pred_test_prob,  levels = c("Spieler2", "Spieler1"), direction = "<")
auc_test  <- as.numeric(pROC::auc(roc_test))

# -------------------------------------------------------
# Confusion Matrices (Cutoff 0.5)
# -------------------------------------------------------
cm_train <- caret::confusionMatrix(pred_train_class, train_data$y, positive = "Spieler1")
cm_test  <- caret::confusionMatrix(pred_test_class,  test_data$y,  positive = "Spieler1")

# -------------------------------------------------------
# Metriken (stabiler LogLoss & Brier)
# -------------------------------------------------------
eps <- 1e-15
# TRAIN
pp_tr  <- pmin(pmax(pred_train_prob, eps), 1 - eps)
y01_tr <- as.numeric(train_data$y == "Spieler1")
logloss_tr <- MLmetrics::LogLoss(y_pred = pp_tr, y_true = y01_tr)
brier_tr   <- mean((y01_tr - pred_train_prob)^2)

# TEST
pp_te  <- pmin(pmax(pred_test_prob, eps), 1 - eps)
y01_te <- as.numeric(test_data$y == "Spieler1")
logloss_te <- MLmetrics::LogLoss(y_pred = pp_te, y_true = y01_te)
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
cat("\n--- Finale Metriken ---\n")
print(round(metrics_train_test, 4))

# -------------------------------------------------------
# Speicherung & Plots
# -------------------------------------------------------
out_dir  <- "Models/LR_RFE_CV" # Neuer Ordner für die Ergebnisse
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# Speichern der neuen RFECV-Kurve
ggsave(filename = file.path(plot_dir, "RFE_Performance_Curve.pdf"), plot = p_rfe_performance, width = 8, height = 6)

# Speichern der übrigen Ausgaben
capture.output(rfe_results, file = file.path(out_dir, "rfe_results.txt"))
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
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train_vs_Test.pdf"), plot = p_both, width = 7, height = 5)

saveRDS(model_lr_rfe, file.path(out_dir, "model_lr_rfe.rds"))
saveRDS(rfe_results, file.path(out_dir, "rfe_object.rds"))

# ----------------------------------------------------------------
# Finale Test-Metriken separat speichern
# ----------------------------------------------------------------
final_metrics <- metrics_train_test %>%
  tibble::rownames_to_column("Split") %>%
  dplyr::filter(Split == "Test")

readr::write_csv(final_metrics, file.path(out_dir, "model_metrics.csv"))

message("Fertig. Alle Ergebnisse unter: ", normalizePath(out_dir))