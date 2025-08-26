# -------------------------------------------------------
# Pakete installieren und laden
# -------------------------------------------------------
# Stellt sicher, dass alle benötigten Pakete vorhanden und geladen sind.
packages <- c("tidyverse", "caret", "pROC", "ggplot2", "MLmetrics")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))

# Seed für die Reproduzierbarkeit der Ergebnisse setzen
set.seed(123)


### NEU: HELPER-FUNKTIONEN ###
# -------------------------------------------------------
# Diese Funktionen helfen uns, die Ergebnisse der zweiten CV auszuwerten.

# (1) Berechnet einen Satz von Metriken aus Vorhersagen
compute_metrics <- function(y_true_factor, prob_pos, class_pred, positive = "Spieler1") {
  eps <- 1e-15
  y_true <- as.numeric(y_true_factor == positive)
  pp     <- pmin(pmax(prob_pos, eps), 1 - eps)

  auc_val <- as.numeric(pROC::auc(y_true_factor, prob_pos,
                                  levels = c("Spieler2", "Spieler1"), direction = "<"))
  cm <- caret::confusionMatrix(class_pred, y_true_factor, positive = positive)

  tibble::tibble(
    Accuracy    = unname(cm$overall["Accuracy"]),
    Precision   = unname(cm$byClass["Precision"]),
    Recall      = unname(cm$byClass["Recall"]),
    F1_Score    = unname(cm$byClass["F1"]),
    AUC         = auc_val,
    Log_Loss    = MLmetrics::LogLoss(y_pred = pp, y_true = y_true),
    Brier_Score = mean((y_true - prob_pos)^2)
  )
}

# (2) Fasst die Metriken aus den CV-Folds zusammen (Mittelwert, SD, etc.)
summarize_resampling <- function(model, positive = "Spieler1") {
  # Extrahieren der Vorhersagen für jeden Fold
  preds <- model$pred

  # Berechne Metriken für jeden einzelnen Fold
  resampled_metrics <- preds %>%
    dplyr::group_by(Resample) %>%
    dplyr::summarise(
      dplyr::as_tibble(compute_metrics(
        y_true_factor = obs,
        prob_pos      = .data[[positive]],
        class_pred    = pred,
        positive      = positive
      ))
    )

  # Aggregiere die Metriken über alle Folds
  aggregated_summary <- resampled_metrics %>%
    dplyr::select(-Resample) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Metric", values_to = "Value") %>%
    dplyr::group_by(Metric) %>%
    dplyr::summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD   = sd(Value, na.rm = TRUE),
      .groups = "drop"
    )
  return(aggregated_summary)
}
# -------------------------------------------------------


# -------------------------------------------------------
# Daten laden & Zielvariable vorbereiten
# -------------------------------------------------------
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

train_data$y <- factor(train_data$y, levels = c(0, 1), labels = c("Spieler2", "Spieler1"))
test_data$y  <- factor(test_data$y,  levels = c(0, 1), labels = c("Spieler2", "Spieler1"))

# -------------------------------------------------------
# PHASE 1: Modell-Tuning mit RFE (Kreuzvalidierung zur Feature-Selektion)
# -------------------------------------------------------
message("PHASE 1: Starte RFE-Prozess zur Feature-Selektion...")

rfe_control <- rfeControl(
  functions = lrFuncs,
  method = "cv",
  number = 10,
  verbose = FALSE # Auf FALSE gesetzt für eine sauberere Ausgabe
)

rfe_profile <- rfe(
  y ~ .,
  data = train_data,
  sizes = 2:(ncol(train_data) - 1),
  rfeControl = rfe_control,
  metric = "ROC"
)

best_features <- predictors(rfe_profile)
print(rfe_profile)
message(paste("\nOptimale Anzahl an Features gefunden:", length(best_features)))
print(best_features)


### NEU: PHASE 2 ###
# -------------------------------------------------------
# PHASE 2: Stabilitätsanalyse mit zweiter Kreuzvalidierung
# -------------------------------------------------------
message("\nPHASE 2: Starte zweite CV zur Stabilitätsanalyse des finalen Modells...")

# Daten nur mit den besten Features vorbereiten
train_data_rfe <- train_data[, c("y", best_features)]

# Steuerung für die zweite CV definieren
ctrl_cv_eval <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary, # Wichtig für AUC
  savePredictions = "final" # Entscheidend, um Fold-Ergebnisse zu speichern
)

# Modell mit den RFE-Features in einer neuen 10-fachen CV trainieren
model_final_cv <- train(
  y ~ ., data = train_data_rfe,
  method = "glm",
  family = "binomial",
  trControl = ctrl_cv_eval,
  metric = "ROC"
)

# Ergebnisse der zweiten CV auswerten (Mittelwert und Streuung der Metriken)
cv_stability_stats <- summarize_resampling(model_final_cv, positive = "Spieler1")

message("\n--- Ergebnisse der Stabilitätsanalyse (aus 10-facher CV) ---")
print(round(cv_stability_stats, 4))
# -------------------------------------------------------


### ANGEPASST: FINALE MODELL-EVALUATION ###
# -------------------------------------------------------
# Wir verwenden jetzt das Modell aus der zweiten CV (`model_final_cv`) für die finale Bewertung.
message("\nFINALE EVALUATION: Bewerte das finale Modell auf dem ungesehenen Test-Set...")

# Predictions
pred_train_prob  <- predict(model_final_cv, train_data_rfe, type = "prob")[, "Spieler1"]
pred_test_prob   <- predict(model_final_cv, test_data,      type = "prob")[, "Spieler1"]
pred_train_class <- predict(model_final_cv, train_data_rfe)
pred_test_class  <- predict(model_final_cv, test_data)

# Metriken für Train- und Test-Set berechnen
metrics_train <- compute_metrics(train_data$y, pred_train_prob, pred_train_class)
metrics_test  <- compute_metrics(test_data$y, pred_test_prob, pred_test_class)
metrics_summary <- dplyr::bind_rows(Train = metrics_train, Test = metrics_test, .id = "Split")

message("\n--- Finale Metriken (Train vs. Test) ---")
print(round(metrics_summary, 4))
# -------------------------------------------------------


### ANGEPASST: ERGEBNISSE SPEICHERN ###
# -------------------------------------------------------
out_dir  <- "Models/LR_Tuned_Final"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Finale Train/Test-Metriken speichern
readr::write_csv(metrics_summary, file.path(out_dir, "final_model_metrics.csv"))

# NEU: Ergebnisse der CV-Stabilitätsanalyse speichern
readr::write_csv(cv_stability_stats, file.path(out_dir, "cv_stability_metrics.csv"))

# Finales Modell als RDS-Datei speichern
saveRDS(model_final_cv, file.path(out_dir, "model_final_cv.rds"))

# RFE-Performance-Kurve als PDF und JPEG speichern
pdf(file.path(out_dir, "RFE_Performance_Curve.pdf"), width = 8, height = 6)
plot(rfe_profile, type = c("g", "o"), main = "RFE Performance-Profil (Phase 1)")
dev.off()

jpeg(file.path(out_dir, "RFE_Performance_Curve.jpeg"), width = 800, height = 600)
plot(rfe_profile, type = c("g", "o"), main = "RFE Performance-Profil (Phase 1)")
dev.off()

# ROC-Kurven als PDF speichern
roc_train <- pROC::roc(train_data$y, pred_train_prob, levels = c("Spieler2", "Spieler1"))
roc_test  <- pROC::roc(test_data$y,  pred_test_prob,  levels = c("Spieler2", "Spieler1"))
roc_plot <- pROC::ggroc(list(Train = roc_train, Test = roc_test)) +
  ggplot2::ggtitle(sprintf("Finale ROC-Kurven — Train (AUC=%.3f) vs. Test (AUC=%.3f)", roc_train$auc, roc_test$auc)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(color = "Datensatz")
ggplot2::ggsave(filename = file.path(out_dir, "Final_ROC_Curves.pdf"), plot = roc_plot, width = 7, height = 6)

message("\nSkript erfolgreich ausgeführt. Alle Ergebnisse unter: ", normalizePath(out_dir))