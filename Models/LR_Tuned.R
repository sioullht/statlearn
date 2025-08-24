# =======================================================
# Logistic Regression Study (Variante B):
# RFE (einmalig) + Cross-Validation vs. Bootstrapping
# =======================================================

# -------------------------------------------------------
# Pakete
# -------------------------------------------------------
packages <- c("tidyverse","caret","pROC","ggplot2","MLmetrics","ggrepel")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

# -------------------------------------------------------
# Daten laden & Zielvariable
# -------------------------------------------------------
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

# Labels als Faktor, positive Klasse = "Spieler1"
train_data$y <- factor(train_data$y, levels = c(0,1), labels = c("Spieler2","Spieler1"))
test_data$y  <- factor(test_data$y,  levels = c(0,1), labels = c("Spieler2","Spieler1"))

# Für ROC-Bewertung später sinnvoll: positive Klasse als erste Stufe
train_data$y <- stats::relevel(train_data$y, ref = "Spieler1")
test_data$y  <- stats::relevel(test_data$y,  ref = "Spieler1")

# -------------------------------------------------------
# Basis-Ausgabeverzeichnis
# -------------------------------------------------------
base_dir <- "Models/LR_Tuned_Final"
dirs <- file.path(base_dir, c(
  "RFE",
  "RFE_CrossValidation",
  "RFE_Bootstrapping",
  "Comparison"
))
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------
# Hilfsfunktionen
# -------------------------------------------------------

# (1) Einheits-Metriken aus Wahrscheinlichkeiten & Klassen
compute_metrics <- function(y_true_factor, prob_pos, class_pred, positive = "Spieler1") {
  eps <- 1e-15
  y_true <- as.numeric(y_true_factor == positive)
  pp     <- pmin(pmax(prob_pos, eps), 1 - eps)

  auc_val <- as.numeric(pROC::auc(y_true_factor, prob_pos,
                                  levels = c("Spieler2","Spieler1"), direction = "<"))
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

# (2) Train/Test-Auswertung + Speichern (nur gewünschte Dateien)
evaluate_and_save <- function(model, train_df, test_df, out_dir) {
  # Predictions
  prob_tr  <- predict(model, train_df, type = "prob")[,"Spieler1"]
  class_tr <- predict(model, train_df)
  prob_te  <- predict(model, test_df,  type = "prob")[,"Spieler1"]
  class_te <- predict(model, test_df)

  # Metriken
  m_tr <- compute_metrics(train_df$y, prob_tr, class_tr)
  m_te <- compute_metrics(test_df$y,  prob_te, class_te)
  metrics_train_test <- dplyr::bind_rows(Train = m_tr, Test = m_te, .id = "Split")

  # ROC (für Plots)
  roc_tr <- pROC::roc(train_df$y, prob_tr, levels = c("Spieler2","Spieler1"), direction = "<")
  roc_te <- pROC::roc(test_df$y,  prob_te, levels = c("Spieler2","Spieler1"), direction = "<")
  auc_tr <- as.numeric(pROC::auc(roc_tr))
  auc_te <- as.numeric(pROC::auc(roc_te))

  # Konsole
  message("\n--- Metriken (", basename(out_dir), ") ---")
  print(round(dplyr::select(metrics_train_test, -Split), 4))

  # Speichern
  plot_dir <- file.path(out_dir, "plots")
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  # Confusion Matrices
  cm_train <- caret::confusionMatrix(class_tr, train_df$y, positive = "Spieler1")
  cm_test  <- caret::confusionMatrix(class_te,  test_df$y,  positive = "Spieler1")
  capture.output(cm_train, file = file.path(out_dir, "confusion_matrix_train.txt"))
  capture.output(cm_test,  file = file.path(out_dir, "confusion_matrix_test.txt"))

  # Train/Test Metriken
  readr::write_csv(metrics_train_test, file.path(out_dir, "metrics_train_vs_test.csv"))

  # Plots: ROC_Test + ROC_Train_vs_Test
  p_te <- pROC::ggroc(roc_te) +
    ggplot2::ggtitle(sprintf("ROC (TEST) — AUC = %.3f", auc_te)) +
    ggplot2::theme_minimal()
  ggplot2::ggsave(file.path(plot_dir, "ROC_Test.pdf"), p_te, width = 6, height = 5)

  p_bt <- pROC::ggroc(list(Train = roc_tr, Test = roc_te)) +
    ggplot2::ggtitle(sprintf("ROC — Train (AUC=%.3f) vs. Test (AUC=%.3f)", auc_tr, auc_te)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(linetype = "Split", color = "Split")
  ggplot2::ggsave(file.path(plot_dir, "ROC_Train_vs_Test.pdf"), p_bt, width = 7, height = 5)

  invisible(metrics_train_test %>% dplyr::filter(Split == "Test"))
}

# (3) Resampling-Auswertung → Summary (für Comparison)
summarize_resampling <- function(model, positive = "Spieler1") {
  stopifnot("pred" %in% names(model))
  preds <- model$pred
  if (!is.null(model$bestTune)) {
    for (nm in names(model$bestTune)) {
      preds <- preds %>% dplyr::filter(.data[[nm]] == model$bestTune[[nm]])
    }
  }
  if (!"Spieler1" %in% names(preds)) {
    stop("In model$pred fehlt die Spalte 'Spieler1' (Wahrscheinlichkeit der positiven Klasse).")
  }

  res_metrics <- preds %>%
    dplyr::group_by(Resample) %>%
    dplyr::summarise(
      dplyr::as_tibble(compute_metrics(
        y_true_factor = obs,
        prob_pos      = .data[["Spieler1"]],
        class_pred    = pred,
        positive      = positive
      ))
    )

  agg <- res_metrics %>%
    dplyr::select(-Resample) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Metric", values_to = "Value") %>%
    dplyr::group_by(Metric) %>%
    dplyr::summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD   = sd(Value, na.rm = TRUE),
      N    = sum(!is.na(Value)),
      SE   = SD / sqrt(pmax(N, 1)),
      CI_L = Mean - 1.96 * SE,
      CI_U = Mean + 1.96 * SE,
      .groups = "drop"
    )
  list(summary = agg)
}

# -------------------------------------------------------
# 1) RFE EINMALIG auf TRAIN (auf Accuracy optimiert)
#    + rfe_results.txt (für nachträgliche Kurven-Erstellung)
# -------------------------------------------------------
rfe_ctrl <- rfeControl(
  functions    = lrFuncs,  # Logit-spezifische RFE-Funktionen
  method       = "cv",
  number       = 10,
  verbose      = FALSE,
  returnResamp = "final"
)

x_train <- train_data[, setdiff(names(train_data), "y")]
y_train <- train_data$y
sizes_vec <- 2:ncol(x_train)

# RFE bewusst auf Accuracy (wie in deinen Anhängen) – ohne twoClassSummary
rfe_results <- rfe(
  x = x_train,
  y = y_train,
  sizes = sizes_vec,
  rfeControl = rfe_ctrl,
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 10)
)

opt_size      <- rfe_results$optsize
optimal_feats <- predictors(rfe_results)

cat("\n--- RFE Ergebnis ---\n")
cat("Optimale Anzahl Features:", opt_size, "\n")
print(optimal_feats)

# (a) rfe_results.txt – vollständige caret-Ausgabe
capture.output(rfe_results, file = file.path(base_dir, "RFE", "rfe_results.txt"))

# (b) optimale Features als Liste
readr::write_lines(optimal_feats, file = file.path(base_dir, "RFE", "optimal_features.txt"))

# (c) OPTIONAL: RFE-Performance-Kurve direkt miterzeugen (Accuracy) – kann bleiben
#     (du kannst sie auch ignorieren und später über rfe_results.txt neu bauen)
df_rfe <- as.data.frame(rfe_results$results) %>% dplyr::arrange(Variables)
acc_max <- max(df_rfe$Accuracy, na.rm = TRUE)
p_rfe <- ggplot(df_rfe, aes(x = Variables, y = Accuracy)) +
  geom_line(color = "skyblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2.5) +
  geom_vline(xintercept = opt_size, linetype = "dashed") +
  ggrepel::geom_text_repel(
    data = tibble::tibble(Variables = opt_size, Accuracy = acc_max,
                          lab = paste0("Optimum: ", sprintf("%.4f", acc_max))),
    aes(label = lab),
    color = "red",
    nudge_y = 0.001,
    min.segment.length = 0,
    seed = 123
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "RFE Leistungskurve",
    subtitle = paste0("Die Modellgenauigkeit erreicht bei ", opt_size, " Variablen ihr Maximum"),
    x = "Anzahl der ausgewählten Variablen",
    y = "Modell-Genauigkeit (Accuracy)"
  )
ggplot2::ggsave(filename = file.path(base_dir, "RFE", "RFE_Performance_Curve.pdf"),
                plot = p_rfe, width = 10, height = 6)

# -------------------------------------------------------
# 2) Modelle mit RFE-Features: CV (ROC) & Boot (ROC)
# -------------------------------------------------------
# Für Modellbewertung nutzen wir wieder ROC (twoClassSummary)
train_rfe <- train_data[, c("y", optimal_feats)]
test_rfe  <- test_data[,  c("y", optimal_feats)]

# 2a) Cross-Validation (10-fold, ROC)
ctrl_cv <- caret::trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)
model_rfe_cv <- caret::train(
  y ~ ., data = train_rfe,
  method = "glm",
  family = "binomial",
  trControl = ctrl_cv,
  metric = "ROC"
)
cv_stats <- summarize_resampling(model_rfe_cv)
final_test_rfe_cv <- evaluate_and_save(
  model_rfe_cv, train_rfe, test_rfe,
  out_dir = file.path(base_dir, "RFE_CrossValidation")
)

# 2b) Bootstrapping (B = 100, ROC)
ctrl_boot <- caret::trainControl(
  method = "boot",
  number = 100,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)
model_rfe_boot <- caret::train(
  y ~ ., data = train_rfe,
  method = "glm",
  family = "binomial",
  trControl = ctrl_boot,
  metric = "ROC"
)
boot_stats <- summarize_resampling(model_rfe_boot)
final_test_rfe_boot <- evaluate_and_save(
  model_rfe_boot, train_rfe, test_rfe,
  out_dir = file.path(base_dir, "RFE_Bootstrapping")
)

# -------------------------------------------------------
# 3) Vergleichstabellen (nur im Comparison-Ordner speichern)
# -------------------------------------------------------
# 3a) CV vs. Boot (Resampling-Schätzungen)
resampling_comp <- dplyr::full_join(
  cv_stats$summary  %>% dplyr::rename(Mean_CV   = Mean, SD_CV   = SD, N_CV   = N, SE_CV   = SE,
                                      CI_L_CV   = CI_L, CI_U_CV = CI_U),
  boot_stats$summary %>% dplyr::rename(Mean_BOOT = Mean, SD_BOOT = SD, N_BOOT = N, SE_BOOT = SE,
                                       CI_L_BOOT = CI_L, CI_U_BOOT = CI_U),
  by = "Metric"
) %>% dplyr::mutate(across(where(is.numeric), ~ round(., 4)))

readr::write_csv(resampling_comp, file.path(base_dir, "Comparison", "RFE_resampling_cv_vs_boot.csv"))

# 3b) Test: RFE_CV vs. RFE_Boot (ohne Baseline, da separat)
tests_comp <- dplyr::bind_rows(
  RFE_CrossValidation = final_test_rfe_cv %>% dplyr::select(-Split),
  RFE_Bootstrapping   = final_test_rfe_boot %>% dplyr::select(-Split),
  .id = "Model"
) %>% dplyr::mutate(across(where(is.numeric), ~ round(., 4)))

readr::write_csv(tests_comp, file.path(base_dir, "Comparison", "Test_RFE_CV_vs_RFE_Boot.csv"))

message("\nFertig. Alle Ergebnisse unter: ", normalizePath(base_dir))

