# -------------------------------------------------------
# Pakete
# -------------------------------------------------------
packages <- c("tidyverse","caret","pROC","ggplot2","MLmetrics")
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

# -------------------------------------------------------
# Basis-Ausgabeverzeichnis
# -------------------------------------------------------
base_dir <- "Models/LR_Comparison"
dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------
# Hilfsfunktion: komplette Auswertung + Speicherung
#  - fix: alle Einzelmetriken mit unname() ent-namen
# -------------------------------------------------------
evaluate_and_save <- function(model, train_df, test_df, sub_dir) {

  # Predictions (Prob + Klassen)
  pred_train_prob  <- predict(model, train_df, type = "prob")[, "Spieler1"]
  pred_train_class <- predict(model, train_df)
  pred_test_prob   <- predict(model, test_df,  type = "prob")[, "Spieler1"]
  pred_test_class  <- predict(model, test_df)

  # ROC / AUC
  roc_train <- pROC::roc(train_df$y, pred_train_prob, levels = c("Spieler2","Spieler1"), direction = "<")
  auc_train <- as.numeric(pROC::auc(roc_train))
  roc_test  <- pROC::roc(test_df$y,  pred_test_prob,  levels = c("Spieler2","Spieler1"), direction = "<")
  auc_test  <- as.numeric(pROC::auc(roc_test))

  # Confusion Matrices (Cutoff 0.5)
  cm_train <- caret::confusionMatrix(pred_train_class, train_df$y, positive = "Spieler1")
  cm_test  <- caret::confusionMatrix(pred_test_class,  test_df$y,  positive = "Spieler1")

  # Metriken (LogLoss & Brier)
  eps <- 1e-15
  pp_tr  <- pmin(pmax(pred_train_prob, eps), 1 - eps)
  y01_tr <- as.numeric(train_df$y == "Spieler1")
  logloss_tr <- MLmetrics::LogLoss(y_pred = pp_tr, y_true = y01_tr)
  brier_tr   <- mean((y01_tr - pred_train_prob)^2)

  pp_te  <- pmin(pmax(pred_test_prob, eps), 1 - eps)
  y01_te <- as.numeric(test_df$y == "Spieler1")
  logloss_te <- MLmetrics::LogLoss(y_pred = pp_te, y_true = y01_te)
  brier_te   <- mean((y01_te - pred_test_prob)^2)

  # --- FIX: Alle Einzelwerte mit unname() (keine „doppelten“ Namen)
  metrics_train <- c(
    Accuracy    = unname(cm_train$overall["Accuracy"]),
    Precision   = unname(cm_train$byClass["Precision"]),
    Recall      = unname(cm_train$byClass["Recall"]),
    F1_Score    = unname(cm_train$byClass["F1"]),
    AUC         = unname(auc_train),
    Log_Loss    = unname(logloss_tr),
    Brier_Score = unname(brier_tr)
  )

  metrics_test <- c(
    Accuracy    = unname(cm_test$overall["Accuracy"]),
    Precision   = unname(cm_test$byClass["Precision"]),
    Recall      = unname(cm_test$byClass["Recall"]),
    F1_Score    = unname(cm_test$byClass["F1"]),
    AUC         = unname(auc_test),
    Log_Loss    = unname(logloss_te),
    Brier_Score = unname(brier_te)
  )

  metrics_train_test <- rbind(Train = metrics_train, Test = metrics_test) %>% as.data.frame()
  cat("\n--- Metriken (", sub_dir, ") ---\n", sep = "")
  print(round(metrics_train_test, 4))

  # Speicherung & Plots
  out_dir  <- file.path(base_dir, sub_dir)
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
  ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train_vs_Test.pdf"), plot = p_both, width = 7, height = 5)

  saveRDS(model, file.path(out_dir, "model_lr.rds"))

  # Finale Test-Metriken separat speichern
  final_metrics <- metrics_train_test %>%
    tibble::rownames_to_column("Split") %>%
    dplyr::filter(Split == "Test")
  readr::write_csv(final_metrics, file.path(out_dir, "model_metrics.csv"))

  message("Fertig (", sub_dir, "). Ergebnisse unter: ", normalizePath(out_dir))

  # Rückgabe: nur Test-Metriken (für Vergleichstabelle)
  tibble::tibble(
    Procedure   = sub_dir,
    Accuracy    = as.numeric(metrics_test[["Accuracy"]]),
    Precision   = as.numeric(metrics_test[["Precision"]]),
    Recall      = as.numeric(metrics_test[["Recall"]]),
    F1_Score    = as.numeric(metrics_test[["F1_Score"]]),
    AUC         = as.numeric(metrics_test[["AUC"]]),
    Log_Loss    = as.numeric(metrics_test[["Log_Loss"]]),
    Brier_Score = as.numeric(metrics_test[["Brier_Score"]])
  )
}

# -------------------------------------------------------
# Modell 1: Cross-Validation (10-fold)
# -------------------------------------------------------
ctrl_cv <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

model_lr_cv <- train(
  y ~ ., data = train_data,
  method    = "glm",
  family    = "binomial",
  trControl = ctrl_cv,
  metric    = "ROC"
)

cat("\n--- Modell (CV) ---\n")
print(model_lr_cv)

cv_test_row <- evaluate_and_save(
  model    = model_lr_cv,
  train_df = train_data,
  test_df  = test_data,
  sub_dir  = "CrossValidation"
)

# -------------------------------------------------------
# Modell 2: Bootstrapping (100 Wiederholungen)
# -------------------------------------------------------
ctrl_boot <- trainControl(
  method = "boot",
  number = 100,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

model_lr_boot <- train(
  y ~ ., data = train_data,
  method    = "glm",
  family    = "binomial",
  trControl = ctrl_boot,
  metric    = "ROC"
)

cat("\n--- Modell (Bootstrapping) ---\n")
print(model_lr_boot)

boot_test_row <- evaluate_and_save(
  model    = model_lr_boot,
  train_df = train_data,
  test_df  = test_data,
  sub_dir  = "Bootstrapping"
)

# -------------------------------------------------------
# Vergleichstabelle: CV vs. Bootstrapping (Test-Split)
#   -> nur numerische Spalten runden
# -------------------------------------------------------
comparison_tbl <- dplyr::bind_rows(cv_test_row, boot_test_row) %>%
  dplyr::mutate(Procedure = dplyr::recode(Procedure,
                                          "CrossValidation" = "Cross-Validation",
                                          "Bootstrapping"   = "Bootstrapping"))

cat("\n--- Vergleich (Test) — Cross-Validation vs. Bootstrapping ---\n")
comparison_tbl_rounded <- comparison_tbl %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 4)))
print(comparison_tbl_rounded)

comp_dir <- file.path(base_dir, "Comparison")
dir.create(comp_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(comparison_tbl_rounded, file.path(comp_dir, "metrics_cv_vs_boot_test.csv"))

message("Vergleichstabelle gespeichert unter: ", normalizePath(file.path(comp_dir, "metrics_cv_vs_boot_test.csv")))
