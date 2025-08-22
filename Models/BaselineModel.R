# -------------------------------------------------------
# Baseline (Probabilistic): Rankpunkte -> Sieg-Wahrscheinlichkeit
# P(y=1 | x) = sigmoid(k * (p1_pts - p2_pts))
# k wird auf dem Trainingsset per LogLoss minimiert (Gridsearch, log-spaced)
# -------------------------------------------------------

# Pakete
packages <- c("tidyverse","caret","pROC","ggplot2")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg, dependencies = TRUE)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

# -------------------------------------------------------
# Daten laden
# -------------------------------------------------------
train_data <- readr::read_csv("Step3/train_data.csv", show_col_types = FALSE)
test_data  <- readr::read_csv("Step3/test_data.csv",  show_col_types = FALSE)

# Check auf benötigte Spalten
needed_cols <- c("player1_rank_pts","player2_rank_pts","y")
missing_tr  <- setdiff(needed_cols, names(train_data))
missing_te  <- setdiff(needed_cols, names(test_data))
if (length(missing_tr) > 0 || length(missing_te) > 0) {
  stop(sprintf("Fehlende Spalten:\n train: %s\n test: %s",
               paste(missing_tr, collapse=", "),
               paste(missing_te, collapse=", ")))
}

# Ziel als Faktor (für caret) + numerisch (für AUC/LogLoss/Brier)
train_data$y <- factor(train_data$y, levels = c(0,1), labels = c("neg","pos"))
test_data$y  <- factor(test_data$y,  levels = c(0,1), labels = c("neg","pos"))
y_tr_num <- as.numeric(train_data$y == "pos")
y_te_num <- as.numeric(test_data$y  == "pos")

# -------------------------------------------------------
# Helper
# -------------------------------------------------------
clip <- function(p, eps = 1e-15) pmin(pmax(p, eps), 1 - eps)
sigmoid <- function(x) 1 / (1 + exp(-x))

logloss <- function(y, p) {
  p <- clip(p)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}
brier <- function(y, p) mean((y - p)^2)

# Metrics aus Probs + ggf. Klassen ableiten
compute_metrics <- function(y_true_fac, y_true_num, p_pred, thresh = 0.5) {
  cls <- ifelse(p_pred >= thresh, "pos", "neg") |> factor(levels = c("neg","pos"))
  cm  <- caret::confusionMatrix(cls, y_true_fac, positive = "pos")
  roc <- pROC::roc(response = y_true_num, predictor = p_pred, direction = "<", quiet = TRUE)
  tibble::tibble(
    Accuracy    = cm$overall["Accuracy"] |> as.numeric(),
    Precision   = cm$byClass["Precision"] |> as.numeric(),
    Recall      = cm$byClass["Recall"] |> as.numeric(),
    F1_Score    = cm$byClass["F1"] |> as.numeric(),
    AUC         = as.numeric(pROC::auc(roc)),
    Log_Loss    = logloss(y_true_num, p_pred),
    Brier_Score = brier(y_true_num, p_pred)
  )
}

# -------------------------------------------------------
# k-Optimierung auf TRAIN (log-spaced Grid)
# -------------------------------------------------------
# Rankpunkte-Differenz: positiver diff -> Spieler 1 stärker
diff_tr <- train_data$player1_rank_pts - train_data$player2_rank_pts
diff_te <- test_data$player1_rank_pts  - test_data$player2_rank_pts

# k-Kandidaten (breit log-skaliert)
k_grid <- exp(seq(log(1e-4), log(1), length.out = 50))  # 1e-4 ... 1.0

grid_res <- purrr::map_dfr(k_grid, function(k) {
  p_tr <- sigmoid(k * diff_tr)
  tibble::tibble(k = k, LogLoss = logloss(y_tr_num, p_tr))
})

best_k <- grid_res$k[which.min(grid_res$LogLoss)]

cat(sprintf("Bestes k (Train-LogLoss-min): %.6f\n", best_k))

# Optional: kleine Fein-Optimierung um best_k
k_fine <- exp(seq(log(best_k/3), log(best_k*3), length.out = 40))
grid_fine <- purrr::map_dfr(k_fine, function(k) {
  p_tr <- sigmoid(k * diff_tr)
  tibble::tibble(k = k, LogLoss = logloss(y_tr_num, p_tr))
})
best_k <- grid_fine$k[which.min(grid_fine$LogLoss)]
cat(sprintf("Fein-optimiertes k: %.6f\n", best_k))

# -------------------------------------------------------
# Vorhersagen mit best_k
# -------------------------------------------------------
p_tr <- sigmoid(best_k * diff_tr)
p_te <- sigmoid(best_k * diff_te)

# Optional: Threshold nach F1 auf TRAIN optimieren
ths <- seq(0.05, 0.95, by = 0.01)
f1s <- sapply(ths, function(t) {
  cls <- ifelse(p_tr >= t, "pos", "neg") |> factor(levels = c("neg","pos"))
  caret::confusionMatrix(cls, train_data$y, positive = "pos")$byClass["F1"]
})
best_thr <- ths[which.max(f1s)]
cat(sprintf("Bester Schwellenwert (F1 auf Train): %.2f\n", best_thr))

# -------------------------------------------------------
# Metriken
# -------------------------------------------------------
metrics_train <- compute_metrics(train_data$y, y_tr_num, p_tr, thresh = best_thr) |> dplyr::mutate(Split = "Train")
metrics_test  <- compute_metrics(test_data$y,  y_te_num, p_te, thresh = best_thr) |> dplyr::mutate(Split = "Test")
metrics_all   <- dplyr::bind_rows(metrics_train, metrics_test) |>
  dplyr::relocate(Split)

print(dplyr::mutate(metrics_all, dplyr::across(where(is.numeric), ~ round(.x, 4))))

# -------------------------------------------------------
# Ausgaben & Plots
# -------------------------------------------------------
out_dir  <- "Models/BaselineModel_RankPoints"
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# Speichere k & Threshold
readr::write_lines(sprintf("best_k=%.8f\nbest_threshold=%.2f", best_k, best_thr),
                   file.path(out_dir, "params.txt"))

# Metriken speichern
readr::write_csv(metrics_all, file.path(out_dir, "metrics_baseline_rankpoints.csv"))

# ROC-Plot (Test)
roc_te <- pROC::roc(y_te_num, p_te, direction = "<", quiet = TRUE)
p_roc <- pROC::ggroc(roc_te) +
  ggplot2::ggtitle(sprintf("ROC (RankPoints Baseline) - AUC = %.3f", as.numeric(pROC::auc(roc_te)))) +
  ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Baseline_RankPoints.png"),
                plot = p_roc, width = 6, height = 5, dpi = 150)

# Calibration-Plot (Test, 10 Bins)
cal_df <- tibble::tibble(
  pred = p_te,
  obs  = test_data$y
) |>
  dplyr::mutate(bin = cut(pred, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) |>
  dplyr::group_by(bin) |>
  dplyr::summarise(
    mean_pred = mean(pred),
    mean_obs  = mean(obs == "pos"),
    n = dplyr::n(),
    .groups = "drop"
  )

p_cal <- ggplot2::ggplot(cal_df, ggplot2::aes(x = mean_pred, y = mean_obs)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::labs(title = "Calibration (RankPoints Baseline)",
                x = "Predicted probability",
                y = "Observed frequency") +
  ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "Calibration_Baseline_RankPoints.png"),
                plot = p_cal, width = 6, height = 5, dpi = 150)

message("Fertig. Ergebnisse unter: ", normalizePath(out_dir))