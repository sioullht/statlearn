# =======================================================
# Vergleichsskript: sammelt alle Modell-Metriken unter "Models/"
# und baut eine gemeinsame Übersichtstabelle + Plot
# =======================================================

# Pakete -------------------------------------------------
packages <- c("tidyverse","readr","stringr","ggplot2")
installed <- rownames(installed.packages())
for (p in packages) if (!p %in% installed) install.packages(p, dependencies = TRUE)
invisible(lapply(packages, library, character.only = TRUE))

# Ordner -------------------------------------------------
root_dir    <- "Models"
summary_dir <- file.path(root_dir, "_Summary")
plots_dir   <- file.path(summary_dir, "plots")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# Hilfsfunktionen ---------------------------------------
first_present <- function(x, candidates) {
  cand <- candidates[candidates %in% names(x)]
  if (length(cand) == 0) return(NULL)
  cand[1]
}

standardize_metrics <- function(df_in, model, variant, path){
  df <- as_tibble(df_in)

  # Spalte "Split" sicherstellen
  if (is.null(first_present(df, c("Split","split")))) {
    df <- mutate(df, Split = "Test") # Baselines ohne Split -> Test
  } else {
    nm <- first_present(df, c("Split","split"))
    df <- rename(df, Split = all_of(nm))
  }

  # Spalten flexibel mappen (viele Skripte benutzen Punktnamen)
  map_names <- list(
    Accuracy    = c("Accuracy","Accuracy.Accuracy","overallAccuracy","acc"),
    Precision   = c("Precision","Precision.Precision","prec"),
    Recall      = c("Recall","Recall.Recall","sens","Sensitivity"),
    F1_Score    = c("F1_Score","F1_Score.F1","F1","F1.F1"),
    AUC         = c("AUC","ROC","auc"),
    Log_Loss    = c("Log_Loss","LogLoss","logloss"),
    Brier_Score = c("Brier_Score","Brier","brier")
  )

  out <- tibble(Split = df$Split)
  for (nm in names(map_names)) {
    col <- first_present(df, map_names[[nm]])
    if (!is.null(col)) {
      out[[nm]] <- suppressWarnings(as.numeric(df[[col]]))
    } else {
      out[[nm]] <- NA_real_
    }
  }

  out <- mutate(out,
    Model   = model,
    Variant = variant,
    Source  = path
  ) %>%
    relocate(Model, Variant, Split)

  out
}

# Kandidaten-Dateinamen, die wir unterstützen ------------
# (füge bei Bedarf weitere hinzu)
patterns <- c(
  "metrics_train_vs_test\\.csv$",
  "metrics_train_vs_test_RAW\\.csv$",
  "metrics_train_vs_test_CAL\\.csv$",
  "metrics_baseline\\.csv$",
  "metrics_baseline_rankpoints\\.csv$",
  "model_metrics\\.csv$"
)

all_csvs <- list.files(root_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
metric_files <- all_csvs[str_detect(all_csvs, str_c(patterns, collapse = "|"))]

if (length(metric_files) == 0) {
  stop("Keine Metrik-CSV gefunden. Stelle sicher, dass deine Modelle ihre CSVs in 'Models/*' abgelegt haben.")
}

# Einlesen & Vereinheitlichen ----------------------------
rows <- list()

for (fp in metric_files) {
  # Modellname = Ordner direkt unter 'Models'
  # Beispiel: Models/XGB_Tuned/metrics_train_vs_test.csv -> "XGB_Tuned"
  parts <- str_split(fp, .Platform$file.sep, simplify = TRUE)
  idx_models <- which(parts == "Models")
  model <- if (length(idx_models) && (idx_models + 1) <= ncol(parts)) parts[ , idx_models + 1] else "UNKNOWN"

  # Variant aus Dateinamen
  fname <- basename(fp)
  variant <- case_when(
    str_detect(fname, "_RAW\\.csv$")       ~ "RAW",
    str_detect(fname, "_CAL\\.csv$")       ~ "CAL",
    str_detect(fname, "baseline_rankpoints") ~ "Baseline_RankPoints",
    str_detect(fname, "baseline")          ~ "Baseline",
    TRUE                                   ~ "Default"
  )

  # robust einlesen
  df <- suppressMessages(readr::read_csv(fp, show_col_types = FALSE))
  rows[[length(rows) + 1]] <- standardize_metrics(df, model, variant, fp)
}

metrics_all <- bind_rows(rows)

# Nur Test zuerst (für die Hauptvergleichstabelle) -------
metrics_test <- metrics_all %>%
  filter(tolower(Split) == "test")

# Sortierung: erst nach AUC, dann LogLoss
metrics_test_sorted <- metrics_test %>%
  arrange(desc(AUC), Log_Loss)

# Runden für Ausgabe
metrics_test_print <- metrics_test_sorted %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# Speichern ---------------------------------------------
out_csv_all       <- file.path(summary_dir, "metrics_all_rows.csv")
out_csv_test_only <- file.path(summary_dir, "metrics_test_only.csv")

readr::write_csv(metrics_all, out_csv_all)
readr::write_csv(metrics_test_sorted, out_csv_test_only)

cat("Gespeichert:\n - ", normalizePath(out_csv_all), 
    "\n - ", normalizePath(out_csv_test_only), "\n", sep = "")

# Konsolen-Vorschau -------------------------------------
cat("\n===== Vergleich (TEST, nach AUC absteigend) =====\n")
print(metrics_test_print %>%
        select(Model, Variant, AUC, Accuracy, Precision, Recall, F1_Score, Log_Loss, Brier_Score))

# Plot: AUC je Modell/Variante ---------------------------
if (nrow(metrics_test_sorted) > 0) {
  p_auc <- ggplot(metrics_test_sorted, aes(x = reorder(paste(Model, Variant, sep=" / "), AUC), y = AUC)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = sprintf("%.3f", AUC)), hjust = -0.05, size = 3) +
    ylim(0, max(1, max(metrics_test_sorted$AUC, na.rm = TRUE) + 0.02)) +
    labs(title = "Model Comparison (TEST) – AUC",
         x = "Model / Variant",
         y = "AUC") +
    theme_minimal()

  ggsave(file.path(plots_dir, "AUC_comparison_TEST.pdf"), p_auc, width = 8, height = 6)
  ggsave(file.path(plots_dir, "AUC_comparison_TEST.png"), p_auc, width = 8, height = 6, dpi = 150)

  cat("\nPlots gespeichert unter: ", normalizePath(plots_dir), "\n", sep = "")
}