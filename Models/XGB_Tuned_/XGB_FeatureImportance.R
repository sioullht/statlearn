library(xgboost)
library(ggplot2)
library(rlang)
library(readr)
library(dplyr)

# Pfade
model_dir <- "Models/XGB_Tuned_"
model_rds <- file.path(model_dir, "model_xgb_tuned_cv.rds")

# --- 1) Booster robust herausziehen -----------------------------------------
get_booster <- function(obj){
  if (inherits(obj, "xgb.Booster")) return(obj)
  if (!is.null(obj$finalModel) && inherits(obj$finalModel, "xgb.Booster")) return(obj$finalModel)

  if (inherits(obj, "workflow")) {
    fit <- tryCatch(workflows::pull_workflow_fit(obj), error = function(e) NULL)
    if (!is.null(fit) && inherits(fit$fit, "xgb.Booster")) return(fit$fit)
  }
  if (!is.null(obj$fit) && inherits(obj$fit, "xgb.Booster")) return(obj$fit)

  for (nm in c("model","booster")) {
    if (!is.null(obj[[nm]]) && inherits(obj[[nm]], "xgb.Booster")) return(obj[[nm]])
  }
  abort("Konnte keinen xgb.Booster im RDS finden. Prüfe die Struktur des Objekts.")
}

obj     <- readRDS(model_rds)
booster <- get_booster(obj)

# --- 2) Feature-Namen sicherstellen -----------------------------------------
feature_names <- NULL
if (!is.null(booster$feature_names) && length(booster$feature_names) > 0) {
  feature_names <- booster$feature_names
} else {
  tr_path <- "Step3/train_data.csv"
  if (file.exists(tr_path)) {
    train_data <- readr::read_csv(tr_path, show_col_types = FALSE)
    feature_names <- setdiff(names(train_data), "y")
  }
}

# --- 3) Importance berechnen -------------------------------------------------
if (is.null(feature_names) || length(feature_names) == 0) {
  imp <- xgb.importance(model = booster)
} else {
  imp <- xgb.importance(feature_names = feature_names, model = booster)
}

# --- 4) Plots speichern (nur Top 20 Features) -------------------------------
plot_imp <- function(df, col, title, outfile){
  df_top <- df %>%
    arrange(desc(.data[[col]])) %>%
    slice_head(n = 20)  # nur die Top 20
  p <- ggplot(df_top, aes(x = reorder(Feature, .data[[col]]), y = .data[[col]])) +
    geom_col() +
    coord_flip() +
    labs(title = title, x = "Feature", y = paste0("Importance (", col, ")")) +
    theme_minimal()
  ggsave(outfile, p, width = 8, height = 6)
}

outfile_gain <- file.path(model_dir, "FeatureImportance_Gain_Top20.pdf")
outfile_cover <- file.path(model_dir, "FeatureImportance_Cover_Top20.pdf")
outfile_freq <- file.path(model_dir, "FeatureImportance_Frequency_Top20.pdf")

plot_imp(imp, "Gain", "XGBoost Feature Importance (Gain) – Top 20", outfile_gain)
plot_imp(imp, "Cover", "XGBoost Feature Importance (Cover) – Top 20", outfile_cover)
plot_imp(imp, "Frequency", "XGBoost Feature Importance (Frequency) – Top 20", outfile_freq)

cat("Gespeichert:\n- ", normalizePath(outfile_gain),
    "\n- ", normalizePath(outfile_cover),
    "\n- ", normalizePath(outfile_freq), "\n", sep = "")