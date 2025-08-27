library(readr)
library(dplyr)
library(tidyr)

# ---- Dateilisten ----
base_files <- c(
  "/Users/louisleicht/Statistical_Learning/Models/XGB/metrics_train_vs_test.csv",
  "/Users/louisleicht/Statistical_Learning/Models/RF/metrics_train_vs_test.csv",
  "/Users/louisleicht/Statistical_Learning/Models/NNs/NN/metrics_combined_final.csv",
  "/Users/louisleicht/Statistical_Learning/Models/LR/metrics_train_vs_test.csv"
)

tuned_files <- c(
  "/Users/louisleicht/Statistical_Learning/Models/XGB_Tuned_/metrics_train_vs_test.csv",
  "/Users/louisleicht/Statistical_Learning/Models/RF_Tuned/metrics_train_vs_test.csv",
  "/Users/louisleicht/Statistical_Learning/Models/NNs/NN_Tuned/metrics_combined_final_.csv",
  "/Users/louisleicht/Statistical_Learning/Models/LR_Tuned/RFE_Bootstrapping/metrics_train_vs_test.csv"
)

# ---- Helper ----
read_and_tag <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  
  # Long -> Wide (falls nötig, z. B. NN-Resultate)
  if ("Metric" %in% names(df)) {
    df <- df %>%
      pivot_wider(names_from = Metric, values_from = Test)
    df$Split <- "Test"
  }
  
  model_name <- basename(dirname(path))
  df <- df %>%
    mutate(Model = model_name, Source = path) %>%
    relocate(Model, .before = 1) %>%
    relocate(Source, .after = last_col())
  
  return(df)
}

# ---- Einlesen ----
base_df  <- bind_rows(lapply(base_files,  read_and_tag))
tuned_df <- bind_rows(lapply(tuned_files, read_and_tag))

# ---- Ausgabe ----
out_dir <- "/Users/louisleicht/Statistical_Learning/Models/_Summary"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_base  <- file.path(out_dir, "metrics_standard_models.csv")
out_tuned <- file.path(out_dir, "metrics_tuned_models.csv")

write_csv(base_df,  out_base)
write_csv(tuned_df, out_tuned)

cat("Gespeichert:\n- ", normalizePath(out_base),
    "\n- ", normalizePath(out_tuned), "\n", sep = "")