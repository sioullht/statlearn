library(readr)
library(dplyr)

files <- c(
  "/Users/louisleicht/Statistical_Learning/Models/XGB/metrics_train_vs_test.csv",
  "/Users/louisleicht/Statistical_Learning/Models/XGB_Tuned_/metrics_train_vs_test.csv",
  "/Users/louisleicht/Statistical_Learning/Models/RF_Tuned/metrics_train_vs_test.csv",
  "/Users/louisleicht/Statistical_Learning/Models/RF/metrics_train_vs_test.csv",
  "/Users/louisleicht/Statistical_Learning/Models/NNs/NN_Tuned/metrics_combined_final.csv",
  "/Users/louisleicht/Statistical_Learning/Models/NNs/NN/metrics_train_vs_test_nn.csv",
  "/Users/louisleicht/Statistical_Learning/Models/LR/metrics_train_vs_test.csv",
  "/Users/louisleicht/Statistical_Learning/Models/LR_Tuned/RFE_Bootstrapping/metrics_train_vs_test.csv"
)

read_and_tag <- function(path){
  df <- read_csv(path, show_col_types = FALSE)
  model_name <- basename(dirname(path))   # Ordnername = Modell
  df <- df %>%
    mutate(Model = model_name,
           Source = path) %>%
    relocate(Model, .before = 1)
  return(df)
}

metrics_all <- bind_rows(lapply(files, read_and_tag))

# Nur Test behalten
metrics_test <- metrics_all %>% filter(tolower(Split) == "test")

# Standard vs. Tuned
metrics_test_std   <- metrics_test %>% filter(Model %in% c("XGB","RF"))
metrics_test_tuned <- metrics_test %>% filter(Model %in% c("XGB_Tuned_","RF_Tuned"))

# Speicherpfade
out_dir <- "/Users/louisleicht/Statistical_Learning/Models/_Summary"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_all   <- file.path(out_dir, "metrics_train_test.csv")
out_test  <- file.path(out_dir, "metrics_test.csv")
out_std   <- file.path(out_dir, "metrics_test_standard.csv")
out_tuned <- file.path(out_dir, "metrics_test_tuned.csv")

# Speichern
write_csv(metrics_all,   out_all)
write_csv(metrics_test,  out_test)
write_csv(metrics_test_std,   out_std)
write_csv(metrics_test_tuned, out_tuned)

cat("Gespeichert:\n- ", normalizePath(out_all),
    "\n- ", normalizePath(out_test),
    "\n- ", normalizePath(out_std),
    "\n- ", normalizePath(out_tuned), "\n", sep = "")