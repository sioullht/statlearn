packages <- c("tidyverse","readr","stringr")
installed <- rownames(installed.packages())
for (p in packages) if (!p %in% installed) install.packages(p, dependencies = TRUE)
invisible(lapply(packages, library, character.only = TRUE))

root_dir    <- "Models"
summary_dir <- file.path(root_dir, "_Summary")
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)

to_num_safely <- function(x) {
  x <- gsub(",", ".", as.character(x), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}
first_present_col <- function(df, candidates) {
  cand <- candidates[candidates %in% names(df)]
  if (length(cand) == 0) return(NULL)
  cand[1]
}

# Ziel-Spaltennamen 
KANON <- c("Accuracy","Precision","Recall","F1_Score","AUC","Log_Loss","Brier_Score")

# Mapping bei Spaltenformat (breit)
COLUMN_MAP <- list(
  Split        = c("Split","split","dataset","DataSplit"),
  Accuracy     = c("Accuracy","Accuracy.Accuracy","overallAccuracy","acc"),
  Precision    = c("Precision","Precision.Precision","prec","Pos_Precision"),
  Recall       = c("Recall","Recall.Recall","Sensitivity","sens","TPR","Recall_pos"),
  F1_Score     = c("F1_Score","F1_Score.F1","F1","F1.F1","F1_pos"),
  AUC          = c("AUC","ROC","auc","AUC_ROC"),
  Log_Loss     = c("Log_Loss","LogLoss","logloss","log_loss","Logloss"),
  Brier_Score  = c("Brier_Score","Brier","brier","BrierScore")
)

# Mapping bei Zeilenformat (Metric/Train/Test)
# wir normalisieren: lower + entferne Nicht-Alphanumerisches, dann Regex-Match
ROWMAP_REGEX <- list(
  Accuracy     = c("^accuracy(accuracy)?$", "^acc$"),
  Precision    = c("^precision(precision)?$", "^prec$"),
  Recall       = c("^recall(recall)?$", "^sens(itivity)?$", "^tpr$"),
  F1_Score     = c("^f1(score)?(f1)?$","^f1score$","^f1$"),
  AUC          = c("^auc$", "^roc$"),
  Log_Loss     = c("^logloss$", "^logloss$", "^logloss$","^logloss$","^logloss$","^logloss$","^logloss$","^logloss$"), # Platzhalter
  Brier_Score  = c("^brierscore$", "^brier$")
)
# Korrigiere Log_Loss Regex (oben als Schutz doppelt) – final:
ROWMAP_REGEX$Log_Loss <- c("^logloss$", "^log_loss$", "^logloss$")

normalize_key <- function(s) {
  s <- tolower(trimws(as.character(s)))
  s <- gsub("[^a-z0-9]", "", s)
  s
}
map_row_metric_to_kanon <- function(key_norm) {
  for (k in names(ROWMAP_REGEX)) {
    if (any(stringr::str_detect(key_norm, ROWMAP_REGEX[[k]]))) return(k)
  }
  # Spezialfälle häufig gesehen
  if (key_norm %in% c("accuracyaccuracy","accuracy")) return("Accuracy")
  if (key_norm %in% c("precisionprecision","precision")) return("Precision")
  if (key_norm %in% c("recallrecall","recall","sensitivity","sens","tpr")) return("Recall")
  if (key_norm %in% c("f1scoref1","f1score","f1")) return("F1_Score")
  if (key_norm %in% c("auc","roc")) return("AUC")
  if (key_norm %in% c("logloss","logloss","logloss")) return("Log_Loss")
  if (key_norm %in% c("brierscore","brier")) return("Brier_Score")
  NA_character_
}

# Standardisiert eine beliebige Metrics-CSV zu EINER Zeile pro Split
standardize_metrics <- function(df_in, model, variant, source_path) {
  df <- as_tibble(df_in)

  # --- Fall A: ZEILENFORMAT (Metric / Train / Test) ---
  has_metric <- any(tolower(names(df)) == "metric")
  has_train  <- any(tolower(names(df)) == "train")
  has_test   <- any(tolower(names(df)) == "test")

  if (has_metric && (has_test || has_train) && nrow(df) >= 3) {
    mcol <- names(df)[tolower(names(df)) == "metric"][1]
    tcol <- if (has_test) names(df)[tolower(names(df)) == "test"][1] else NULL
    trcol <- if (has_train) names(df)[tolower(names(df)) == "train"][1] else NULL

    # Werte für TEST extrahieren (wir wollen Test-only CSV)
    if (!is.null(tcol)) {
      norm_keys <- normalize_key(df[[mcol]])
      kanons <- vapply(norm_keys, map_row_metric_to_kanon, character(1))
      values <- to_num_safely(df[[tcol]])

      tmp <- tibble::tibble(Kanon = kanons, Wert = values) %>%
        filter(!is.na(Kanon)) %>%
        group_by(Kanon) %>% slice_tail(n = 1) %>% ungroup()

      out <- tibble(Split = "Test")
      for (nm in KANON) {
        val <- tmp$Wert[tmp$Kanon == nm]
        out[[nm]] <- if (length(val)) val else NA_real_
      }

      return(out %>%
               mutate(Model = model, Variant = variant, Source = source_path) %>%
               relocate(Model, Variant, Split))
    } else {
      # Nur Train vorhanden -> überspringen, da wir Test-only schreiben
      return(tibble())
    }
  }

  # --- Fall B: SPALTENFORMAT (klassisch) ---
  split_col <- first_present_col(df, COLUMN_MAP$Split)
  if (!is.null(split_col)) {
    df <- df %>% rename(Split = all_of(split_col))
  } else {
    fn <- basename(source_path)
    split_guess <- dplyr::case_when(
      grepl("test", fn,  ignore.case = TRUE) ~ "Test",
      grepl("train", fn, ignore.case = TRUE) ~ "Train",
      TRUE                                   ~ "Test"
    )
    df$Split <- split_guess
  }

  df$Split <- dplyr::case_when(
    tolower(df$Split) %in% c("test","testing","eval","evaluation") ~ "Test",
    tolower(df$Split) %in% c("train","training")                   ~ "Train",
    TRUE                                                           ~ as.character(df$Split)
  )

  out <- tibble(Split = as.character(df$Split))
  for (nm in setdiff(names(COLUMN_MAP), "Split")) {
    col <- first_present_col(df, COLUMN_MAP[[nm]])
    out[[nm]] <- if (!is.null(col)) to_num_safely(df[[col]]) else NA_real_
  }

  out %>%
    mutate(Model = model, Variant = variant, Source = source_path) %>%
    relocate(Model, Variant, Split)
}

# ---- Dateien suchen: alles unter Models/, aber _Summary ausschließen ----
norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

all_csvs <- list.files(root_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
all_csvs_n <- norm(all_csvs)
summary_prefix <- norm(file.path(root_dir, "_Summary"))

metric_files <- all_csvs[
  grepl("metrics", basename(all_csvs), ignore.case = TRUE) &
  !grepl("confusion|importance|param|feature|coeff|weight|coefs|importances|pred|oof|calibr|thres",
         basename(all_csvs), ignore.case = TRUE) &
  !startsWith(all_csvs_n, summary_prefix) &
  !basename(all_csvs) %in% c(
    "metrics_all_rows.csv",
    "metrics_test_only.csv",
    "metrics_all_train_test.csv",
    "leaderboard_by_model.csv",
    "leaderboard_global.csv"
  )
]

if (length(metric_files) == 0) {
  stop("Keine Metrik-CSV gefunden. Stelle sicher, dass deine Skripte 'metrics*.csv' schreiben.")
}

# ---- Einlesen & Vereinheitlichen (Model + Variant aus weiterem Pfad) ----
rows <- list()

for (fp in metric_files) {
  parts <- str_split(fp, .Platform$file.sep, simplify = TRUE)
  idx_models <- which(parts == "Models")
  if (!length(idx_models)) next

  # Model = Ordner direkt unter 'Models'
  model <- if ((idx_models + 1) <= ncol(parts)) parts[, idx_models + 1] else "UNKNOWN"

  # Variant = alle Unterordner zwischen <Model> und Datei (ohne 'plots')
  after_model_idx <- idx_models + 1
  before_file_idx <- ncol(parts)
  subs <- character(0)
  if (before_file_idx > after_model_idx + 0) {
    subpath <- parts[, (after_model_idx + 1):(before_file_idx - 0), drop = FALSE]
    subs <- subpath[subpath != ""]
    subs <- subs[!grepl("\\.csv$", subs, ignore.case = TRUE)]
    subs <- subs[!grepl("^plots$", subs, ignore.case = TRUE)]
  }
  variant <- if (length(subs)) paste(subs, collapse = "/") else "Default"

  df_raw <- suppressMessages(readr::read_csv(fp, show_col_types = FALSE))
  std <- standardize_metrics(df_raw, model, variant, fp)
  if (nrow(std)) rows[[length(rows) + 1]] <- std
}

metrics_all <- bind_rows(rows) %>% distinct()

# ---- nur TEST behalten und Spalten wie bisher anordnen ----
metrics_test <- metrics_all %>% filter(tolower(Split) == "test")

col_order <- c("Model","Variant","Split",
               "Accuracy","Precision","Recall","F1_Score",
               "AUC","Log_Loss","Brier_Score","Source")
for (nm in col_order) if (!nm %in% names(metrics_test)) metrics_test[[nm]] <- NA
metrics_test <- metrics_test %>% select(all_of(col_order))

# ---- speichern (wie bisher) ----
out_csv_test_only <- file.path(summary_dir, "metrics_test_only.csv")
readr::write_csv(metrics_test, out_csv_test_only)

cat("Gespeichert:\n - ", normalizePath(out_csv_test_only), "\n", sep = "")

# optionale Vorschau
cat("\n===== Vorschau (erste 20 Zeilen TEST) =====\n")
print(
  metrics_test %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
    slice_head(n = 20)
)