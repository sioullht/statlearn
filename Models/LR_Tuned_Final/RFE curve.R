# ===========================
# RFE Curve (Accuracy only)
# ===========================
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
})

# Pfade anpassen
rfe_txt <- "Models/LR_Tuned_Final/rfe_results.txt"
out_dir <- "Models/LR_Tuned_Final"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ----- Parser für caret-Ausgabe -----
parse_rfe_results <- function(file) {
  stopifnot(file.exists(file))
  lines <- readLines(file, warn = FALSE)

  header_idx <- grep("^\\s*Variables\\s+Accuracy\\s+Kappa\\s+AccuracySD\\s+KappaSD\\s+Selected", lines)
  if (!length(header_idx)) stop("Tabellen-Header nicht gefunden.")
  header_idx <- header_idx[1]

  data_lines <- character(0)
  for (i in (header_idx + 1):length(lines)) {
    li <- lines[i]
    if (grepl("^\\s*\\d+\\s+", li)) data_lines <- c(data_lines, li) else break
  }
  if (!length(data_lines)) stop("Keine Datenzeilen gefunden.")

  df <- lapply(data_lines, function(z) {
    z2 <- str_squish(z)
    parts <- str_split(z2, "\\s+", simplify = TRUE)
    if (ncol(parts) < 5) stop("Unerwartetes Zeilenformat: ", z2)
    if (ncol(parts) == 5) parts <- cbind(parts, "")  # leere Selected-Spalte ergänzen
    tibble(
      Variables  = as.integer(parts[1]),
      Accuracy   = as.numeric(parts[2]),
      AccuracySD = as.numeric(parts[4]),
      Selected   = parts[6] == "*"
    )
  }) |> bind_rows()

  df
}

# ----- Daten laden -----
rfe_df <- parse_rfe_results(rfe_txt)

# ----- Plot: Accuracy-Kurve mit SD-Ribbon -----
p_acc <- ggplot(rfe_df, aes(x = Variables, y = Accuracy)) +
  geom_ribbon(aes(ymin = Accuracy - AccuracySD, ymax = Accuracy + AccuracySD), alpha = 0.15) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_point(data = subset(rfe_df, Selected), aes(x = Variables, y = Accuracy),
             size = 3, shape = 21, fill = "white", stroke = 1.2) +
  geom_text(data = subset(rfe_df, Selected),
            aes(label = paste0("Selected: ", Variables)), vjust = -1, fontface = "bold") +
  labs(title = "RFE Curve (Accuracy)",
       x = "Anzahl Variablen (Subset Size)",
       y = "Accuracy (CV)") +
  theme_minimal(base_family = "sans") +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"))

# ----- Speichern -----
ggsave(file.path(out_dir, "rfe_curve_accuracy.jpg"), p_acc, width = 7, height = 5, dpi = 300)
ggsave(file.path(out_dir, "rfe_curve_accuracy.pdf"), p_acc, width = 7, height = 5)

message("Fertig. Gespeichert in: ", normalizePath(out_dir))


