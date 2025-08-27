# 02_demographics_combined.R
# Creates ONE file: density_age_height_combined.jpeg

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(dplyr)
})

# ---- Load ----
path <- "Step1/DP_Step1_Reordered.csv"   # <-- adjust if needed
dt <- fread(path)

# ---- Safety check for required columns ----
req <- c("player1_age","player2_age","player1_ht","player2_ht")
missing <- setdiff(req, names(dt))
if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))

# ---- Combine Age & Height into one long table for a faceted density plot ----
demo_long <- rbindlist(list(
  dt[, .(who = "Player1", variable = "Age (years)",   value = as.numeric(player1_age))],
  dt[, .(who = "Player2", variable = "Age (years)",   value = as.numeric(player2_age))],
  dt[, .(who = "Player1", variable = "Height (cm)",   value = as.numeric(player1_ht))],
  dt[, .(who = "Player2", variable = "Height (cm)",   value = as.numeric(player2_ht))]
)) %>%
  filter(!is.na(value))

# ---- Faceted density (Age & Height in one figure) ----
p_demo <- ggplot(demo_long, aes(x = value, fill = who)) +
  geom_density(alpha = 0.35) +
  facet_wrap(~ variable, scales = "free_x", ncol = 2) +
  labs(
    x = NULL, y = "Density", fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )

# ---- Save ONE image (JPEG) ----
ggsave("density_age_height_combined.jpeg", p_demo, width = 9, height = 4.8, dpi = 300)
