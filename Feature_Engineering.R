options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!require("dplyr")) install.packages("dplyr")
if (!require("fastDummies")) install.packages("fastDummies")

library(dplyr)
library(fastDummies)

df <- read.csv("ATP_ViLo_final.csv")

df <- fastDummies::dummy_cols(df, select_columns = "surface", remove_selected_columns = TRUE)

# Differenz-Features
df <- df %>%
  mutate(
    diff_age = player1_age - player2_age,
    diff_ht = player1_ht - player2_ht,
    diff_rank = player2_rank - player1_rank,
    diff_rank_pts = player1_rank_pts - player2_rank_pts,

    diff_ace = player1_ace - player2_ace,
    diff_df = player1_df - player2_df,
    diff_svpt = player1_svpt - player2_svpt,
    diff_1stin = player1_1stin - player2_1stin,
    diff_1stwon = player1_1stwon - player2_1stwon,
    diff_2ndwon = player1_2ndwon - player2_2ndwon,
    diff_bpsaved = player1_bpsaved - player2_bpsaved,
    diff_bpfaced = player1_bpfaced - player2_bpfaced,
    diff_svgms = player1_svgms - player2_svgms
  )

# Prozentuale Leistungswerte
df <- df %>%
  mutate(
    player1_1st_serve_pct = ifelse(player1_svpt > 0, player1_1stin / player1_svpt, NA),
    player2_1st_serve_pct = ifelse(player2_svpt > 0, player2_1stin / player2_svpt, NA),

    player1_1st_won_pct = ifelse(player1_1stin > 0, player1_1stwon / player1_1stin, NA),
    player2_1st_won_pct = ifelse(player2_1stin > 0, player2_1stwon / player2_1stin, NA),

    player1_bp_save_pct = ifelse(player1_bpfaced > 0, player1_bpsaved / player1_bpfaced, 1),
    player2_bp_save_pct = ifelse(player2_bpfaced > 0, player2_bpsaved / player2_bpfaced, 1),

    player1_total_points_won_pct = ifelse(player1_svpt > 0, (player1_1stwon + player1_2ndwon) / player1_svpt, NA),
    player2_total_points_won_pct = ifelse(player2_svpt > 0, (player2_1stwon + player2_2ndwon) / player2_svpt, NA)
  )

# Prozent-Differenzen
df <- df %>%
  mutate(
    diff_1st_serve_pct = player1_1st_serve_pct - player2_1st_serve_pct,
    diff_1st_won_pct = player1_1st_won_pct - player2_1st_won_pct,
    diff_bp_save_pct = player1_bp_save_pct - player2_bp_save_pct,
    diff_total_points_won_pct = player1_total_points_won_pct - player2_total_points_won_pct
  )

# Alle NA-Werte durch 0 ersetzen (z. B. bei Division durch 0)
df <- df %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

# Datensatz speichern
write.csv(df, "ATP_final_FE.csv", row.names = FALSE)

cat("✅ Feature Engineering abgeschlossen. Datei gespeichert als 'ATP_final_ready.csv'\n")