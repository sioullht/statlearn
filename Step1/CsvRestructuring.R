library(tidyverse)
library(dplyr)

# Eingabe- und Ausgabedateien definieren
input_file <- "Step1/DP_Step1.csv"
output_file <- "Step1/DP_Step1_Reordered.csv"

# Lese die CSV-Datei
df <- read_csv(input_file)

# Wähle und ordne die Spalten neu an
df_reordered <- df %>%
  select(
    # --- Statische Spielerdaten ---
    player1_id,
    player2_id,
    player1_age,
    player2_age,
    player1_ht,
    player2_ht,
    p1_hand_R, p1_hand_L, p1_hand_U,
    p2_hand_R, p2_hand_L, p2_hand_U,
    date,
    
    # --- Ranking-Daten ---
    player1_rank,
    player1_rank_pts,
    player2_rank,
    player2_rank_pts,
    
    # --- Berechnete Differenz-Features (die wichtigsten Prädiktoren) ---
    log_rank_diff,
    rankpoints_diff,
    ace_diff,
    df_diff,
    in1_pct_diff,
    won1_pct_diff,
    sv_win_pct_diff,
    bp_saved_pct_diff,
    bp_per_game_diff,
    
    # --- Berechnete Einzel-Features (Detail-Performance) ---
    p1_ace_percent, p1_doublefault_percent, p1_1in_percent, p1_1won_percent, p1_serve_win_percent, p1_breakp_saved, p1_breakp_succeed,
    p2_ace_percent, p2_doublefault_percent, p2_1in_percent, p2_1won_percent, p2_serve_win_percent, p2_breakp_saved, p2_breakp_succeed,
    
    # --- Originale Roh-Statistiken (falls für Analysen benötigt) ---
    player1_ace, player1_df, player1_svpt, player1_1stin, player1_1stwon, player1_2ndwon, player1_bpsaved, player1_bpfaced, player1_svgms,
    player2_ace, player2_df, player2_svpt, player2_1stin, player2_1stwon, player2_2ndwon, player2_bpsaved, player2_bpfaced, player2_svgms,

    # --- Zielvariable 'y' GANZ AM ENDE ---
    y
  )

# Speichere den neu geordneten DataFrame als neue CSV-Datei
write_csv(df_reordered, output_file)