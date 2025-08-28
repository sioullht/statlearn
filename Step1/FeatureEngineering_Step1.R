library(tidyverse)
library(dplyr)

df <- read_csv("ATP_ViLo_.csv")
df <- df %>%
  mutate(
    p1_ace_percent     = (player1_ace / player1_svpt),
    p1_doublefault_percent      = (player1_df / player1_svpt),
    p1_1in_percent     = (player1_1stin / player1_svpt),
    p1_1won_percent    = ifelse(player1_1stin > 0, (player1_1stwon / player1_1stin), 0),
    p1_serve_win_percent  = ((player1_1stwon + player1_2ndwon) / player1_svpt),
    p1_breakp_saved = ifelse(player1_bpfaced > 0, (player1_bpsaved / player1_bpfaced), NA),
    p1_breakp_saved  = ifelse(player1_svgms > 0, player1_bpfaced / player1_svgms, NA),
    p1_breakp_succeed   = ifelse(player1_bpfaced > 0, 1 - (player1_bpsaved / player1_bpfaced), NA),

    p2_ace_percent     = (player2_ace / player2_svpt),
    p2_doublefault_percent      = (player2_df / player2_svpt),
    p2_1in_percent     = (player2_1stin / player2_svpt),
    p2_1won_percent    = ifelse(player2_1stin > 0, (player2_1stwon / player2_1stin), 0),
    p2_serve_win_percent  = ((player2_1stwon + player2_2ndwon) / player2_svpt),
    p2_breakp_saved = ifelse(player2_bpfaced > 0, (player2_bpsaved / player2_bpfaced), NA),
    p2_breakp_saved  = ifelse(player2_svgms > 0, player2_bpfaced / player2_svgms, NA),
    p2_breakp_succeed   = ifelse(player2_bpfaced > 0, 1 - (player2_bpsaved / player2_bpfaced), NA),

    ace_diff           = (player1_ace / player1_svpt) - (player2_ace / player2_svpt),
    df_diff            = (player1_df / player1_svpt) - (player2_df / player2_svpt),
    in1_pct_diff       = (player1_1stin / player1_svpt) - (player2_1stin / player2_svpt),
    won1_pct_diff      = ifelse(player1_1stin > 0, player1_1stwon / player1_1stin, 0) -
                         ifelse(player2_1stin > 0, player2_1stwon / player2_1stin, 0),
    sv_win_pct_diff    = ((player1_1stwon + player1_2ndwon) / player1_svpt) -
                         ((player2_1stwon + player2_2ndwon) / player2_svpt),
    bp_saved_pct_diff  = ifelse(player1_bpfaced > 0, player1_bpsaved / player1_bpfaced, NA) -
                         ifelse(player2_bpfaced > 0, player2_bpsaved / player2_bpfaced, NA),
    bp_per_game_diff   = ifelse(player1_svgms > 0, player1_bpfaced / player1_svgms, NA) -
                         ifelse(player2_svgms > 0, player2_bpfaced / player2_svgms, NA),

    
    # Logarithmischer Unterschied Ränge 
    log_rank_diff = log(player2_rank) - log(player1_rank),
    
    #Unterschied der Ranglistenpunkte
    rankpoints_diff = player1_rank_pts - player2_rank_pts,
    
    #One-Hot Encoding für player1_hand
    p1_hand_R = ifelse(player1_hand == 'R', 1, 0),
    p1_hand_L = ifelse(player1_hand == 'L', 1, 0),
    p1_hand_U = ifelse(player1_hand == 'U', 1, 0),

    #One-Hot Encoding für player2_hand
    p2_hand_R = ifelse(player2_hand == 'R', 1, 0),
    p2_hand_L = ifelse(player2_hand == 'L', 1, 0),
    p2_hand_U = ifelse(player2_hand == 'U', 1, 0)
  ) %>%

  #ursprünglichen 'hand'-Spalten entfernen
  select(-player1_hand, -player2_hand)

write.csv(df, "FE_Step1.csv", row.names = FALSE)
