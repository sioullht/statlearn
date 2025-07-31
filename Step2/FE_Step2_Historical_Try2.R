# --- Schritt 0: Vorbereitung ---
library(tidyverse)
library(zoo)

df_original <- read_csv("Step1/DP_Step1_Reordered.csv")

df_sorted <- df_original %>%
  arrange(date) %>%
  mutate(match_id = row_number())

# --- Schritt 1: Daten umstrukturieren (Wide-to-Long-Format) ---
stat_cols <- c(
  "ace", "df", "svpt", "1stin", "1stwon", "2ndwon",
  "bpsaved", "bpfaced", "svgms"
)
p1_data <- df_sorted %>%
  select(match_id, date, player1_id, player2_id, one_of(paste0("player1_", stat_cols))) %>%
  rename_with(~str_remove(., "player1_"), .cols = all_of(paste0("player1_", stat_cols))) %>%
  rename(player_id = player1_id, opponent_id = player2_id)
p2_data <- df_sorted %>%
  select(match_id, date, player2_id, player1_id, one_of(paste0("player2_", stat_cols))) %>%
  rename_with(~str_remove(., "player2_"), .cols = all_of(paste0("player2_", stat_cols))) %>%
  rename(player_id = player2_id, opponent_id = player1_id)
long_data <- bind_rows(p1_data, p2_data) %>%
  arrange(player_id, date, match_id)

# --- NEU: Speichern der ersten Zwischendatei nach Schritt 1 ---
write_csv(long_data, "Step2/FE_Step2_Intermediate_Long_Format.csv")
print("Die erste Zwischendatei 'FE_Step2_Intermediate_Long_Format.csv' wurde erfolgreich gespeichert.")
# -----------------------------------------------------------------

# --- Schritt 2: Berechnung der rollierenden (historischen) Durchschnitte ---
historical_data <- long_data %>%
  group_by(player_id) %>%
  mutate(across(all_of(stat_cols),
                ~lag(rollapplyr(.,
                                width = 10,
                                FUN = mean,
                                partial = TRUE,
                                fill = NA,
                                align = "right")),
                .names = "h_{.col}")) %>%
  mutate(across(starts_with("h_"), ~replace_na(., 0))) %>%
  ungroup()

# --- NEU: Speichern der zweiten Zwischendatei nach Schritt 2 ---
write_csv(historical_data, "Step2/FE_Step2_Intermediate_With_Historical_Data.csv")
print("Die zweite Zwischendatei 'FE_Step2_Intermediate_With_Historical_Data.csv' wurde erfolgreich gespeichert.")
# ----------------------------------------------------------------------

# --- Schritt 3: Daten zurückführen (Long-to-Wide-Format) ---
historical_slim <- historical_data %>%
  select(match_id, player_id, starts_with("h_"))
df_final <- df_sorted %>%
  left_join(historical_slim, by = c("match_id", "player1_id" = "player_id")) %>%
  rename_with(~str_replace(., "h_", "player1_h_"), .cols = starts_with("h_"))
df_final <- df_final %>%
  left_join(historical_slim, by = c("match_id", "player2_id" = "player_id")) %>%
  rename_with(~str_replace(., "h_", "player2_h_"), .cols = starts_with("h_"))

# --- Schritt 4: Neuberechnung aller abgeleiteten Features ---
df_recalculated <- df_final %>%
  mutate(
    # Spieler 1 & 2 Prozentwerte
    p1_ace_percent = ifelse(player1_h_svpt > 0, player1_h_ace / player1_h_svpt, 0),
    p1_doublefault_percent = ifelse(player1_h_svpt > 0, player1_h_df / player1_h_svpt, 0),
    p1_1in_percent = ifelse(player1_h_svpt > 0, player1_h_1stin / player1_h_svpt, 0),
    p1_1won_percent = ifelse(player1_h_1stin > 0, player1_h_1stwon / player1_h_1stin, 0),
    p1_serve_win_percent = ifelse(player1_h_svpt > 0, (player1_h_1stwon + player1_h_2ndwon) / player1_h_svpt, 0),
    p1_breakp_saved = ifelse(player1_h_bpfaced > 0, player1_h_bpsaved / player1_h_bpfaced, 0),
    
    p2_ace_percent = ifelse(player2_h_svpt > 0, player2_h_ace / player2_h_svpt, 0),
    p2_doublefault_percent = ifelse(player2_h_svpt > 0, player2_h_df / player2_h_svpt, 0),
    p2_1in_percent = ifelse(player2_h_svpt > 0, player2_h_1stin / player2_h_svpt, 0),
    p2_1won_percent = ifelse(player2_h_1stin > 0, player2_h_1stwon / player2_h_1stin, 0),
    p2_serve_win_percent = ifelse(player2_h_svpt > 0, (player2_h_1stwon + player2_h_2ndwon) / player2_h_svpt, 0),
    p2_breakp_saved = ifelse(player2_h_bpfaced > 0, player2_h_bpsaved / player2_h_bpfaced, 0),
    
    # NEU: Angepasste Logik für breakp_succeed
    p1_breakp_succeed = ifelse(player1_h_svpt == 0, 0, 1 - p2_breakp_saved),
    p2_breakp_succeed = ifelse(player2_h_svpt == 0, 0, 1 - p1_breakp_saved),

    # Differenz-Features
    ace_diff = p1_ace_percent - p2_ace_percent,
    df_diff = p1_doublefault_percent - p2_doublefault_percent,
    in1_pct_diff = p1_1in_percent - p2_1in_percent,
    won1_pct_diff = p1_1won_percent - p2_1won_percent,
    sv_win_pct_diff = p1_serve_win_percent - p2_serve_win_percent,
    bp_saved_pct_diff = p1_breakp_saved - p2_breakp_saved,
    bp_per_game_diff = ifelse(player1_h_svgms > 0, player1_h_bpfaced / player1_h_svgms, 0) -
                       ifelse(player2_h_svgms > 0, player2_h_bpfaced / player2_h_svgms, 0)
  )

# --- Schritt 5: Finales Dataset erstellen ---
final_columns <- df_recalculated %>%
  select(
    # Statische Daten und Ranking
    player1_id, player2_id, player1_age, player2_age, player1_ht, player2_ht,
    p1_hand_R, p1_hand_L, p1_hand_U, p2_hand_R, p2_hand_L, p2_hand_U,
    date, player1_rank, player1_rank_pts, player2_rank, player2_rank_pts,

    # Neu berechnete Features
    log_rank_diff, rankpoints_diff, ace_diff, df_diff, in1_pct_diff,
    won1_pct_diff, sv_win_pct_diff, bp_saved_pct_diff, bp_per_game_diff,

    # Neu berechnete Spieler-Prozentwerte
    p1_ace_percent, p1_doublefault_percent, p1_1in_percent, p1_1won_percent,
    p1_serve_win_percent, p1_breakp_saved, p1_breakp_succeed,
    p2_ace_percent, p2_doublefault_percent, p2_1in_percent, p2_1won_percent,
    p2_serve_win_percent, p2_breakp_saved, p2_breakp_succeed,

    # Hinzugefügte historische Hilfsspalten
    starts_with("player1_h_"),
    starts_with("player2_h_"),

    # Zielvariable
    y
  )

# Speichere das finale Dataset
write_csv(final_columns, "Step2/FE_Step2_Historical_Try2.csv")
