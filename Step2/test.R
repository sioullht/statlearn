# Benötigte Pakete laden
# Führen Sie 'install.packages("slider")' einmal aus, falls noch nicht geschehen.
library(tidyverse)
library(dplyr)
library(slider)

# --- Setup ---
input_file <- "Step1/DP_Step1_Reordered.csv" 
final_output_file <- "Step2/Final_Prediction_Dataset_SortedByDate.csv"

message("Skript gestartet. Lade die Eingabedatei...")
# Laden Sie den Datensatz
df <- read_csv(input_file)
message(paste("Datei", input_file, "erfolgreich geladen.", nrow(df), "Zeilen."))

# --- Schritt 1: Temporäre Umformung und Berechnung ---

message("\nSchritt 1: Daten werden für die historische Berechnung umgeformt...")
# Erstelle eine temporäre, eindeutige Zeilen-ID
df <- df %>% mutate(row_id = row_number())

# Spieler-Events erstellen
player1_events <- df %>% 
  select(row_id, date, player1_id, player1_ace:player1_svgms) %>% 
  rename_with(~str_remove(., "player1_"))

player2_events <- df %>% 
  select(row_id, date, player2_id, player2_ace:player2_svgms) %>% 
  rename_with(~str_remove(., "player2_"))

# Kombinieren und sortieren
player_events_long <- bind_rows(player1_events, player2_events) %>%
  arrange(id, date) # Sortieren nach Spieler und Datum

message("Umformung abgeschlossen. Beginne mit der Berechnung der rollierenden Durchschnitte...")
message("Dieser Schritt kann einige Minuten dauern...")

# Spalten für die Berechnung definieren
stats_to_average <- c("ace", "df", "svpt", "1stin", "1stwon", "2ndwon", "bpsaved", "bpfaced", "svgms")

# Historische Werte berechnen
historical_stats <- player_events_long %>%
  group_by(id) %>%
  mutate(across(
    .cols = all_of(stats_to_average),
    .fns = ~ slide_dbl(lag(.), ~mean(., na.rm = TRUE), .before = 9, .complete = FALSE),
    .names = "hist_{.col}"
  )) %>%
  mutate(across(starts_with("hist_"), ~replace_na(., 0))) %>%
  select(row_id, player_id = id, starts_with("hist_"))

message("Berechnung der historischen Werte abgeschlossen.")

# --- Schritt 2: Historische Daten zurück ins ursprüngliche Format bringen ---
message("\nSchritt 2: Historische Daten werden zurück in das Match-Format gefügt...")

df_final <- df %>%
  left_join(historical_stats, by = c("row_id", "player1_id" = "player_id")) %>%
  rename_with(~paste0("p1_", .), .cols = starts_with("hist_")) %>%
  left_join(historical_stats, by = c("row_id", "player2_id" = "player_id")) %>%
  rename_with(~paste0("p2_", .), .cols = starts_with("hist_"))

message("Zusammenfügen der Daten abgeschlossen.")

# --- Schritt 3: Finale Features basierend auf historischen Werten berechnen ---
message("\nSchritt 3: Finale prozentuale Werte und Differenzen werden berechnet...")

df_final <- df_final %>%
  mutate(
    # Prozentuale historische Werte
    p1_hist_ace_percent = ifelse(p1_hist_svpt > 0, p1_hist_ace / p1_hist_svpt, 0),
    p1_hist_serve_win_percent = ifelse(p1_hist_svpt > 0, (p1_hist_1stwon + p1_hist_2ndwon) / p1_hist_svpt, 0),
    p2_hist_ace_percent = ifelse(p2_hist_svpt > 0, p2_hist_ace / p2_hist_svpt, 0),
    p2_hist_serve_win_percent = ifelse(p2_hist_svpt > 0, (p2_hist_1stwon + p2_hist_2ndwon) / p2_hist_svpt, 0),
    
    # Historische Differenzen
    hist_ace_diff = p1_hist_ace_percent - p2_hist_ace_percent,
    hist_serve_win_diff = p1_hist_serve_win_percent - p2_hist_serve_win_percent
  )
message("Berechnung der finalen Features abgeschlossen.")

# --- Schritt 4: Finale Auswahl der Spalten (Data Leakage verhindern) ---
message("\nSchritt 4: Finale Spalten werden ausgewählt, um Data Leakage zu verhindern...")

df_final <- df_final %>%
  select(
    # Statische Daten und Ranking-Differenzen behalten
    player1_id, player2_id, player1_age, player2_age, player1_ht, player2_ht,
    p1_hand_R, p1_hand_L, p1_hand_U, p2_hand_R, p2_hand_L, p2_hand_U,
    player1_rank, player1_rank_pts, player2_rank, player2_rank_pts,
    log_rank_diff, rankpoints_diff,
    
    # Die NEUEN historischen Features behalten
    starts_with("p1_hist_"),
    starts_with("p2_hist_"),
    starts_with("hist_"),
    
    # Die Zielvariable
    y
  )

message("Finale Spaltenauswahl abgeschlossen.")

# Speichere den finalen Datensatz
message(paste("\nSpeichere den finalen Datensatz in:", final_output_file))
write_csv(df_final, final_output_file)

message("Skript erfolgreich beendet! 🎉")