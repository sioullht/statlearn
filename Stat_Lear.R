library(dplyr)
library(readr)

# CSV-Datei einlesen
df <- read_csv("atp_matches_till_2022.csv")

# Relevante Spalten auswählen
spalten <- c(
  'tourney_date', 'surface', 'match_num', 'winner_id', 'winner_name', 'winner_hand', 'winner_ht', 'winner_ioc', 'winner_age',
  'loser_id', 'loser_name', 'loser_hand', 'loser_ht', 'loser_ioc', 'loser_age', 'score',
  'w_ace', 'w_df', 'w_svpt', 'w_1stIn', 'w_1stWon', 'w_2ndWon', 'w_bpSaved', 'w_bpFaced', 'w_SvGms',
  'l_ace', 'l_df', 'l_svpt', 'l_1stIn', 'l_1stWon', 'l_2ndWon', 'l_bpSaved', 'l_bpFaced', 'l_SvGms',
  'winner_rank', 'loser_rank', 'winner_rank_points', 'loser_rank_points',
  'winner_age', 'winner_ht', 'winner_hand', 'loser_age', 'loser_ht', 'loser_hand'
)

# Nur ausgewählte Spalten behalten
df <- df %>% select(all_of(spalten))

# Erste Zeilen anzeigen
print(head(df))

# Bereinigte Datei speichern
write_csv(df, "ATP_bereinigt.csv")

# Datei erneut einlesen
df <- read_csv("ATP_bereinigt.csv")

# 'tourney_date' in numerisch umwandeln
df$tourney_date <- as.numeric(df$tourney_date)

# Jahr extrahieren
df$year <- df$tourney_date %/% 10000

# Daten zwischen 2000 und 2020 filtern
df_filtered <- df %>%
  filter(year >= 2000 & year <= 2020) %>%
  select(-year)

# Gefilterte Datei speichern
write_csv(df_filtered, "ATP_ViLo.csv")