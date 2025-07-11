library(dplyr)
library(readr)

# -------------------- #
# 1. Rohdaten einlesen #
# -------------------- #

df <- read_csv("atp_matches_till_2022.csv")

# ------------------------------- #
# 2. Relevante Spalten auswählen  #
# ------------------------------- #

spalten <- c(
  'tourney_date', 'tourney_name', 'surface', 
  'winner_id', 'winner_name', 'winner_hand', 'winner_ht', 'winner_ioc', 'winner_age',
  'loser_id', 'loser_name', 'loser_hand', 'loser_ht', 'loser_ioc', 'loser_age', 
  'score', 'best_of', 'minutes', 
  'w_ace', 'w_df', 'w_svpt', 'w_1stIn', 'w_1stWon', 'w_2ndWon', 'w_bpSaved', 'w_bpFaced', 'w_SvGms',
  'l_ace', 'l_df', 'l_svpt', 'l_1stIn', 'l_1stWon', 'l_2ndWon', 'l_bpSaved', 'l_bpFaced', 'l_SvGms',
  'winner_rank', 'loser_rank', 'winner_rank_points', 'loser_rank_points'
)

# Nur vorhandene Spalten auswählen
df <- df %>% select(any_of(spalten))

# tourney_date als numerisch, Jahr extrahieren
df$tourney_date <- as.numeric(df$tourney_date)
df$year <- df$tourney_date %/% 10000

# Daten auf Zeitraum 2000–2020 beschränken
df <- df %>%
  filter(year >= 2000 & year <= 2020) %>%
  select(-year)

# -------------------------------------- #
# 3. Zufällige Zuordnung von Spielern    #
# -------------------------------------- #

set.seed(123)

df <- df %>%
  mutate(rand = runif(n()),
         player1_name = ifelse(rand < 0.5, winner_name, loser_name),
         player2_name = ifelse(rand < 0.5, loser_name, winner_name)) %>%
  transmute(
    player1_id = ifelse(player1_name == winner_name, winner_id, loser_id),
    player2_id = ifelse(player2_name == winner_name, winner_id, loser_id),

    player1_name,
    player2_name,

    player1_ioc = ifelse(player1_name == winner_name, winner_ioc, loser_ioc),
    player2_ioc = ifelse(player2_name == winner_name, winner_ioc, loser_ioc),

    player1_age = ifelse(player1_name == winner_name, winner_age, loser_age),
    player2_age = ifelse(player2_name == winner_name, winner_age, loser_age),

    player1_ht = ifelse(player1_name == winner_name, winner_ht, loser_ht),
    player2_ht = ifelse(player2_name == winner_name, winner_ht, loser_ht),

    player1_hand = ifelse(player1_name == winner_name, winner_hand, loser_hand),
    player2_hand = ifelse(player2_name == winner_name, winner_hand, loser_hand),

    player1_ace = ifelse(player1_name == winner_name, w_ace, l_ace),
    player2_ace = ifelse(player2_name == winner_name, w_ace, l_ace),

    player1_df = ifelse(player1_name == winner_name, w_df, l_df),
    player2_df = ifelse(player2_name == winner_name, w_df, l_df),

    player1_svpt = ifelse(player1_name == winner_name, w_svpt, l_svpt),
    player2_svpt = ifelse(player2_name == winner_name, w_svpt, l_svpt),

    player1_1stin = ifelse(player1_name == winner_name, w_1stIn, l_1stIn),
    player2_1stin = ifelse(player2_name == winner_name, w_1stIn, l_1stIn),

    player1_1stwon = ifelse(player1_name == winner_name, w_1stWon, l_1stWon),
    player2_1stwon = ifelse(player2_name == winner_name, w_1stWon, l_1stWon),

    player1_2ndwon = ifelse(player1_name == winner_name, w_2ndWon, l_2ndWon),
    player2_2ndwon = ifelse(player2_name == winner_name, w_2ndWon, l_2ndWon),

    player1_bpsaved = ifelse(player1_name == winner_name, w_bpSaved, l_bpSaved),
    player2_bpsaved = ifelse(player2_name == winner_name, w_bpSaved, l_bpSaved),

    player1_bpfaced = ifelse(player1_name == winner_name, w_bpFaced, l_bpFaced),
    player2_bpfaced = ifelse(player2_name == winner_name, w_bpFaced, l_bpFaced),

    player1_svgms = ifelse(player1_name == winner_name, w_SvGms, l_SvGms),
    player2_svgms = ifelse(player2_name == winner_name, w_SvGms, l_SvGms),

    player1_rank = ifelse(player1_name == winner_name, winner_rank, loser_rank),
    player2_rank = ifelse(player2_name == winner_name, winner_rank, loser_rank),

    player1_rank_pts = ifelse(player1_name == winner_name, winner_rank_points, loser_rank_points),
    player2_rank_pts = ifelse(player2_name == winner_name, winner_rank_points, loser_rank_points),

    surface = surface,
    tourney_name = tourney_name,
    date = tourney_date,   # gleich umbenannt
    best_of = best_of,
    minutes = minutes,

    y = ifelse(player1_name == winner_name, 1, 0)
  )

# ---------------------- #
# 4. Weitere Bereinigung #
# ---------------------- #

df <- df %>%
  na.omit() %>%
  select(-player1_name, -player2_name)

# ------------------ #
# 5. Datei speichern #
# ------------------ #

write_csv(df, "ATP_ViLo_.csv")
