library(dplyr)

set.seed(123)

df <- read.csv("ATP_ViLo.csv")

df_ml <- df %>%
  mutate(rand = runif(n()),
         
         player1_name = ifelse(rand < 0.5, winner_name, loser_name),
         player2_name = ifelse(rand < 0.5, loser_name, winner_name)) %>%
  
  transmute(
    # Spieler-ID, Name, Nation
    player1_id = ifelse(player1_name == winner_name, winner_id, loser_id),
    player2_id = ifelse(player2_name == winner_name, winner_id, loser_id),

    player1_name,
    player2_name,

    player1_ioc = ifelse(player1_name == winner_name, winner_ioc, loser_ioc),
    player2_ioc = ifelse(player2_name == winner_name, winner_ioc, loser_ioc),

    # Alter, Größe, Schlaghand
    player1_age = ifelse(player1_name == winner_name, winner_age, loser_age),
    player2_age = ifelse(player2_name == winner_name, winner_age, loser_age),

    player1_ht = ifelse(player1_name == winner_name, winner_ht, loser_ht),
    player2_ht = ifelse(player2_name == winner_name, winner_ht, loser_ht),

    player1_hand = ifelse(player1_name == winner_name, winner_hand, loser_hand),
    player2_hand = ifelse(player2_name == winner_name, winner_hand, loser_hand),

    # Match-Statistiken
    player1_ace     = ifelse(player1_name == winner_name, w_ace, l_ace),
    player2_ace     = ifelse(player2_name == winner_name, w_ace, l_ace),

    player1_df      = ifelse(player1_name == winner_name, w_df, l_df),
    player2_df      = ifelse(player2_name == winner_name, w_df, l_df),

    player1_svpt    = ifelse(player1_name == winner_name, w_svpt, l_svpt),
    player2_svpt    = ifelse(player2_name == winner_name, w_svpt, l_svpt),

    player1_1stin   = ifelse(player1_name == winner_name, w_1stIn, l_1stIn),
    player2_1stin   = ifelse(player2_name == winner_name, w_1stIn, l_1stIn),

    player1_1stwon  = ifelse(player1_name == winner_name, w_1stWon, l_1stWon),
    player2_1stwon  = ifelse(player2_name == winner_name, w_1stWon, l_1stWon),

    player1_2ndwon  = ifelse(player1_name == winner_name, w_2ndWon, l_2ndWon),
    player2_2ndwon  = ifelse(player2_name == winner_name, w_2ndWon, l_2ndWon),

    player1_bpsaved = ifelse(player1_name == winner_name, w_bpSaved, l_bpSaved),
    player2_bpsaved = ifelse(player2_name == winner_name, w_bpSaved, l_bpSaved),

    player1_bpfaced = ifelse(player1_name == winner_name, w_bpFaced, l_bpFaced),
    player2_bpfaced = ifelse(player2_name == winner_name, w_bpFaced, l_bpFaced),

    player1_svgms   = ifelse(player1_name == winner_name, w_SvGms, l_SvGms),
    player2_svgms   = ifelse(player2_name == winner_name, w_SvGms, l_SvGms),

    # Ranking
    player1_rank = ifelse(player1_name == winner_name, winner_rank, loser_rank),
    player2_rank = ifelse(player2_name == winner_name, winner_rank, loser_rank),

    player1_rank_pts = ifelse(player1_name == winner_name, winner_rank_points, loser_rank_points),
    player2_rank_pts = ifelse(player2_name == winner_name, winner_rank_points, loser_rank_points),

    # Kontextinformationen
    surface = surface,
    tourney_name = tourney_name,
    tourney_date = tourney_date,
    best_of = best_of,
    minutes = minutes,

    # Zielvariable
    y = ifelse(player1_name == winner_name, 1, 0)
  )

# In neue CSV-Datei exportieren
write.csv(df_ml, "ATP_ViLo_random.csv", row.names = FALSE)
