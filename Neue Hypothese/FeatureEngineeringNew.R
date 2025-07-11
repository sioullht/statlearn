library(dplyr)
library(lubridate)

# ----------------------------
cat("📥 Lese Daten ein...\n")
df <- read.csv("ATP_ViLo_final.csv")
df$y <- as.factor(df$y)  # Zielvariable als Faktor
cat("✅ Daten erfolgreich eingelesen.\n")

df$date <- ymd(df$date)

# Matches nach Datum sortieren
df <- df %>%
  arrange(date)

get_player_stats_before_match <- function(player_id, match_date, df) {
  past_matches <- df %>%
    filter(date < match_date) %>%
    filter(player1_id == player_id | player2_id == player_id) %>%
    mutate(
      is_win = ifelse(player1_id == player_id & y == 1, 1,
                      ifelse(player2_id == player_id & y == 0, 1, 0)),
      ace = ifelse(player1_id == player_id, player1_ace, player2_ace),
      df = ifelse(player1_id == player_id, player1_df, player2_df),
      svpt = ifelse(player1_id == player_id, player1_svpt, player2_svpt),
      minutes = minutes,
      surface = surface,
      rank = ifelse(player1_id == player_id, player1_rank, player2_rank)
    )

  tibble(
    winrate = mean(past_matches$is_win, na.rm = TRUE),
    matches_played = nrow(past_matches),
    avg_svpt = mean(past_matches$svpt, na.rm = TRUE),
    avg_ace = mean(past_matches$ace, na.rm = TRUE),
    avg_df = mean(past_matches$df, na.rm = TRUE),
    avg_duration = mean(past_matches$minutes, na.rm = TRUE),
    winrate_clay = mean(past_matches$is_win[past_matches$surface == "Clay"], na.rm = TRUE),
    winrate_hard = mean(past_matches$is_win[past_matches$surface == "Hard"], na.rm = TRUE),
    winrate_grass = mean(past_matches$is_win[past_matches$surface == "Grass"], na.rm = TRUE)
  )
}

get_recent_form <- function(player_id, match_date, df, n = 10) {
  recent_matches <- df %>%
    filter(date < match_date) %>%
    filter(player1_id == player_id | player2_id == player_id) %>%
    arrange(desc(date)) %>%
    head(n) %>%
    mutate(is_win = ifelse(player1_id == player_id & y == 1, 1,
                           ifelse(player2_id == player_id & y == 0, 1, 0)))

  tibble(
    recent_winrate = mean(recent_matches$is_win, na.rm = TRUE),
    recent_matches = nrow(recent_matches)
  )
}

get_head2head <- function(p1, p2, match_date, df) {
  h2h_matches <- df %>%
    filter(date < match_date) %>%
    filter((player1_id == p1 & player2_id == p2) | (player1_id == p2 & player2_id == p1)) %>%
    mutate(
      p1_win = ifelse(player1_id == p1 & y == 1, 1,
                      ifelse(player2_id == p1 & y == 0, 1, 0))
    )

  tibble(
    h2h_total = nrow(h2h_matches),
    h2h_winrate_p1 = mean(h2h_matches$p1_win, na.rm = TRUE)
  )
}

get_ranking_avg <- function(player_id, match_date, df, days = 90) {
  recent_ranks <- df %>%
    filter(date < match_date & date >= (match_date - days(days))) %>%
    filter(player1_id == player_id | player2_id == player_id) %>%
    mutate(rank = ifelse(player1_id == player_id, player1_rank, player2_rank))

  mean(recent_ranks$rank, na.rm = TRUE)
}

get_match_context <- function(match_row) {
  tibble(
    surface = match_row$surface,
    best_of = match_row$best_of,
    match_weekday = wday(match_row$date, label = TRUE),
    match_month = month(match_row$date),
    season_phase = case_when(
      match_row$match_month %in% 1:4 ~ "early",
      match_row$match_month %in% 5:8 ~ "mid",
      TRUE ~ "late"
    )
  )
}

feature_df <- list()

for (i in 1:nrow(df)) {
  row <- df[i,]
  date_i <- row$date
  p1 <- row$player1_id
  p2 <- row$player2_id

  p1_stats <- get_player_stats_before_match(p1, date_i, df)
  p2_stats <- get_player_stats_before_match(p2, date_i, df)

  p1_form <- get_recent_form(p1, date_i, df)
  p2_form <- get_recent_form(p2, date_i, df)

  h2h <- get_head2head(p1, p2, date_i, df)

  p1_rank_avg <- get_ranking_avg(p1, date_i, df)
  p2_rank_avg <- get_ranking_avg(p2, date_i, df)

  context <- get_match_context(row)

  combined <- bind_cols(
    tibble(match_id = i, player1_id = p1, player2_id = p2),
    p1_stats %>% rename_with(~ paste0("p1_", .)),
    p2_stats %>% rename_with(~ paste0("p2_", .)),
    p1_form %>% rename_with(~ paste0("p1_", .)),
    p2_form %>% rename_with(~ paste0("p2_", .)),
    tibble(rank_avg_diff = p2_rank_avg - p1_rank_avg),
    h2h,
    context,
    tibble(y = row$y)
  )

  feature_df[[i]] <- combined
}

df_final <- bind_rows(feature_df)

# Zielvariable wieder hinzufügen
df$y <- target

# Datensatz speichern
write.csv(df, "ATP_final_FE.csv", row.names = FALSE)

cat("✅ Feature Engineering abgeschlossen. Datei gespeichert als 'ATP_final_FE.csv'\n")
