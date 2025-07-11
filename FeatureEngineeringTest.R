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

# ----------------------------
# 📊 Spielerstatistiken vor dem Match
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
    avg_duration = mean(past_matches$minutes, na.rm = TRUE)
  )
}

# ----------------------------
# 🗓️ Kontext-Matchinformationen
get_match_context <- function(match_row) {
  tibble(
    surface = match_row$surface,
    best_of = match_row$best_of,
    match_weekday = wday(match_row$date, label = TRUE),
    match_month = month(match_row$date),
    season_phase = case_when(
      month(match_row$date) %in% 1:4 ~ "early",
      month(match_row$date) %in% 5:8 ~ "mid",
      TRUE ~ "late"
    )
  )
}

# ----------------------------
# 🚀 Feature Engineering Loop (vereinfacht)
feature_df <- list()

for (i in 1:nrow(df)) {
  row <- df[i,]
  date_i <- row$date
  p1 <- row$player1_id
  p2 <- row$player2_id

  p1_stats <- get_player_stats_before_match(p1, date_i, df)
  p2_stats <- get_player_stats_before_match(p2, date_i, df)

  context <- get_match_context(row)

  combined <- bind_cols(
    tibble(match_id = i, player1_id = p1, player2_id = p2),
    p1_stats %>% rename_with(~ paste0("p1_", .)),
    p2_stats %>% rename_with(~ paste0("p2_", .)),
    context,
    tibble(y = row$y)
  )

  feature_df[[i]] <- combined

  # Optional: Fortschritt anzeigen
  if (i %% 1000 == 0) cat("⏳ Verarbeitet:", i, "von", nrow(df), "Matches\n")
}

# ----------------------------
# ✅ Finaler Datensatz speichern
df_final <- bind_rows(feature_df)
df_final$y <- as.factor(df_final$y)

write.csv(df_final, "ATP_final_FE_fast.csv", row.names = FALSE)
cat("✅ Feature Engineering abgeschlossen. Datei gespeichert als 'ATP_final_FE_fast.csv'\n")
