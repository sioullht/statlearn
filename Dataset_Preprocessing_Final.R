library(dplyr)

# 1. CSV laden
df <- read.csv("ATP_ViLo_random.csv")

# 2. NA-Werte entfernen
df <- df %>% na.omit()

# 3. tourney_date in "date" umbenennen
df <- df %>%
  rename(date = tourney_date)

# 4. score-Spalte entfernen (falls sie noch existiert)
if ("score" %in% names(df)) {
  df <- df %>% select(-score)
}

# 5. Spielernamen entfernen
df <- df %>% select(-player1_name, -player2_name)

# 6. Tourney Name entfernen
df <- df %>% select(-tourney_name)

# 7. Optional: Speichern
write.csv(df, "ATP_ViLo_final.csv", row.names = FALSE)
