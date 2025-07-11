# 📦 Pakete laden (installieren falls nötig)
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")

library(dplyr)
library(ggplot2)

#📥 CSV einlesen – passe den Pfad ggf. an, falls df nicht existiert
df <- read.csv("/Users/louisleicht/Statistical_Learning/ATP_ViLo_final.csv")

# ✅ Sicherstellen, dass df existiert und korrekt ist
if (!("data.frame" %in% class(df))) {
  stop("❌ Objekt 'df' ist kein DataFrame. Bitte lade die Spieldaten korrekt ein.")
}

# 👤 Spieler-IDs aus beiden Spalten zusammenführen
all_players <- bind_rows(
  df %>% select(player_id = player1_id),
  df %>% select(player_id = player2_id)
) %>%
  filter(!is.na(player_id))

# 📊 Matches pro Spieler zählen
player_counts <- all_players %>%
  group_by(player_id) %>%
  summarise(matches_played = n()) %>%
  ungroup()

# 🔍 Spieler mit mehr als 15 Matches herausfiltern
player_over_15 <- player_counts %>%
  filter(matches_played > 13)

# 📈 Histogramm der Spieler mit >15 Spielen
ggplot(player_over_15, aes(x = matches_played)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "Anzahl Spieler mit mehr als 15 Matches",
    x = "Anzahl Matches",
    y = "Anzahl Spieler"
  ) +
  theme_minimal()

# 📌 Statistische Kennzahlen berechnen
mean_matches <- mean(player_counts$matches_played)
median_matches <- median(player_counts$matches_played)
anzahl_spieler_über_15 <- nrow(player_over_15)
print(sum(table(c(df$player1_id, df$player2_id))))
print(length(unique(c(df$player1_id, df$player2_id))))

# 🖨️ Ausgabe in Konsole
cat("📊 Durchschnittliche Anzahl Spiele pro Spieler:", round(mean_matches, 2), "\n")
cat("📊 Median Anzahl Spiele pro Spieler:", median_matches, "\n")
cat("👥 Anzahl Spieler mit >15 Spielen:", anzahl_spieler_über_15, "\n")