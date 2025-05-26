# tidyverse laden (wenn noch nicht installiert: install.packages("tidyverse"))
library(tidyverse)

# Pfad zum Datensatz (anpassen, falls dein Benutzername anders ist)
data_path <- "/Users/louisleicht/Statistical_Learning/atp_matches_till_2022.csv"

# CSV einlesen
matches <- read_csv(data_path)

# Nur die relevanten Spalten auswählen
selected_data <- matches %>%
  select(
    w_ace, w_df, w_svpt, w_1stIn, w_1stWon, w_2ndWon,
    w_bpSaved, w_bpFaced, w_SvGms,
    l_ace, l_df, l_svpt, l_1stIn, l_1stWon, l_2ndWon,
    l_bpSaved, l_bpFaced, l_SvGms,
    winner_rank, loser_rank,
    winner_rank_points, loser_rank_points,
    winner_age, winner_ht, winner_hand,
    loser_age, loser_ht, loser_hand
  )

# Optional: Nur vollständige Zeilen behalten
selected_data_clean <- selected_data %>%
  drop_na()

# Erste Zeilen prüfen
head(selected_data_clean)

write_csv(selected_data_clean, "/Users/louisleicht/Statistical_Learning/atp_cleaned.csv")


#TODO: Schauen wie man R wie JNs nutzt. Dass alles aufgeräumter ist und einzelne Code abschnitte ausgeführt werden könnnen 