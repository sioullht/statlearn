library(tidyverse)

# 1. Daten einlesen
input_file <- "Step2/FE_Step2_Historical_Try2.csv"
output_file <- "Step3/FE_Step3_Filtered.csv"

# Lese den Datensatz
df <- read_csv(input_file)

# 2. Zeilen filtern und Spalten entfernen
df_final <- df %>%
  # Behalte nur die Zeilen, bei denen BEIDE Spieler eine Historie haben
  filter(player1_h_svpt > 0 & player2_h_svpt > 0) %>%
  
  # NEU: Entferne die ID- und Datums-Spalten
  select(-player1_id, -player2_id, -date)

# 3. Gefilterte Daten speichern
write_csv(df_final, output_file)