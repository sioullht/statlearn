library(tidyverse)
library(dplyr)

# Den Dateipfad zu Ihrer Quelldatei
input_file <- "Step1/FE_Step1.csv"

# Name für die neue, bearbeitete Datei
output_file <- "Step1/DP_Step1.csv"

# Lese, filtere, wähle Spalten aus und schreibe die neue CSV-Datei in einem Schritt
read_csv(input_file) %>%
  
  # Schritt 1: Behalte nur Spiele, die auf "Hard" gespielt wurden
  filter(surface == "Hard") %>%
  
  # Schritt 2: Entferne die nicht mehr benötigten Spalten
  select(-tourney_name, -date, -best_of, -minutes, -player1_ioc, -player2_ioc) %>%

  # Schritt 3 (NEU): Die konstante "surface"-Spalte entfernen
  select(-surface) %>%
  
  # Schritt 4: Speichere den bearbeiteten Data Frame als neue CSV-Datei
  write_csv(output_file)