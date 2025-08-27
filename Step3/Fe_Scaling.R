# Skript 2: Skalierung der Features durchführen

# install.packages("caret") # Falls nicht installiert
library(tidyverse)
library(caret)

# 1. Gefilterte Daten einlesen
input_file <- "Step3/FE_Step3_Filtered.csv"
df <- read_csv(input_file)

# 2. Daten aufteilen (Trainings- und Testset)
# Dies ist entscheidend, um die Skalierung korrekt durchzuführen!
# Wir nehmen 70% der Daten zum Trainieren und 30% zum Testen.
set.seed(123) # Für reproduzierbare Ergebnisse
train_indices <- createDataPartition(df$y, p = 0.7, list = FALSE)
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# 3. Prädiktoren für die Skalierung identifizieren
# Wir skalieren alle numerischen Spalten ausser IDs, Datum und der Zielvariable 'y'.
# Die Hand-Spalten (p1_hand_R etc.) sind bereits 0/1 und müssen nicht skaliert werden.
cols_to_scale <- setdiff(names(train_data), c("player1_id", "player2_id", "date", "p1_hand_R", "p1_hand_L", "p1_hand_U", "p2_hand_R", "p2_hand_L", "p2_hand_U", "y"))

# 4. Skalierungsparameter aus den Trainingsdaten lernen
# Wir verwenden die 'preProcess'-Funktion aus dem caret-Paket für die Standardisierung.
scaler <- preProcess(train_data[cols_to_scale], method = c("center", "scale"))

# 5. Skalierung auf Trainings- und Testdaten anwenden
train_data_scaled <- predict(scaler, train_data)
test_data_scaled <- predict(scaler, test_data)

# 6. Skalierte Daten speichern
write_csv(train_data_scaled, "Step3/train_data.csv")
write_csv(test_data_scaled, "Step3/test_data.csv")
