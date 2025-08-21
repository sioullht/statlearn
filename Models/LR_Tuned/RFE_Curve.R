# Pakete installieren und laden
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("readr")) install.packages("readr")
if (!require("ggrepel")) install.packages("ggrepel") # NEU
library(ggplot2)
library(readr)
library(ggrepel) # NEU

# 1. Die Daten aus der Textdatei als Text-String einlesen
rfe_text_data <- "
Variables Accuracy  Kappa AccuracySD KappaSD
         2   0.6469 0.2937   0.008726 0.01746
         3   0.6468 0.2936   0.008170 0.01634
         4   0.6479 0.2958   0.007332 0.01467
         5   0.6491 0.2982   0.007571 0.01514
         6   0.6509 0.3018   0.009399 0.01880
         7   0.6514 0.3029   0.009329 0.01866
         8   0.6541 0.3082   0.009582 0.01917
         9   0.6537 0.3073   0.009289 0.01858
        10   0.6549 0.3098   0.008689 0.01738
        11   0.6542 0.3085   0.009502 0.01900
        12   0.6548 0.3096   0.009539 0.01908
        13   0.6552 0.3105   0.009508 0.01902
        14   0.6556 0.3111   0.010283 0.02057
        15   0.6558 0.3117   0.009783 0.01957
        16   0.6571 0.3143   0.008215 0.01643
        17   0.6564 0.3128   0.008187 0.01638
        18   0.6569 0.3138   0.007561 0.01513
        19   0.6565 0.3129   0.007406 0.01481
        20   0.6553 0.3107   0.007370 0.01474
        21   0.6539 0.3078   0.008517 0.01704
        22   0.6541 0.3082   0.007413 0.01483
        23   0.6534 0.3069   0.008114 0.01623
        24   0.6524 0.3049   0.007674 0.01535
        25   0.6540 0.3080   0.008076 0.01616
        26   0.6525 0.3050   0.009106 0.01822
        27   0.6527 0.3053   0.009349 0.01870
        28   0.6528 0.3057   0.009877 0.01976
        29   0.6528 0.3056   0.008395 0.01679
        30   0.6530 0.3060   0.008260 0.01652
"

# 2. Den Text in einen sauberen Datenrahmen umwandeln
rfe_results_df <- readr::read_table(rfe_text_data)

# 3. Den Plot mit ggplot2 erstellen
rfe_performance_curve <- ggplot(rfe_results_df, aes(x = Variables, y = Accuracy)) +
  geom_line(color = "skyblue", size = 1) +
  geom_point(color = "steelblue", size = 2.5) +
  geom_point(data = rfe_results_df[rfe_results_df$Variables == 16, ],
             aes(x = Variables, y = Accuracy),
             color = "red", size = 5, shape = 1) +
  
  # ERSETZTE TEXT-ZEILE
  ggrepel::geom_text_repel(data = rfe_results_df[rfe_results_df$Variables == 16, ],
            aes(label = paste("Optimum:", Accuracy)),
            color = "red",
            nudge_y = 0.001, # Schiebt den Text noch etwas weiter nach oben
            min.segment.length = 0) + # Zeichnet immer eine Verbindungslinie

  theme_minimal(base_size = 14) +
  labs(
    title = "Nachträglich erstellte RFE Leistungskurve",
    subtitle = "Die Modellgenauigkeit erreicht bei 16 Variablen ihr Maximum",
    x = "Anzahl der ausgewählten Variablen",
    y = "Modell-Genauigkeit (Accuracy)"
  )

# 4. Den Plot anzeigen
print(rfe_performance_curve)

# 5. Den Plot als PDF-Datei speichern
ggsave("RFE_Performance_Curve_nachtraeglich.pdf", 
       plot = rfe_performance_curve, 
       width = 10, 
       height = 6)