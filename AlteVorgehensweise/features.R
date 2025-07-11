if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("randomForest")) install.packages("randomForest")

# Pakete laden
library(randomForest)
library(ggplot2)
library(dplyr)

# Modell laden
load("/Users/louisleicht/Statistical_Learning/ModelOutput/rf_model2.RData")  # lädt das Objekt "model_rf"

# Feature Importance extrahieren
importance_df <- as.data.frame(importance(model_rf))
importance_df$Feature <- rownames(importance_df)

# Sortieren nach Wichtigkeit
importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini))

# Top 20 Features auswählen (optional)
top_n <- 20
importance_top <- head(importance_df, top_n)

# Plot erstellen und anzeigen
plot_importance <- ggplot(importance_top, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = paste("Top", top_n, "wichtigste Merkmale (Random Forest)"),
    x = "Merkmal",
    y = "MeanDecreaseGini"
  )

# Plot anzeigen
print(plot_importance)

# CSV-Datei exportieren
write.csv(importance_df, "feature_importance.csv", row.names = FALSE)

# Bestätigung ausgeben
cat("Feature Importance wurde in 'feature_importance.csv' gespeichert.\n")
cat("Plot wurde erstellt und angezeigt.\n")