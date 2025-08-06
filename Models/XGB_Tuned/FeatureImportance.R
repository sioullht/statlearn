# -------------------------------------------------------
# Feature Importance für XGBoost-Modell
# -------------------------------------------------------

library(caret)
library(tibble)
library(ggplot2)
library(dplyr)

# Modell laden
model_xgb <- readRDS("Models/XGB_Tuned/model_xgb_tuned.rds")

# Importance berechnen
imp <- varImp(model_xgb)$importance %>%
  rownames_to_column("Feature") %>%
  as_tibble() %>%
  arrange(desc(Overall)) %>%
  dplyr::slice(1:20)  # << explizit dplyr::slice verwenden

# Plot
ggplot(imp, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 20 Feature Importances (XGBoost)",
    x = "Feature",
    y = "Importance"
  )

# Optional: als PDF speichern
ggsave("Models/XGB_Tuned/feature_importance_xgb.pdf", width = 8, height = 6)

find("slice")
