# -------------------------------------------------------
# 1. Pakete laden
# -------------------------------------------------------
packages <- c("tidyverse", "randomForest", "pROC", "scales", "caret")

installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!pkg %in% installed) install.packages(pkg)
}
lapply(packages, library, character.only = TRUE)

# -------------------------------------------------------
# 2. Daten einlesen
# -------------------------------------------------------
train_data <- read_csv("Step3/train_data.csv")
test_data  <- read_csv("Step3/test_data.csv")

# -------------------------------------------------------
# 3. Zielvariable vorbereiten (0/1 → neg/pos für binary classification)
# -------------------------------------------------------
train_data$y <- factor(train_data$y, levels = c(0, 1), labels = c("neg", "pos"))
test_data$y  <- factor(test_data$y,  levels = c(0, 1), labels = c("neg", "pos"))

# -------------------------------------------------------
# 4. Random Forest Modell trainieren (ohne CV, ohne Tuning)
# -------------------------------------------------------
set.seed(123)
model_rf <- randomForest(y ~ ., data = train_data, importance = TRUE)

print(model_rf)

# -------------------------------------------------------
# 5. Vorhersagen auf Testdaten
# -------------------------------------------------------
# Klassenvorhersage
pred_class <- predict(model_rf, test_data)

# Wahrscheinlichkeiten (für "pos")
pred_prob <- predict(model_rf, test_data, type = "prob")[, "pos"]

# -------------------------------------------------------
# 6. Confusion Matrix & Standardmetriken
# -------------------------------------------------------
cm <- confusionMatrix(pred_class, test_data$y, positive = "pos")

accuracy <- cm$overall["Accuracy"]
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Recall"]
f1 <- cm$byClass["F1"]

# -------------------------------------------------------
# 7. ROC & AUC (explizit pROC:: verwenden)
# -------------------------------------------------------
roc_obj <- roc(response = test_data$y, predictor = pred_prob)
auc_value <- pROC::auc(roc_obj)
plot(roc_obj, col = "blue", main = "ROC Curve")

# -------------------------------------------------------
# 8. Log Loss (manuell berechnet)
# -------------------------------------------------------
eps <- 1e-15  # zur Vermeidung von log(0)
pred_prob_clipped <- pmin(pmax(pred_prob, eps), 1 - eps)
y_true <- as.numeric(test_data$y == "pos")
logloss_value <- -mean(y_true * log(pred_prob_clipped) + (1 - y_true) * log(1 - pred_prob_clipped))

# -------------------------------------------------------
# 9. Brier Score
# -------------------------------------------------------
brier_score <- mean((y_true - pred_prob)^2)

# -------------------------------------------------------
# 10. Calibration Curve
# -------------------------------------------------------
calibration_df <- data.frame(
  prob = pred_prob,
  actual = y_true
) %>%
  mutate(prob_bin = cut(prob, breaks = seq(0, 1, by = 0.1))) %>%
  group_by(prob_bin) %>%
  summarise(
    mean_pred = mean(prob),
    mean_obs  = mean(actual),
    .groups = "drop"
  )

ggplot(calibration_df, aes(x = mean_pred, y = mean_obs)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    x = "Predicted Probability",
    y = "Observed Frequency",
    title = "Calibration Curve"
  ) +
  coord_equal()

# -------------------------------------------------------
# 11. Alle Metriken zusammenfassen
# -------------------------------------------------------
cat("📊 Evaluation auf Testdaten:\n")
cat("-------------------------------------\n")
cat("Accuracy    :", round(accuracy, 3), "\n")
cat("Precision   :", round(precision, 3), "\n")
cat("Recall      :", round(recall, 3), "\n")
cat("F1 Score    :", round(f1, 3), "\n")
cat("AUC         :", round(auc_value, 3), "\n")
cat("Log Loss    :", round(logloss_value, 3), "\n")
cat("Brier Score :", round(brier_score, 3), "\n")

saveRDS(model_rf, file = "model_rf.rds")

metrics_df <- data.frame(
  Accuracy     = accuracy,
  Precision    = precision,
  Recall       = recall,
  F1_Score     = f1,
  AUC          = as.numeric(auc_value),
  Log_Loss     = logloss_value,
  Brier_Score  = brier_score
)

write.csv(metrics_df, "model_metrics.csv", row.names = FALSE)
