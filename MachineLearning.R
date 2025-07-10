# ----------------------------
# Modelltraining + Cross-Validation + Learning Curve + Export
# ----------------------------

# Pakete installieren (nur beim ersten Mal nötig)
if (!require("randomForest")) install.packages("randomForest")
if (!require("caret")) install.packages("caret")
if (!require("ggplot2")) install.packages("ggplot2")

# Pakete laden
library(randomForest)
library(caret)
library(ggplot2)

# ----------------------------
cat("📥 Lese Daten ein...\n")
df <- read.csv("ATP_final_FE.csv")
df$y <- as.factor(df$y)  # Zielvariable als Faktor
cat("✅ Daten erfolgreich eingelesen.\n")

# ----------------------------
# Klassisches Random Forest Modell
# ----------------------------
cat("\n🌲 Starte Training Random Forest Modell...\n")

set.seed(42)
sample_index <- sample(nrow(df), 0.7 * nrow(df))  
train <- df[sample_index, ]
test  <- df[-sample_index, ]

model_rf <- randomForest(y ~ ., data = train, ntree = 200, importance = TRUE)
cat("✅ Modelltraining abgeschlossen.\n")

# Vorhersagen
cat("🔍 Berechne Vorhersagen auf Testdaten...\n")
preds <- predict(model_rf, newdata = test)

# Accuracy
accuracy <- mean(preds == test$y)
cat("🎯 Accuracy (Test):", round(accuracy * 100, 2), "%\n")

# Confusion Matrix
cat("📊 Erstelle Confusion Matrix...\n")
cm <- table(Predicted = preds, Actual = test$y)
print(cm)

# Feature Importance
cat("📌 Feature Importance (Top-Variablen):\n")
print(importance(model_rf))

# ----------------------------
# Learning Curve Plot
# ----------------------------
cat("\n📉 Berechne Learning Curve...\n")

learning_curve <- function(df, fractions = seq(0.1, 1, 0.1)) {
  results <- data.frame(Fraction = numeric(), Accuracy = numeric())
  
  for (f in fractions) {
    cat(paste0("  ▶ Trainiere mit ", round(f * 100), "% der Daten...\n"))
    set.seed(42)
    idx <- sample(nrow(df), f * nrow(df))
    train_sub <- df[idx, ]
    test_sub <- df[-idx, ]
    
    model <- randomForest(y ~ ., data = train_sub, ntree = 100)
    preds <- predict(model, newdata = test_sub)
    acc <- mean(preds == test_sub$y)
    
    results <- rbind(results, data.frame(Fraction = round(f, 2), Accuracy = acc))
  }
  
  return(results)
}

lc_data <- learning_curve(df)
cat("✅ Learning Curve fertig berechnet.\n")

# Plotten
cat("📊 Zeige Learning Curve...\n")
print(
  ggplot(lc_data, aes(x = Fraction, y = Accuracy)) +
    geom_line(color = "blue") +
    geom_point() +
    ggtitle("Learning Curve") +
    ylab("Test Accuracy") +
    xlab("Trainingsdatenanteil") +
    theme_minimal()
)

# ----------------------------
# Ergebnisse speichern 
# ----------------------------
cat("\n💾 Speichere Ergebnisse...\n")

write.csv(cm, "confusion_matrix.csv", row.names = TRUE)
cat("✅ Confusion Matrix gespeichert: confusion_matrix.csv\n")

write.csv(lc_data, "learning_curve.csv", row.names = FALSE)
cat("✅ Learning Curve gespeichert: learning_curve.csv\n")

save(model_rf, file = "rf_model.RData")
cat("✅ Modell gespeichert: rf_model.RData\n")

cat("\n🚀 Skript erfolgreich abgeschlossen!\n")