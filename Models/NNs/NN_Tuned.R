# -------------------------------------------------------
# Pakete (jetzt mit keras3)
# -------------------------------------------------------
# Stelle sicher, dass alle notwendigen Pakete installiert sind
packages <- c("keras3", "readr", "tidyverse", "caret", "pROC", "ggplot2")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

# -------------------------------------------------------
# Daten laden & vorbereiten
# -------------------------------------------------------
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

x_train <- as.matrix(train_data[, -which(names(train_data) == "y")])
y_train <- as.matrix(train_data$y)

x_test <- as.matrix(test_data[, -which(names(test_data) == "y")])
y_test <- as.matrix(test_data$y)

y_train_factor <- factor(train_data$y, levels = c(0, 1), labels = c("Spieler 2", "Spieler 1"))
y_test_factor  <- factor(test_data$y,  levels = c(0, 1), labels = c("Spieler 2", "Spieler 1"))

# -------------------------------------------------------
# Neuronales Netz mit keras3 definieren und trainieren
# -------------------------------------------------------
# 1. Modell-Definition
model <- keras_model_sequential(input_shape = ncol(x_train)) %>%
  layer_dense(units = 128, activation = "relu") %>% # Erste Ebene
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 64, activation = "relu") %>%  # Zweite Ebene
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 32, activation = "relu") %>%  # Dritte Ebene
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = "sigmoid")   # Output-Schicht

# 2. Kompilieren (optional mit angepasster Lernrate)
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "binary_crossentropy",
  metrics = "accuracy"
)


# Callbacks: Early Stopping + ReduceLROnPlateau (deins behalten)
early_stop <- callback_early_stopping(
  monitor = "val_loss",
  patience = 5,
  restore_best_weights = TRUE
)

# Callback für die Lernraten-Anpassung definieren
reduce_lr <- callback_reduce_lr_on_plateau(
  monitor = "val_loss", # Überwacht den Validierungsverlust
  factor = 0.2,         # Reduziert die LR um den Faktor 0.2 (also auf 20%)
  patience = 2,         # Reduziert, wenn es 2 Epochen keine Verbesserung gab
  verbose = 1
)

# 4. Training mit Callback
history <- model %>% fit(
  x_train, y_train,
  epochs = 60,
  batch_size = 64,
  validation_split = 0.2,
  callbacks = list(early_stop, reduce_lr),
  verbose = 1
)

cat("Neuronales Netz wurde erfolgreich trainiert.\n")

# -------------------------------------------------------
# Vorhersagen für die Auswertung generieren
# -------------------------------------------------------
prob_train <- as.array(predict(model, x_train))[,1]
prob_test  <- as.array(predict(model, x_test))[,1]

pred_class_train <- factor(ifelse(prob_train >= 0.5, "Spieler 1", "Spieler 2"), levels = c("Spieler 2", "Spieler 1"))
pred_class_test  <- factor(ifelse(prob_test >= 0.5, "Spieler 1", "Spieler 2"), levels = c("Spieler 2", "Spieler 1"))

# -------------------------------------------------------
# Auswertung mit caret und pROC
# -------------------------------------------------------
cm_train <- caret::confusionMatrix(data = pred_class_train, reference = y_train_factor, positive = "Spieler 1")
cm_test  <- caret::confusionMatrix(data = pred_class_test,  reference = y_test_factor,  positive = "Spieler 1")

roc_train <- pROC::roc(response = y_train_factor, predictor = prob_train, levels = c("Spieler 2", "Spieler 1"))
roc_test  <- pROC::roc(response = y_test_factor,  predictor = prob_test,  levels = c("Spieler 2", "Spieler 1"))
auc_train <- as.numeric(pROC::auc(roc_train))
auc_test  <- as.numeric(pROC::auc(roc_test))

eps <- 1e-15
pp_tr <- pmin(pmax(prob_train, eps), 1 - eps)
y01_tr <- as.numeric(y_train_factor == "Spieler 1")
logloss_tr <- -mean(y01_tr * log(pp_tr) + (1 - y01_tr) * log(1 - pp_tr))
brier_tr <- mean((y01_tr - prob_train)^2)

pp_te <- pmin(pmax(prob_test, eps), 1 - eps)
y01_te <- as.numeric(y_test_factor == "Spieler 1")
logloss_te <- -mean(y01_te * log(pp_te) + (1 - y01_te) * log(1 - pp_te))
brier_te <- mean((y01_te - prob_test)^2)

metrics_train <- c(Accuracy=cm_train$overall["Accuracy"], Precision=cm_train$byClass["Precision"], Recall=cm_train$byClass["Recall"], F1_Score=cm_train$byClass["F1"], AUC=auc_train, Log_Loss=logloss_tr, Brier_Score=brier_tr)
metrics_test <- c(Accuracy=cm_test$overall["Accuracy"], Precision=cm_test$byClass["Precision"], Recall=cm_test$byClass["Recall"], F1_Score=cm_test$byClass["F1"], AUC=auc_test, Log_Loss=logloss_te, Brier_Score=brier_te)
metrics_train_test <- round(data.frame(Train = metrics_train, Test = metrics_test), 3)

print("Umfassende Metriken (Train vs. Test):")
print(metrics_train_test)

