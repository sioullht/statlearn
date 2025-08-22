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
model <- keras_model_sequential(input_shape = ncol(x_train)) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = "accuracy"
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 32,
  validation_split = 0.2,
  verbose = 0
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

# -------------------------------------------------------
# Ergebnisse speichern
# -------------------------------------------------------
out_dir <- "NN"
plot_dir <- "NN/plots"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

capture.output(cm_test, file = file.path(out_dir, "confusion_matrix_test.txt"))

# --- DATEI 1: CSV mit Train vs. Test Metriken (wie bisher) ---
metrics_train_test %>%
  tibble::rownames_to_column("Metric") %>%
  readr::write_csv(file.path(out_dir, "metrics_train_vs_test_nn.csv"))

# --- DATEI 2 (NEU): CSV nur mit den Test-Metriken ---
# Erstelle einen sauberen Dataframe nur für die Test-Ergebnisse
metrics_test_df <- round(data.frame(Value = metrics_test), 3)
metrics_test_df %>%
  tibble::rownames_to_column("Metric") %>%
  readr::write_csv(file.path(out_dir, "metrics_test_only_nn.csv"))
  
# Plots (unverändert)
p_train <- pROC::ggroc(roc_train) + ggplot2::ggtitle(sprintf("ROC (TRAIN) — AUC = %.3f", auc_train)) + ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train_NN.pdf"), plot = p_train, width = 6, height = 5)

p_test <- pROC::ggroc(roc_test) + ggplot2::ggtitle(sprintf("ROC (TEST) — AUC = %.3f", auc_test)) + ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Test_NN.pdf"), plot = p_test, width = 6, height = 5)

p_both <- pROC::ggroc(list(Train = roc_train, Test = roc_test)) +
  ggplot2::ggtitle(sprintf("ROC Neuronales Netz — Train (AUC=%.3f) vs. Test (AUC=%.3f)", auc_train, auc_test)) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_discrete(name = "Datensatz")
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Compare_NN.pdf"), plot = p_both, width = 7, height = 5)

cat(sprintf("Auswertung erfolgreich abgeschlossen. Zwei CSV-Dateien wurden in '%s' gespeichert.\n", out_dir))