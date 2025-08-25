# -------------------------------------------------------
# Pakete laden
# -------------------------------------------------------
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
# Hilfsfunktion: Modell definieren
# -------------------------------------------------------
build_model <- function(input_dim) {
  model <- keras_model_sequential(input_shape = input_dim) %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dropout(rate = 0.1) %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.001),
    loss = "binary_crossentropy",
    metrics = "accuracy"
  )
  return(model)
}

# -------------------------------------------------------
# Modell trainieren (Train/Test wie bisher)
# -------------------------------------------------------
model <- build_model(ncol(x_train))

reduce_lr <- callback_reduce_lr_on_plateau(
  monitor = "val_loss", factor = 0.2, patience = 2, verbose = 1
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 30,
  batch_size = 32,
  validation_split = 0.2,
  callbacks = list(reduce_lr),
  verbose = 0
)

cat("Neuronales Netz wurde erfolgreich trainiert.\n")

# -------------------------------------------------------
# Vorhersagen Train/Test
# -------------------------------------------------------
prob_train <- as.array(predict(model, x_train))[,1]
prob_test  <- as.array(predict(model, x_test))[,1]

pred_class_train <- factor(ifelse(prob_train >= 0.5, "Spieler 1", "Spieler 2"), levels = c("Spieler 2", "Spieler 1"))
pred_class_test  <- factor(ifelse(prob_test >= 0.5, "Spieler 1", "Spieler 2"), levels = c("Spieler 2", "Spieler 1"))

# -------------------------------------------------------
# Auswertung Train/Test
# -------------------------------------------------------
cm_train <- caret::confusionMatrix(pred_class_train, y_train_factor, positive = "Spieler 1")
cm_test  <- caret::confusionMatrix(pred_class_test,  y_test_factor,  positive = "Spieler 1")

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

# -------------------------------------------------------
# Cross Validation (10-fold)
# -------------------------------------------------------
set.seed(123)
folds <- caret::createFolds(y_train_factor, k = 10, returnTrain = TRUE)

cv_results <- lapply(seq_along(folds), function(i) {
  cat("Fold", i, "von 10\n")
  
  train_idx <- folds[[i]]
  x_tr <- x_train[train_idx, ]
  y_tr <- y_train[train_idx]
  x_val <- x_train[-train_idx, ]
  y_val <- y_train[-train_idx]
  y_val_factor <- y_train_factor[-train_idx]
  
  model_cv <- build_model(ncol(x_train))
  model_cv %>% fit(x_tr, y_tr, epochs = 30, batch_size = 32, verbose = 0)
  
  prob_val <- as.array(predict(model_cv, x_val))[,1]
  pred_val <- factor(ifelse(prob_val >= 0.5, "Spieler 1", "Spieler 2"), levels = c("Spieler 2", "Spieler 1"))
  
  cm_val <- caret::confusionMatrix(pred_val, y_val_factor, positive = "Spieler 1")
  roc_val <- pROC::roc(response = y_val_factor, predictor = prob_val, levels = c("Spieler 2","Spieler 1"))
  auc_val <- as.numeric(pROC::auc(roc_val))
  
  eps <- 1e-15
  pp <- pmin(pmax(prob_val, eps), 1 - eps)
  y01 <- as.numeric(y_val_factor == "Spieler 1")
  logloss <- -mean(y01 * log(pp) + (1 - y01) * log(1 - pp))
  brier <- mean((y01 - prob_val)^2)
  
  c(Accuracy=cm_val$overall["Accuracy"],
    Precision=cm_val$byClass["Precision"],
    Recall=cm_val$byClass["Recall"],
    F1_Score=cm_val$byClass["F1"],
    AUC=auc_val,
    Log_Loss=logloss,
    Brier_Score=brier)
})

cv_metrics <- do.call(rbind, cv_results)
cv_summary <- data.frame(
  Metric = colnames(cv_metrics),
  Mean = round(colMeans(cv_metrics), 3),
  SD   = round(apply(cv_metrics, 2, sd), 3)
)

# -------------------------------------------------------
# Bootstrapping (50 Resamples, OOB)
# -------------------------------------------------------
set.seed(123)
boot_idx <- caret::createResample(y_train_factor, times = 50)

boot_results <- lapply(seq_along(boot_idx), function(i) {
  cat("Bootstrap", i, "von 50\n")
  
  train_idx <- boot_idx[[i]]
  x_boot <- x_train[train_idx, ]
  y_boot <- y_train[train_idx]
  
  oob_idx <- setdiff(seq_len(nrow(x_train)), unique(train_idx))
  if (length(oob_idx) < 10) return(NULL)
  
  x_oob <- x_train[oob_idx, ]
  y_oob <- y_train[oob_idx]
  y_oob_factor <- y_train_factor[oob_idx]
  
  model_boot <- build_model(ncol(x_train))
  model_boot %>% fit(x_boot, y_boot, epochs = 30, batch_size = 32, verbose = 0)
  
  prob_oob <- as.array(predict(model_boot, x_oob))[,1]
  pred_oob <- factor(ifelse(prob_oob >= 0.5, "Spieler 1", "Spieler 2"), levels = c("Spieler 2", "Spieler 1"))
  
  cm_oob <- caret::confusionMatrix(pred_oob, y_oob_factor, positive = "Spieler 1")
  roc_oob <- pROC::roc(response = y_oob_factor, predictor = prob_oob, levels = c("Spieler 2","Spieler 1"))
  auc_oob <- as.numeric(pROC::auc(roc_oob))
  
  eps <- 1e-15
  pp <- pmin(pmax(prob_oob, eps), 1 - eps)
  y01 <- as.numeric(y_oob_factor == "Spieler 1")
  logloss <- -mean(y01 * log(pp) + (1 - y01) * log(1 - pp))
  brier <- mean((y01 - prob_oob)^2)
  
  c(Accuracy=cm_oob$overall["Accuracy"],
    Precision=cm_oob$byClass["Precision"],
    Recall=cm_oob$byClass["Recall"],
    F1_Score=cm_oob$byClass["F1"],
    AUC=auc_oob,
    Log_Loss=logloss,
    Brier_Score=brier)
})

boot_metrics <- do.call(rbind, boot_results)
boot_summary <- data.frame(
  Metric = colnames(boot_metrics),
  Mean = round(colMeans(boot_metrics, na.rm = TRUE), 3),
  SD   = round(apply(boot_metrics, 2, sd, na.rm = TRUE), 3)
)

# -------------------------------------------------------
# Ergebnisse speichern
# -------------------------------------------------------
out_dir <- "Models/NNs/NN_Tuned2"
plot_dir <- "Models/NNs/NN_Tuned2/plots"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# Train/Test Ergebnisse
metrics_train_test %>%
  tibble::rownames_to_column("Metric") %>%
  readr::write_csv(file.path(out_dir, "metrics_train_vs_test_nn.csv"))

# CV-Ergebnisse
readr::write_csv(cv_summary, file.path(out_dir, "metrics_cv10_nn.csv"))

# Bootstrap-Ergebnisse
readr::write_csv(boot_summary, file.path(out_dir, "metrics_bootstrap_nn.csv"))

cat("Alle Ergebnisse erfolgreich gespeichert in:", out_dir, "\n")

# -------------------------------------------------------
# Kombinierte Übersichtstabelle
# -------------------------------------------------------

# Test-Metriken aus Train/Test Ergebnis
metrics_test_df <- data.frame(
  Metric = rownames(metrics_train_test),
  Test = as.numeric(metrics_train_test$Test)
)

# Cross Validation Mittelwerte
cv_df <- cv_summary %>% select(Metric, Mean) %>% rename(CV10_Mean = Mean)

# Bootstrap Mittelwerte
boot_df <- boot_summary %>% select(Metric, Mean) %>% rename(Bootstrap_Mean = Mean)

# Zusammenführen
combined_results <- metrics_test_df %>%
  left_join(cv_df, by = "Metric") %>%
  left_join(boot_df, by = "Metric")

# Rundung
combined_results <- combined_results %>%
  mutate(across(-Metric, ~round(., 3)))

# Speichern
readr::write_csv(combined_results, file.path(out_dir, "metrics_combined_nn.csv"))

cat("Kombinierte Übersichtstabelle gespeichert in:", file.path(out_dir, "metrics_combined_nn.csv"), "\n")
