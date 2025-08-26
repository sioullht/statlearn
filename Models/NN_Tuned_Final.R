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
if (!file.exists("Step3/train_data.csv") || !file.exists("Step3/test_data.csv")) {
  stop("Stellen Sie sicher, dass 'train_data.csv' und 'test_data.csv' im Ordner 'Step3' vorhanden sind.")
}
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

if (!"y" %in% names(train_data) || !"y" %in% names(test_data)) {
  stop("Die Zielvariable 'y' wurde in den Daten nicht gefunden.")
}

y_train_factor <- factor(train_data$y, levels = c(0, 1), labels = c("Spieler_2", "Spieler_1"))
y_test_factor  <- factor(test_data$y,  levels = c(0, 1), labels = c("Spieler_2", "Spieler_1"))

x_train_df <- train_data[, -which(names(train_data) == "y")]
x_test_df  <- test_data[, -which(names(test_data) == "y")]

# Umwandlung in Matrizen für Keras
x_train <- as.matrix(x_train_df)
x_test  <- as.matrix(x_test_df)
y_train <- as.matrix(train_data$y)
y_test <- as.matrix(test_data$y)


# -------------------------------------------------------
# Ordner für Ergebnisse definieren
# -------------------------------------------------------
out_dir <- "Models/NNs/NN_Tuned"
plot_dir <- file.path(out_dir, "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)


# -------------------------------------------------------
# Hyperparameter-Tuning mit Grid Search
# -------------------------------------------------------
cat("\nStarte Hyperparameter-Tuning mit Grid Search...\n")

# 1. Tuning-Raster definieren
tune_grid <- expand.grid(
  units1 = c(128, 64),
  units2 = c(64, 32),
  dropout1 = c(0.2, 0.4),
  learning_rate = c(0.001, 0.0005)
)

tuning_results <- list()
set.seed(123)
cv_folds <- createFolds(y_train_factor, k = 5, returnTrain = TRUE)

# 2. Schleife über das Raster
for (i in 1:nrow(tune_grid)) {
  params <- tune_grid[i, ]
  cv_auc_scores <- c()
  
  cat(sprintf("Teste Kombination %d/%d: units1=%d, units2=%d, dropout1=%.2f, lr=%.4f\n", 
              i, nrow(tune_grid), params$units1, params$units2, params$dropout1, params$learning_rate))
  
  # 3. Kreuzvalidierung für jede Kombination
  for (j in seq_along(cv_folds)) {
    train_idx <- cv_folds[[j]]
    x_tr <- x_train[train_idx, ]
    y_tr <- y_train[train_idx]
    x_val <- x_train[-train_idx, ]
    y_val_factor <- y_train_factor[-train_idx]
    
    model_tune <- keras_model_sequential(input_shape = ncol(x_tr)) %>%
      layer_dense(units = params$units1, activation = "relu") %>%
      layer_dropout(rate = params$dropout1) %>%
      layer_dense(units = params$units2, activation = "relu") %>%
      layer_dropout(rate = 0.2) %>%
      layer_dense(units = 1, activation = "sigmoid")
    
    model_tune %>% compile(
      optimizer = optimizer_adam(learning_rate = params$learning_rate),
      loss = "binary_crossentropy",
      metrics = "accuracy"
    )
    
    model_tune %>% fit(x_tr, y_tr, epochs = 15, batch_size = 32, verbose = 0)
    
    prob_val <- as.array(predict(model_tune, x_val))[,1]
    roc_val <- pROC::roc(response = y_val_factor, predictor = prob_val, levels = c("Spieler_2","Spieler_1"), quiet = TRUE)
    cv_auc_scores <- c(cv_auc_scores, as.numeric(pROC::auc(roc_val)))
  }
  
  tuning_results[[i]] <- c(params, mean_auc = mean(cv_auc_scores))
}

tuning_summary <- do.call(rbind, lapply(tuning_results, as.data.frame))
cat("\nTuning-Ergebnisse:\n")
print(tuning_summary)

# 5. Beste Kombination finden
best_params <- tuning_summary[which.max(tuning_summary$mean_auc), ]
cat("\nBeste Hyperparameter:\n")
print(best_params)

# -------------------------------------------------------
# Finales Modell mit den besten Hyperparametern trainieren
# -------------------------------------------------------
cat("\nTrainiere finales Modell mit den besten Hyperparametern...\n")

build_final_model <- function(input_dim, params) {
  model <- keras_model_sequential(input_shape = input_dim) %>%
    layer_dense(units = params$units1, activation = "relu") %>%
    layer_dropout(rate = params$dropout1) %>%
    layer_dense(units = params$units2, activation = "relu") %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = params$learning_rate),
    loss = "binary_crossentropy",
    metrics = "accuracy"
  )
  return(model)
}

final_model <- build_final_model(ncol(x_train), best_params)

reduce_lr <- callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.2, patience = 3, verbose = 1)
history <- final_model %>% fit(
  x_train, y_train,
  epochs = 40,
  batch_size = 32,
  validation_split = 0.2,
  callbacks = list(reduce_lr),
  verbose = 0
)

cat("Finales neuronales Netz wurde erfolgreich trainiert.\n")

# -------------------------------------------------------
# Modell und Ergebnisse speichern
# -------------------------------------------------------
save_model(final_model, file.path(out_dir, "nn_model_final_tuned.keras"))
readr::write_csv(tuning_summary, file.path(out_dir, "tuning_summary.csv"))
cat("Getuntes Modell und Tuning-Ergebnisse wurden gespeichert in:", out_dir, "\n")

# -------------------------------------------------------
# FINALE AUSWERTUNG: Train/Test
# -------------------------------------------------------
cat("\nFühre finale Auswertung auf Train/Test-Daten durch...\n")
prob_train <- as.array(predict(final_model, x_train))[,1]
prob_test  <- as.array(predict(final_model, x_test))[,1]

levels(y_train_factor) <- make.names(levels(y_train_factor))
levels(y_test_factor) <- make.names(levels(y_test_factor))
pred_class_train <- factor(ifelse(prob_train >= 0.5, "Spieler_1", "Spieler_2"), levels = c("Spieler_2", "Spieler_1"))
pred_class_test  <- factor(ifelse(prob_test >= 0.5, "Spieler_1", "Spieler_2"), levels = c("Spieler_2", "Spieler_1"))

cm_train <- caret::confusionMatrix(pred_class_train, y_train_factor, positive = "Spieler_1")
cm_test  <- caret::confusionMatrix(pred_class_test,  y_test_factor,  positive = "Spieler_1")

roc_train <- pROC::roc(response = y_train_factor, predictor = prob_train, levels = c("Spieler_2", "Spieler_1"))
roc_test  <- pROC::roc(response = y_test_factor,  predictor = prob_test,  levels = c("Spieler_2", "Spieler_1"))
auc_train <- as.numeric(pROC::auc(roc_train))
auc_test  <- as.numeric(pROC::auc(roc_test))

eps <- 1e-15
pp_tr <- pmin(pmax(prob_train, eps), 1 - eps)
y01_tr <- as.numeric(y_train_factor == "Spieler_1")
logloss_tr <- -mean(y01_tr * log(pp_tr) + (1 - y01_tr) * log(1 - pp_tr))
brier_tr <- mean((y01_tr - prob_train)^2)

pp_te <- pmin(pmax(prob_test, eps), 1 - eps)
y01_te <- as.numeric(y_test_factor == "Spieler_1")
logloss_te <- -mean(y01_te * log(pp_te) + (1 - y01_te) * log(1 - pp_te))
brier_te <- mean((y01_te - prob_test)^2)

metrics_train <- c(Accuracy=cm_train$overall["Accuracy"], Precision=cm_train$byClass["Precision"], Recall=cm_train$byClass["Recall"], F1_Score=cm_train$byClass["F1"], AUC=auc_train, Log_Loss=logloss_tr, Brier_Score=brier_tr)
metrics_test <- c(Accuracy=cm_test$overall["Accuracy"], Precision=cm_test$byClass["Precision"], Recall=cm_test$byClass["Recall"], F1_Score=cm_test$byClass["F1"], AUC=auc_test, Log_Loss=logloss_te, Brier_Score=brier_te)
metrics_train_test <- round(data.frame(Train = metrics_train, Test = metrics_test), 3)

# -------------------------------------------------------
# FINALE AUSWERTUNG: Cross Validation zur Validierung
# -------------------------------------------------------
cat("\nFühre finale 10-fache Kreuzvalidierung zur Validierung durch...\n")
set.seed(123)
final_folds <- caret::createFolds(y_train_factor, k = 10, returnTrain = TRUE)

cv_results <- lapply(seq_along(final_folds), function(i) {
  cat("Fold", i, "von 10\n")
  
  train_idx <- final_folds[[i]]
  x_tr <- x_train[train_idx, ]
  y_tr <- y_train[train_idx]
  x_val <- x_train[-train_idx, ]
  y_val_factor <- y_train_factor[-train_idx]
  
  model_cv <- build_final_model(ncol(x_train), best_params)
  model_cv %>% fit(x_tr, y_tr, epochs = 40, batch_size = 32, verbose = 0)
  
  prob_val <- as.array(predict(model_cv, x_val))[,1]
  pred_val <- factor(ifelse(prob_val >= 0.5, "Spieler_1", "Spieler_2"), levels = c("Spieler_2", "Spieler_1"))
  
  cm_val <- caret::confusionMatrix(pred_val, y_val_factor, positive = "Spieler_1")
  roc_val <- pROC::roc(response = y_val_factor, predictor = prob_val, levels = c("Spieler_2","Spieler_1"))
  auc_val <- as.numeric(pROC::auc(roc_val))
  
  eps <- 1e-15
  pp <- pmin(pmax(prob_val, eps), 1 - eps)
  y01 <- as.numeric(y_val_factor == "Spieler_1")
  logloss <- -mean(y01 * log(pp) + (1 - y01) * log(1 - pp))
  brier <- mean((y01 - prob_val)^2)
  
  c(Accuracy=cm_val$overall["Accuracy"], Precision=cm_val$byClass["Precision"], Recall=cm_val$byClass["Recall"], F1_Score=cm_val$byClass["F1"], AUC=auc_val, Log_Loss=logloss, Brier_Score=brier)
})

cv_metrics <- do.call(rbind, cv_results)
cv_summary <- data.frame(
  Metric = colnames(cv_metrics),
  Mean = round(colMeans(cv_metrics), 3),
  SD   = round(apply(cv_metrics, 2, sd), 3)
)

# -------------------------------------------------------
# Finale Ergebnisse speichern
# -------------------------------------------------------
metrics_train_test %>%
  tibble::rownames_to_column("Metric") %>%
  readr::write_csv(file.path(out_dir, "metrics_train_vs_test_final.csv"))

readr::write_csv(cv_summary, file.path(out_dir, "metrics_cv10_final.csv"))

# Kombinierte Übersichtstabelle
metrics_test_df <- data.frame(Metric = rownames(metrics_train_test), Test = as.numeric(metrics_train_test$Test))
cv_df <- cv_summary %>% select(Metric, Mean) %>% rename(CV10_Mean = Mean)
combined_results <- metrics_test_df %>% left_join(cv_df, by = "Metric") %>% mutate(across(-Metric, ~round(., 3)))
readr::write_csv(combined_results, file.path(out_dir, "metrics_combined_final.csv"))

cat("\nAlle finalen Auswertungen erfolgreich gespeichert in:", out_dir, "\n")
cat("Skript vollständig ausgeführt.\n")

