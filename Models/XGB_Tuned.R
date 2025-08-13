# ==== Pakete ====
packages <- c("tidyverse","caret","pROC","xgboost","scales","Matrix","tictoc","doParallel")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
lapply(packages, library, character.only = TRUE)

# ==== Daten ====
train_data <- read_csv("Step3/train_data.csv")
test_data  <- read_csv("Step3/test_data.csv")

train_data$y <- factor(train_data$y, levels = c(0,1), labels = c("neg","pos"))
test_data$y  <- factor(test_data$y,  levels = c(0,1), labels = c("neg","pos"))

set.seed(123)

# ==== (optional) Parallelisierung ====
# Nutzt alle Kerne minus 1 für die Resamples (Caret)
cores <- max(1, parallel::detectCores() - 1)
cl <- makeCluster(cores)
registerDoParallel(cl)
on.exit({ try(stopCluster(cl), silent=TRUE); registerDoSEQ() }, add = TRUE)

# ==== CV-Setup: 5x3 Repeats, OOF-Preds speichern + Logging ====
control <- trainControl(
  method = "repeatedcv", number = 5, repeats = 3,
  classProbs = TRUE, summaryFunction = twoClassSummary,
  savePredictions = "final",      # für Kalibrierung (OOF)
  verboseIter = TRUE,             # <-- Caret-Fortschritt (Fold/Repeat/Tuning)
  returnData  = FALSE,
  allowParallel = TRUE
)

# ==== 1) Breites & feines Grid definieren ====
xgb_grid_full <- expand.grid(
  nrounds = c(200, 400, 800, 1200),
  max_depth = c(3, 5, 7, 9),
  eta = c(0.01, 0.03, 0.05, 0.1),
  gamma = c(0, 0.5, 1, 2),
  colsample_bytree = c(0.5, 0.7, 0.9, 1.0),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(0.6, 0.8, 1.0)
)

# ==== 2) Random Search: zufällige Teilmenge aus großem Grid ====
n_samples <- 80L                     # anpassen je nach Budget
set.seed(123)
xgb_grid_rand <- xgb_grid_full[sample(nrow(xgb_grid_full), n_samples), ]

# ==== Hilfsfunktion für Zeitformat ====
fmt_dur <- function(sec) {
  if (!is.finite(sec)) return("n/a")
  if (sec < 90) sprintf("%.0f s", sec)
  else if (sec < 3600) sprintf("%.1f min", sec/60)
  else sprintf("%.2f h", sec/3600)
}

# ==== Pilotlauf für ETA-Schätzung ============================================
pilot_rows <- min(5L, nrow(xgb_grid_rand))
xgb_grid_pilot <- xgb_grid_rand[seq_len(pilot_rows), , drop = FALSE]

control_pilot <- trainControl(
  method = "repeatedcv", number = 2, repeats = 1,
  classProbs = TRUE, summaryFunction = twoClassSummary,
  verboseIter = FALSE, returnData = FALSE, allowParallel = TRUE
)

tic("Pilotlauf")
invisible(
  train(
    y ~ ., data = train_data,
    method    = "xgbTree",
    trControl = control_pilot,
    tuneGrid  = xgb_grid_pilot,
    metric    = "ROC",
    verbose = 0, print_every_n = 0,
    nthread = 1                      # wichtig: Caret parallelisiert über Resamples
  )
)
pilot_time <- toc(quiet = TRUE)$toc - toc(quiet = TRUE)$tic

fits_pilot <- nrow(xgb_grid_pilot) * (2 * 1)
fits_full  <- nrow(xgb_grid_rand)  * (5 * 3)

avg_rounds_pilot <- mean(xgb_grid_pilot$nrounds)
avg_rounds_full  <- mean(xgb_grid_rand$nrounds)
scale_rounds <- if (is.finite(avg_rounds_pilot) && avg_rounds_pilot > 0)
  avg_rounds_full / avg_rounds_pilot else 1

eta_sec <- pilot_time * (fits_full / fits_pilot) * scale_rounds
message(sprintf(
  "\n[ETA] Pilot dauerte %s für %d Fits. Erwartete Gesamtdauer: ~ %s für %d Fits (mit %d Kernen).\n",
  fmt_dur(pilot_time), fits_pilot, fmt_dur(eta_sec), fits_full, cores
))

# ==== Training mit laufendem Fortschritt ======================================
tic("Volles Training")
model_xgb <- train(
  y ~ ., data = train_data,
  method    = "xgbTree",
  trControl = control,                 # verboseIter=TRUE -> Caret-Progress
  tuneGrid  = xgb_grid_rand,
  metric    = "ROC",
  verbose = 1,                         # XGBoost-Logs einschalten
  print_every_n = 25,                  # alle 25 Boosting-Runden
  nthread = 1                           # Caret übernimmt die Parallelisierung
)
train_time <- toc(quiet = TRUE)$toc - toc(quiet = TRUE)$tic
message(sprintf("[DONE] Tatsächliche Dauer: %s\n", fmt_dur(train_time)))

print(model_xgb)
plot(model_xgb)

# ==== Evaluation (ungekalibriert) ============================================
pred_prob_test  <- predict(model_xgb, test_data, type = "prob")[,"pos"]
pred_class_test <- factor(ifelse(pred_prob_test >= 0.5, "pos", "neg"),
                          levels = c("neg","pos"))

roc_obj <- roc(test_data$y, pred_prob_test)
auc_val <- auc(roc_obj)
cm <- confusionMatrix(pred_class_test, test_data$y, positive = "pos")
logloss <- -mean(as.numeric(test_data$y=="pos")*log(pmax(pmin(pred_prob_test,1-1e-15),1e-15)) +
                 (1 - as.numeric(test_data$y=="pos"))*log(pmax(pmin(1-pred_prob_test,1-1e-15),1e-15)))
brier <- mean((as.numeric(test_data$y=="pos") - pred_prob_test)^2)

# ==== Kalibrierung (Platt Scaling) mit OOF-Predictions ========================
# OOF-Preds passend zur besten Tuning-Kombi extrahieren
bt <- model_xgb$bestTune
pred_tbl <- model_xgb$pred
for (nm in names(bt)) {
  pred_tbl <- pred_tbl %>% dplyr::filter(.data[[nm]] == bt[[nm]])
}
oof <- pred_tbl %>% dplyr::select(rowIndex, pred = pos, obs)

# Reihenfolge sichern
oof <- oof[order(oof$rowIndex), ]
y_oof <- factor(train_data$y[oof$rowIndex], levels = c("neg","pos"))
glm_cal <- glm((y_oof=="pos") ~ oof$pred, family = binomial())
pred_prob_test_cal <- predict(glm_cal, newdata = data.frame(`oof$pred` = pred_prob_test), type = "response")

# Schwellenwert-Optimierung (Youden) auf Test-ROC der kalibrierten Probs
roc_cal <- roc(test_data$y, pred_prob_test_cal)
thr <- coords(roc_cal, "best", ret = "threshold", best.method = "youden")
pred_class_test_cal <- factor(ifelse(pred_prob_test_cal >= as.numeric(thr), "pos", "neg"),
                              levels = c("neg","pos"))

cm_cal <- confusionMatrix(pred_class_test_cal, test_data$y, positive = "pos")
logloss_cal <- -mean(as.numeric(test_data$y=="pos")*log(pmax(pmin(pred_prob_test_cal,1-1e-15),1e-15)) +
                     (1 - as.numeric(test_data$y=="pos"))*log(pmax(pmin(1-pred_prob_test_cal,1-1e-15),1e-15)))
brier_cal <- mean((as.numeric(test_data$y=="pos") - pred_prob_test_cal)^2)

metrics_xgb <- data.frame(
  Variant = c("raw","calibrated"),
  Accuracy = c(cm$overall["Accuracy"], cm_cal$overall["Accuracy"]),
  Precision = c(cm$byClass["Precision"], cm_cal$byClass["Precision"]),
  Recall = c(cm$byClass["Recall"], cm_cal$byClass["Recall"]),
  F1_Score = c(cm$byClass["F1"], cm_cal$byClass["F1"]),
  AUC = c(auc_val, auc(roc_cal)),
  Log_Loss = c(logloss, logloss_cal),
  Brier_Score = c(brier, brier_cal),
  Threshold = c(0.5, as.numeric(thr))
)

# ==== Speichern ===============================================================
dir.create("Models/XGB_Tuned_Caret", recursive = TRUE, showWarnings = FALSE)
saveRDS(model_xgb, "Models/XGB_Tuned_Caret/model_xgb.rds")
saveRDS(glm_cal,   "Models/XGB_Tuned_Caret/platt_glm.rds")
write.csv(metrics_xgb, "Models/XGB_Tuned_Caret/metrics_xgb.csv", row.names = FALSE)
