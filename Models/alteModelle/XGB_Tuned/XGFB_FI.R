# ================== Top-k-Feature-Run (kompatibel zu deinem caret-Modell) ==================
packages <- c("tidyverse","caret","pROC","xgboost","Matrix","ggplot2")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg, dependencies = TRUE)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

# Pfade wie in deinem Projekt
model_dir  <- "Models/XGB_Tuned"
model_rds  <- file.path(model_dir, "model_xgb_tuned.rds")

train_csv  <- "Step3/train_data.csv"
test_csv   <- "Step3/test_data.csv"

out_dir    <- file.path(model_dir, "TopK")     # neuer Ordner für Top-k-Variante
plot_dir   <- file.path(out_dir, "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

target     <- "y"
top_k      <- 10   # <— hier die Anzahl der Top-Features einstellen
# ==========================================================================================

# Daten laden & Zielvariable wie im Hauptskript
train_df <- readr::read_csv(train_csv, show_col_types = FALSE)
test_df  <- readr::read_csv(test_csv,  show_col_types = FALSE)
train_df[[target]] <- factor(train_df[[target]], levels=c(0,1), labels=c("neg","pos"))
test_df [[target]] <- factor(test_df [[target]], levels=c(0,1), labels=c("neg","pos"))

# altes caret-Trainobjekt laden
xgb_obj <- readRDS(model_rds)

# finaler Booster + bestTune
stopifnot(!is.null(xgb_obj$finalModel))
booster <- xgb_obj$finalModel
best_tune <- xgb_obj$bestTune  # data.frame mit Spalten: nrounds, max_depth, eta, gamma, colsample_bytree, min_child_weight, subsample

# Top-k-Features nach Gain aus finalem Booster
imp <- xgboost::xgb.importance(model = booster)
top_feats <- head(imp[order(-imp$Gain), "Feature", drop = TRUE], top_k)

# Nur diese Features (falls vorhanden)
keep <- intersect(top_feats, setdiff(names(train_df), target))
if (length(keep) == 0) stop("Keine der Top-Features im Datensatz gefunden.")
message(sprintf("Verwende %d Top-Features: %s", length(keep), paste(keep, collapse=", ")))

# Optionale Klassenbalance wie zuvor
pos_rate <- mean(train_df[[target]] == "pos"); neg_rate <- 1 - pos_rate
scale_pos_w <- neg_rate / pos_rate
if (!is.finite(scale_pos_w) || abs(scale_pos_w - 1) < 0.2) scale_pos_w <- 1

# Gleiche CV-Einstellungen, aber OHNE GridSearch (wir nehmen exakt bestTune)
control_topk <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  allowParallel = TRUE
)

# Ein-Zeilen-Grid = exakt dein bestTune
topk_grid <- data.frame(
  nrounds = best_tune$nrounds,
  max_depth = best_tune$max_depth,
  eta = best_tune$eta,
  gamma = best_tune$gamma,
  colsample_bytree = best_tune$colsample_bytree,
  min_child_weight = best_tune$min_child_weight,
  subsample = best_tune$subsample
)

# Training mit denselben Preprocess-Schritten
form_topk <- as.formula(paste(target, "~", paste(keep, collapse = " + ")))

model_xgb_topk <- caret::train(
  form_topk,
  data       = train_df,
  method     = "xgbTree",
  trControl  = control_topk,
  tuneGrid   = topk_grid,
  metric     = "ROC",
  verbose    = FALSE,
  preProcess = c("YeoJohnson","center","scale","nzv"),
  scale_pos_weight = scale_pos_w
)

print(model_xgb_topk)

# --- Bewertung auf TEST (RAW + optionale einfache Platt-Kalibrierung aus OOF) ---
# 1) OOF der Topk-Variante holen (für Kalibrierung wie bei dir)
bt2 <- model_xgb_topk$bestTune
oof_full2 <- model_xgb_topk$pred
oof_best2 <- subset(
  oof_full2,
  nrounds==bt2$nrounds & max_depth==bt2$max_depth & eta==bt2$eta &
    gamma==bt2$gamma & colsample_bytree==bt2$colsample_bytree &
    min_child_weight==bt2$min_child_weight & subsample==bt2$subsample
)

oof_df2 <- data.frame(
  y = factor(oof_best2$obs, levels=c("neg","pos")),
  oof = oof_best2$pos
)
cal_glm2 <- glm(I(y=="pos") ~ oof, data=oof_df2, family=binomial())
calibrate_probs2 <- function(p) predict(cal_glm2, newdata = data.frame(oof = p), type = "response")

# 2) Vorhersagen
y01_te <- as.numeric(test_df[[target]] == "pos")
pred_test_prob_raw <- predict(model_xgb_topk, test_df, type="prob")[, "pos"]
pred_test_prob_cal <- calibrate_probs2(pred_test_prob_raw)

# 3) Metriken (AUC, LogLoss, Brier)
clip <- function(p, eps=1e-15) pmin(pmax(p, eps), 1-eps)
logloss <- function(y, p) { p <- clip(p); -mean(y*log(p) + (1-y)*log(1-p)) }
brier   <- function(y, p) mean((y - p)^2)

roc_raw <- pROC::roc(test_df[[target]], pred_test_prob_raw, levels=c("neg","pos"), direction="<", quiet=TRUE)
roc_cal <- pROC::roc(test_df[[target]], pred_test_prob_cal, levels=c("neg","pos"), direction="<", quiet=TRUE)

auc_raw <- as.numeric(pROC::auc(roc_raw))
auc_cal <- as.numeric(pROC::auc(roc_cal))
ll_raw  <- logloss(y01_te, pred_test_prob_raw)
ll_cal  <- logloss(y01_te, pred_test_prob_cal)
br_raw  <- brier(y01_te, pred_test_prob_raw)
br_cal  <- brier(y01_te, pred_test_prob_cal)

metrics_topk <- tibble::tibble(
  Split   = "Test",
  Model   = "XGB_Tuned",
  Variant = sprintf("Top%d", length(keep)),
  AUC_RAW = auc_raw,
  LogLoss_RAW = ll_raw,
  Brier_RAW   = br_raw,
  AUC_CAL = auc_cal,
  LogLoss_CAL = ll_cal,
  Brier_CAL   = br_cal
)

readr::write_csv(metrics_topk, file.path(out_dir, sprintf("metrics_Test_Top%d.csv", length(keep))))
saveRDS(model_xgb_topk, file.path(out_dir, sprintf("model_xgb_top%d.rds", length(keep))))

# Plots (ROC RAW/CAL)
p_raw <- pROC::ggroc(roc_raw) + ggplot2::ggtitle(sprintf("ROC RAW (Top-%d) — AUC = %.3f", length(keep), auc_raw)) + ggplot2::theme_minimal()
p_cal <- pROC::ggroc(roc_cal) + ggplot2::ggtitle(sprintf("ROC CAL (Top-%d) — AUC = %.3f", length(keep), auc_cal)) + ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, sprintf("ROC_Test_RAW_Top%d.pdf", length(keep))), plot = p_raw, width = 6, height = 5)
ggplot2::ggsave(filename = file.path(plot_dir, sprintf("ROC_Test_CAL_Top%d.pdf", length(keep))), plot = p_cal, width = 6, height = 5)

message("Fertig. Ergebnisse unter: ", normalizePath(out_dir))