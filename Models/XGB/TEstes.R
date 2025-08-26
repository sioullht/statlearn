# =========================
# ROC + AUC aus gespeichertem XGBoost-Modell
# =========================

# Pakete laden/ installieren
need <- c("pROC", "xgboost", "ggplot2", "readr")
for (p in need) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(pROC); library(xgboost); library(ggplot2); library(readr)

set.seed(123)

# ---- Pfade ----
model_path <- "Models/XGB/model_xgb.rds"   # anpassen, falls nötig
train_csv  <- "Step3/train_data.csv"
test_csv   <- "Step3/test_data.csv"

# Plots landen im selben Ordner wie das Modell
plot_dir <- file.path(dirname(model_path), "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Daten laden ----
train <- read_csv(train_csv, show_col_types = FALSE)
test  <- read_csv(test_csv,  show_col_types = FALSE)

# ---- Zielvariable & Feature-Matrizen ----
stopifnot("y" %in% names(train), "y" %in% names(test))
y_train <- as.numeric(train$y == 1)
y_test  <- as.numeric(test$y == 1)

X_train <- as.matrix(subset(train, select = -y))
X_test  <- as.matrix(subset(test,  select = -y))

# ---- Modell laden ----
if (!file.exists(model_path)) {
  stop(sprintf("Modell nicht gefunden: %s\nArbeitsverzeichnis: %s",
               model_path, getwd()))
}
xgb_model <- readRDS(model_path)

# ---- Vorhersage-Wahrscheinlichkeiten ----
# wichtig: explizit xgboost::predict und Matrix übergeben
p_train <- xgboost::predict(xgb_model, newdata = X_train)  # P(y=1)
p_test  <- xgboost::predict(xgb_model, newdata = X_test)

# ---- ROC + AUC ----
roc_tr <- pROC::roc(response = y_train, predictor = p_train, quiet = TRUE)
roc_te <- pROC::roc(response = y_test,  predictor = p_test,  quiet = TRUE)
auc_tr <- as.numeric(pROC::auc(roc_tr))
auc_te <- as.numeric(pROC::auc(roc_te))

# ---- Plots ----
diag_line <- geom_abline(slope = -1, intercept = 1, linetype = "dashed", alpha = 0.5)

p_train_plot <- pROC::ggroc(roc_tr) +
  diag_line +
  ggtitle(sprintf("ROC (TRAIN) — AUC = %.3f", auc_tr)) +
  theme_minimal()

p_test_plot <- pROC::ggroc(roc_te) +
  diag_line +
  ggtitle(sprintf("ROC (TEST) — AUC = %.3f", auc_te)) +
  theme_minimal()

p_both_plot <- pROC::ggroc(list(Train = roc_tr, Test = roc_te)) +
  diag_line +
  ggtitle(sprintf("ROC — Train (AUC = %.3f) vs. Test (AUC = %.3f)", auc_tr, auc_te)) +
  theme_minimal() +
  labs(color = "Split", linetype = "Split")

# ---- Speichern ----
ggsave(file.path(plot_dir, "ROC_Train.pdf"),         p_train_plot, width = 6, height = 5)
ggsave(file.path(plot_dir, "ROC_Test.pdf"),          p_test_plot,  width = 6, height = 5)
ggsave(file.path(plot_dir, "ROC_Train_vs_Test.pdf"), p_both_plot,  width = 6, height = 5)

# Optional zusätzlich als PNG
ggsave(file.path(plot_dir, "ROC_Train.png"),         p_train_plot, width = 6, height = 5, dpi = 300)
ggsave(file.path(plot_dir, "ROC_Test.png"),          p_test_plot,  width = 6, height = 5, dpi = 300)
ggsave(file.path(plot_dir, "ROC_Train_vs_Test.png"), p_both_plot,  width = 6, height = 5, dpi = 300)

cat("Fertig. Plots unter:\n", normalizePath(plot_dir), "\n")
cat(sprintf("AUC  Train = %.3f\nAUC  Test  = %.3f\n", auc_tr, auc_te))