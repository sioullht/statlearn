# =========================
# ROC + AUC aus gespeich. Modell
# =========================
# Pakete
need <- c("pROC","xgboost","ggplot2","readr")
for (p in need) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(pROC); library(xgboost); library(ggplot2); library(readr)

set.seed(123)

# ---- Pfade (anpassen, falls nötig) ----
model_path <- "XGB/model_xgb.rds"
train_csv  <- "Step3/train_data.csv"
test_csv   <- "Step3/test_data.csv"
plot_dir   <- "XGB/plots"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Daten laden ----
train <- read_csv(train_csv)
test  <- read_csv(test_csv)

# Zielvariable (hier: Spalte 'y' mit 0/1-Labels)
y_train <- as.numeric(train$y == 1)
y_test  <- as.numeric(test$y == 1)
X_train <- as.matrix(subset(train, select = -y))
X_test  <- as.matrix(subset(test,  select = -y))

dtrain <- xgb.DMatrix(X_train, label = y_train)
dtest  <- xgb.DMatrix(X_test,  label = y_test)

# ---- Modell laden ----
xgb_model <- readRDS(model_path)

# ---- Vorhersage-Wahrscheinlichkeiten ----
p_train <- predict(xgb_model, dtrain)  # P(y=1)
p_test  <- predict(xgb_model, dtest)

# ---- ROC + AUC ----
roc_tr <- roc(response = y_train, predictor = p_train, quiet = TRUE)
roc_te <- roc(response = y_test,  predictor = p_test,  quiet = TRUE)
auc_tr <- as.numeric(auc(roc_tr))
auc_te <- as.numeric(auc(roc_te))

# ---- Plots ----
p_train_plot <- ggroc(roc_tr) +
  ggtitle(sprintf("ROC (TRAIN) — AUC = %.3f", auc_tr)) +
  theme_minimal()

p_test_plot <- ggroc(roc_te) +
  ggtitle(sprintf("ROC (TEST) — AUC = %.3f", auc_te)) +
  theme_minimal()

p_both_plot <- ggroc(list(Train = roc_tr, Test = roc_te)) +
  ggtitle(sprintf("ROC — Train (AUC = %.3f) vs. Test (AUC = %.3f)", auc_tr, auc_te)) +
  theme_minimal() +
  labs(color = "Split", linetype = "Split")

# ---- Speichern ----
ggsave(file.path(plot_dir, "ROC_Train.pdf"),        p_train_plot, width = 6, height = 5)
ggsave(file.path(plot_dir, "ROC_Test.pdf"),         p_test_plot,  width = 6, height = 5)
ggsave(file.path(plot_dir, "ROC_Train_vs_Test.pdf"),p_both_plot,  width = 6, height = 5)

cat("Fertig. Plots unter:", normalizePath(plot_dir), "\n")
cat(sprintf("AUC Train = %.3f | AUC Test = %.3f\n", auc_tr, auc_te))
