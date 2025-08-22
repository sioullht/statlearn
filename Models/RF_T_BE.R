# -------------------------------------------------------
# Pakete
# -------------------------------------------------------
packages <- c("tidyverse","caret","pROC","randomForest","ggplot2","doParallel")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

# -------------------------------------------------------
# Daten laden & Zielvariable
# -------------------------------------------------------
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

train_data$y <- factor(train_data$y, levels = c(0,1), labels = c("neg","pos"))
test_data$y  <- factor(test_data$y,  levels = c(0,1), labels = c("neg","pos"))

# -------------------------------------------------------
# Parallelisierung
# -------------------------------------------------------
n_cores <- max(1, parallel::detectCores() - 1)
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

# -------------------------------------------------------
# TrainControl für CV
# -------------------------------------------------------
ctrl_cv <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  allowParallel = TRUE
)

# -------------------------------------------------------
# 1) Backward Elimination via RFE
# -------------------------------------------------------
p <- ncol(train_data) - 1L
sizes <- unique(sort(c(5, 10, 20, 30, 50, 75, 100, floor(seq(0.2*p, p, length.out = 5)))))

rfe_ctrl <- rfeControl(
  functions = caretFuncs,      # erlaubt metric = "ROC"
  method    = "cv",
  number    = 5,
  verbose   = FALSE,
  allowParallel = TRUE
)

set.seed(123)
rfe_fit <- rfe(
  x = subset(train_data, select = -y),
  y = train_data$y,
  sizes = sizes,
  rfeControl = rfe_ctrl,
  trControl  = ctrl_cv,
  metric     = "ROC",
  method     = "rf",
  tuneGrid   = expand.grid(mtry = max(1, floor(sqrt(p)))),
  ntree      = 500
)

print(rfe_fit)
best_vars <- predictors(rfe_fit)
cat("Ausgewählte Features:", length(best_vars), "\n")

# -------------------------------------------------------
# 2) Dein RF-Workflow, aber nur mit den RFE-Features
# -------------------------------------------------------
rf_grid <- expand.grid(
  mtry = floor(seq(2, sqrt(length(best_vars))*2, length.out = 6))
)

set.seed(123)
model_rf_tuned <- train(
  x = subset(train_data, select = all_of(best_vars)),
  y = train_data$y,
  method    = "rf",
  trControl = ctrl_cv,
  tuneGrid  = rf_grid,
  metric    = "ROC",
  ntree     = 500,
  nodesize  = 5,
  maxnodes  = 32,
  importance= TRUE
)

print(model_rf_tuned)

# -------------------------------------------------------
# Predictions: TRAIN & TEST
# -------------------------------------------------------
pred_train_prob  <- predict(model_rf_tuned, subset(train_data, select = all_of(best_vars)), type = "prob")[, "pos"]
pred_train_class <- predict(model_rf_tuned, subset(train_data, select = all_of(best_vars)))

pred_test_prob   <- predict(model_rf_tuned, subset(test_data, select = all_of(best_vars)), type = "prob")[, "pos"]
pred_test_class  <- predict(model_rf_tuned, subset(test_data, select = all_of(best_vars)))

# -------------------------------------------------------
# ROC / AUC
# -------------------------------------------------------
roc_train <- pROC::roc(train_data$y, pred_train_prob, levels = c("neg","pos"), direction = "<")
auc_train <- as.numeric(pROC::auc(roc_train))

roc_test  <- pROC::roc(test_data$y,  pred_test_prob,  levels = c("neg","pos"), direction = "<")
auc_test  <- as.numeric(pROC::auc(roc_test))

# -------------------------------------------------------
# Confusion Matrices
# -------------------------------------------------------
cm_train <- caret::confusionMatrix(pred_train_class, train_data$y, positive = "pos")
cm_test  <- caret::confusionMatrix(pred_test_class,  test_data$y,  positive = "pos")

# -------------------------------------------------------
# Metriken (inkl. LogLoss & Brier)
# -------------------------------------------------------
eps <- 1e-15
clip <- function(p) pmin(pmax(p, eps), 1 - eps)

pp_tr  <- clip(pred_train_prob)
y01_tr <- as.numeric(train_data$y == "pos")
logloss_tr <- -mean(y01_tr * log(pp_tr) + (1 - y01_tr) * log(1 - pp_tr))
brier_tr   <- mean((y01_tr - pred_train_prob)^2)

pp_te  <- clip(pred_test_prob)
y01_te <- as.numeric(test_data$y == "pos")
logloss_te <- -mean(y01_te * log(pp_te) + (1 - y01_te) * log(1 - pp_te))
brier_te   <- mean((y01_te - pred_test_prob)^2)

metrics_train <- c(
  Accuracy    = cm_train$overall["Accuracy"],
  Precision   = cm_train$byClass["Precision"],
  Recall      = cm_train$byClass["Recall"],
  F1_Score    = cm_train$byClass["F1"],
  AUC         = auc_train,
  Log_Loss    = logloss_tr,
  Brier_Score = brier_tr
)

metrics_test <- c(
  Accuracy    = cm_test$overall["Accuracy"],
  Precision   = cm_test$byClass["Precision"],
  Recall      = cm_test$byClass["Recall"],
  F1_Score    = cm_test$byClass["F1"],
  AUC         = auc_test,
  Log_Loss    = logloss_te,
  Brier_Score = brier_te
)

metrics_train_test <- rbind(Train = metrics_train, Test = metrics_test) %>% as.data.frame()
print(round(metrics_train_test, 4))

# -------------------------------------------------------
# Speicherung
# -------------------------------------------------------
out_dir  <- "Models/RF_RFE_T_BE"
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# Confusion Matrices
capture.output(cm_train, file = file.path(out_dir, "confusion_matrix_train.txt"))
capture.output(cm_test,  file = file.path(out_dir, "confusion_matrix_test.txt"))

# Metriken speichern
metrics_train_test %>%
  tibble::rownames_to_column("Split") %>%
  readr::write_csv(file.path(out_dir, "metrics_train_vs_test.csv"))

# ROC-Plots
p_train <- pROC::ggroc(roc_train) +
  ggplot2::ggtitle(sprintf("ROC (TRAIN) — AUC = %.3f", auc_train)) +
  ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train.pdf"), plot = p_train, width = 6, height = 5)

p_test <- pROC::ggroc(roc_test) +
  ggplot2::ggtitle(sprintf("ROC (TEST) — AUC = %.3f", auc_test)) +
  ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Test.pdf"), plot = p_test, width = 6, height = 5)

p_both <- pROC::ggroc(list(Train = roc_train, Test = roc_test)) +
  ggplot2::ggtitle(sprintf("ROC — Train (AUC=%.3f) vs. Test (AUC=%.3f)", auc_train, auc_test)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(linetype = "Split", color = "Split")
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train_vs_Test.pdf"), plot = p_both, width = 6, height = 5)

# Modell speichern
saveRDS(model_rf_tuned, file.path(out_dir, "model_rf_tuned.rds"))

# Cluster stoppen
parallel::stopCluster(cl)
foreach::registerDoSEQ()

message("Fertig. Ergebnisse unter: ", normalizePath(out_dir))