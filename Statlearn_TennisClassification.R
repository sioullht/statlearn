#Data Preprocessing
library(dplyr)
library(readr)

#Datem einlesen
df <- read_csv("atp_matches_till_2022.csv")

spalten <- c(
  'tourney_date', 'tourney_name', 'surface', 
  'winner_id', 'winner_name', 'winner_hand', 'winner_ht', 'winner_ioc', 'winner_age',
  'loser_id', 'loser_name', 'loser_hand', 'loser_ht', 'loser_ioc', 'loser_age', 
  'score', 'best_of', 'minutes', 
  'w_ace', 'w_df', 'w_svpt', 'w_1stIn', 'w_1stWon', 'w_2ndWon', 'w_bpSaved', 'w_bpFaced', 'w_SvGms',
  'l_ace', 'l_df', 'l_svpt', 'l_1stIn', 'l_1stWon', 'l_2ndWon', 'l_bpSaved', 'l_bpFaced', 'l_SvGms',
  'winner_rank', 'loser_rank', 'winner_rank_points', 'loser_rank_points'
)

df <- df %>% select(any_of(spalten))


df$tourney_date <- as.numeric(df$tourney_date)
df$year <- df$tourney_date %/% 10000

#Daten auf Zeitraum 2000–2020 beschränken
df <- df %>%
  filter(year >= 2000 & year <= 2020) %>%
  select(-year)

#Zufällige Zuordnung von Spielern
set.seed(123)

df <- df %>%
  mutate(rand = runif(n()),
         player1_name = ifelse(rand < 0.5, winner_name, loser_name),
         player2_name = ifelse(rand < 0.5, loser_name, winner_name)) %>%
  transmute(
    player1_id = ifelse(player1_name == winner_name, winner_id, loser_id),
    player2_id = ifelse(player2_name == winner_name, winner_id, loser_id),

    player1_name,
    player2_name,

    player1_ioc = ifelse(player1_name == winner_name, winner_ioc, loser_ioc),
    player2_ioc = ifelse(player2_name == winner_name, winner_ioc, loser_ioc),

    player1_age = ifelse(player1_name == winner_name, winner_age, loser_age),
    player2_age = ifelse(player2_name == winner_name, winner_age, loser_age),

    player1_ht = ifelse(player1_name == winner_name, winner_ht, loser_ht),
    player2_ht = ifelse(player2_name == winner_name, winner_ht, loser_ht),

    player1_hand = ifelse(player1_name == winner_name, winner_hand, loser_hand),
    player2_hand = ifelse(player2_name == winner_name, winner_hand, loser_hand),

    player1_ace = ifelse(player1_name == winner_name, w_ace, l_ace),
    player2_ace = ifelse(player2_name == winner_name, w_ace, l_ace),

    player1_df = ifelse(player1_name == winner_name, w_df, l_df),
    player2_df = ifelse(player2_name == winner_name, w_df, l_df),

    player1_svpt = ifelse(player1_name == winner_name, w_svpt, l_svpt),
    player2_svpt = ifelse(player2_name == winner_name, w_svpt, l_svpt),

    player1_1stin = ifelse(player1_name == winner_name, w_1stIn, l_1stIn),
    player2_1stin = ifelse(player2_name == winner_name, w_1stIn, l_1stIn),

    player1_1stwon = ifelse(player1_name == winner_name, w_1stWon, l_1stWon),
    player2_1stwon = ifelse(player2_name == winner_name, w_1stWon, l_1stWon),

    player1_2ndwon = ifelse(player1_name == winner_name, w_2ndWon, l_2ndWon),
    player2_2ndwon = ifelse(player2_name == winner_name, w_2ndWon, l_2ndWon),

    player1_bpsaved = ifelse(player1_name == winner_name, w_bpSaved, l_bpSaved),
    player2_bpsaved = ifelse(player2_name == winner_name, w_bpSaved, l_bpSaved),

    player1_bpfaced = ifelse(player1_name == winner_name, w_bpFaced, l_bpFaced),
    player2_bpfaced = ifelse(player2_name == winner_name, w_bpFaced, l_bpFaced),

    player1_svgms = ifelse(player1_name == winner_name, w_SvGms, l_SvGms),
    player2_svgms = ifelse(player2_name == winner_name, w_SvGms, l_SvGms),

    player1_rank = ifelse(player1_name == winner_name, winner_rank, loser_rank),
    player2_rank = ifelse(player2_name == winner_name, winner_rank, loser_rank),

    player1_rank_pts = ifelse(player1_name == winner_name, winner_rank_points, loser_rank_points),
    player2_rank_pts = ifelse(player2_name == winner_name, winner_rank_points, loser_rank_points),

    surface = surface,
    tourney_name = tourney_name,
    date = tourney_date,
    best_of = best_of,
    minutes = minutes,

    y = ifelse(player1_name == winner_name, 1, 0)
  )

df <- df %>%
  na.omit() %>%
  select(-player1_name, -player2_name)

write_csv(df, "ATP_ViLo_.csv")








#Beginn Feature Engineering 
library(tidyverse)
library(dplyr)

df <- read_csv("ATP_ViLo_.csv")
df <- df %>%
  mutate(
    p1_ace_percent     = (player1_ace / player1_svpt),
    p1_doublefault_percent      = (player1_df / player1_svpt),
    p1_1in_percent     = (player1_1stin / player1_svpt),
    p1_1won_percent    = ifelse(player1_1stin > 0, (player1_1stwon / player1_1stin), 0),
    p1_serve_win_percent  = ((player1_1stwon + player1_2ndwon) / player1_svpt),
    p1_breakp_saved = ifelse(player1_bpfaced > 0, (player1_bpsaved / player1_bpfaced), NA),
    p1_breakp_saved  = ifelse(player1_svgms > 0, player1_bpfaced / player1_svgms, NA),
    p1_breakp_succeed   = ifelse(player1_bpfaced > 0, 1 - (player1_bpsaved / player1_bpfaced), NA),

    p2_ace_percent     = (player2_ace / player2_svpt),
    p2_doublefault_percent      = (player2_df / player2_svpt),
    p2_1in_percent     = (player2_1stin / player2_svpt),
    p2_1won_percent    = ifelse(player2_1stin > 0, (player2_1stwon / player2_1stin), 0),
    p2_serve_win_percent  = ((player2_1stwon + player2_2ndwon) / player2_svpt),
    p2_breakp_saved = ifelse(player2_bpfaced > 0, (player2_bpsaved / player2_bpfaced), NA),
    p2_breakp_saved  = ifelse(player2_svgms > 0, player2_bpfaced / player2_svgms, NA),
    p2_breakp_succeed   = ifelse(player2_bpfaced > 0, 1 - (player2_bpsaved / player2_bpfaced), NA),

    ace_diff           = (player1_ace / player1_svpt) - (player2_ace / player2_svpt),
    df_diff            = (player1_df / player1_svpt) - (player2_df / player2_svpt),
    in1_pct_diff       = (player1_1stin / player1_svpt) - (player2_1stin / player2_svpt),
    won1_pct_diff      = ifelse(player1_1stin > 0, player1_1stwon / player1_1stin, 0) -
                         ifelse(player2_1stin > 0, player2_1stwon / player2_1stin, 0),
    sv_win_pct_diff    = ((player1_1stwon + player1_2ndwon) / player1_svpt) -
                         ((player2_1stwon + player2_2ndwon) / player2_svpt),
    bp_saved_pct_diff  = ifelse(player1_bpfaced > 0, player1_bpsaved / player1_bpfaced, NA) -
                         ifelse(player2_bpfaced > 0, player2_bpsaved / player2_bpfaced, NA),
    bp_per_game_diff   = ifelse(player1_svgms > 0, player1_bpfaced / player1_svgms, NA) -
                         ifelse(player2_svgms > 0, player2_bpfaced / player2_svgms, NA),

    
    # Logarithmischer Unterschied Ränge 
    log_rank_diff = log(player2_rank) - log(player1_rank),
    
    #Unterschied der Ranglistenpunkte
    rankpoints_diff = player1_rank_pts - player2_rank_pts,
    
    #One-Hot Encoding für player1_hand
    p1_hand_R = ifelse(player1_hand == 'R', 1, 0),
    p1_hand_L = ifelse(player1_hand == 'L', 1, 0),
    p1_hand_U = ifelse(player1_hand == 'U', 1, 0),

    #One-Hot Encoding für player2_hand
    p2_hand_R = ifelse(player2_hand == 'R', 1, 0),
    p2_hand_L = ifelse(player2_hand == 'L', 1, 0),
    p2_hand_U = ifelse(player2_hand == 'U', 1, 0)
  ) %>%

  #ursprünglichen 'hand'-Spalten entfernen
  select(-player1_hand, -player2_hand)

write.csv(df, "FE_Step1.csv", row.names = FALSE)



























#Machine Learning Models

#RF
packages <- c("tidyverse","caret","pROC","randomForest","ggplot2")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)


#Daten laden & Zielvariable
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

train_data$y <- factor(train_data$y, levels = c(0,1), labels = c("neg","pos"))
test_data$y  <- factor(test_data$y,  levels = c(0,1), labels = c("neg","pos"))

p <- ncol(train_data) - 1L
mtry_fixed <- max(1L, floor(sqrt(p)))   

ctrl_none <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

model_rf <- train(
  y ~ ., data = train_data,
  method = "rf",
  trControl = ctrl_none,
  tuneGrid  = data.frame(mtry = mtry_fixed),  
  metric    = "ROC",
  ntree     = 500,
  nodesize  = 5,     
  maxnodes  = 32,    
  importance = TRUE
)

print(model_rf)

pred_train_prob  <- predict(model_rf, train_data, type = "prob")[, "pos"]
pred_train_class <- predict(model_rf, train_data)

pred_test_prob   <- predict(model_rf, test_data,  type = "prob")[, "pos"]
pred_test_class  <- predict(model_rf, test_data)


#ROC / AUC
roc_train <- pROC::roc(train_data$y, pred_train_prob, levels = c("neg","pos"), direction = "<")
auc_train <- as.numeric(pROC::auc(roc_train))

roc_test  <- pROC::roc(test_data$y,  pred_test_prob,  levels = c("neg","pos"), direction = "<")
auc_test  <- as.numeric(pROC::auc(roc_test))

cm_train <- caret::confusionMatrix(pred_train_class, train_data$y, positive = "pos")
cm_test  <- caret::confusionMatrix(pred_test_class,  test_data$y,  positive = "pos")

eps <- 1e-15

pp_tr  <- pmin(pmax(pred_train_prob, eps), 1 - eps)
y01_tr <- as.numeric(train_data$y == "pos")
logloss_tr <- -mean(y01_tr * log(pp_tr) + (1 - y01_tr) * log(1 - pp_tr))
brier_tr   <- mean((y01_tr - pred_train_prob)^2)

pp_te  <- pmin(pmax(pred_test_prob, eps), 1 - eps)
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


#Speichern
out_dir  <- "Models/RF"
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

capture.output(cm_train, file = file.path(out_dir, "confusion_matrix_train.txt"))
capture.output(cm_test,  file = file.path(out_dir, "confusion_matrix_test.txt"))

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

saveRDS(model_rf, file.path(out_dir, "model_rf.rds"))
message("Fertig. Alle Ergebnisse unter: ", normalizePath(out_dir))


#RF Tuned 
packages <- c("tidyverse","caret","pROC","randomForest","ggplot2","doParallel")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

# Daten laden & Zielvariable
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

train_data$y <- factor(train_data$y, levels = c(0,1), labels = c("neg","pos"))
test_data$y  <- factor(test_data$y,  levels = c(0,1), labels = c("neg","pos"))


n_cores <- max(1, parallel::detectCores() - 1)
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

# Cross Validation
ctrl_cv <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  allowParallel = TRUE
)

# Grid für mtry 
p <- ncol(train_data) - 1L
rf_grid <- expand.grid(
  mtry = floor(seq(2, sqrt(p)*2, length.out = 6)) # z.B. 6 Werte
)

# Training RF mit CV + GridSearch
set.seed(123)
model_rf_tuned <- train(
  y ~ ., data = train_data,
  method = "rf",
  trControl = ctrl_cv,
  tuneGrid  = rf_grid,
  metric    = "ROC",
  ntree     = 500,
  nodesize  = 5,     
  maxnodes  = 32,    
  importance = TRUE
)

print(model_rf_tuned)

pred_train_prob  <- predict(model_rf_tuned, train_data, type = "prob")[, "pos"]
pred_train_class <- predict(model_rf_tuned, train_data)

pred_test_prob   <- predict(model_rf_tuned, test_data,  type = "prob")[, "pos"]
pred_test_class  <- predict(model_rf_tuned, test_data)

# ROC / AUC
roc_train <- pROC::roc(train_data$y, pred_train_prob, levels = c("neg","pos"), direction = "<")
auc_train <- as.numeric(pROC::auc(roc_train))

roc_test  <- pROC::roc(test_data$y,  pred_test_prob,  levels = c("neg","pos"), direction = "<")
auc_test  <- as.numeric(pROC::auc(roc_test))

# Confusion Matrices
cm_train <- caret::confusionMatrix(pred_train_class, train_data$y, positive = "pos")
cm_test  <- caret::confusionMatrix(pred_test_class,  test_data$y,  positive = "pos")

# Metriken 
eps <- 1e-15

pp_tr  <- pmin(pmax(pred_train_prob, eps), 1 - eps)
y01_tr <- as.numeric(train_data$y == "pos")
logloss_tr <- -mean(y01_tr * log(pp_tr) + (1 - y01_tr) * log(1 - pp_tr))
brier_tr   <- mean((y01_tr - pred_train_prob)^2)

pp_te  <- pmin(pmax(pred_test_prob, eps), 1 - eps)
y01_te <- as.numeric(test_data$y == "pos")
logloss_te <- -mean(y01_te * log(pp_te) + (1 - pp_te) * log(1 - pp_te))
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

#Speichern
out_dir  <- "Models/RF_Tuned"
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

capture.output(cm_train, file = file.path(out_dir, "confusion_matrix_train.txt"))
capture.output(cm_test,  file = file.path(out_dir, "confusion_matrix_test.txt"))

metrics_train_test %>%
  tibble::rownames_to_column("Split") %>%
  readr::write_csv(file.path(out_dir, "metrics_train_vs_test.csv"))

#ROC-Plots
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

#Modell speichern
saveRDS(model_rf_tuned, file.path(out_dir, "model_rf_tuned.rds"))

parallel::stopCluster(cl)
foreach::registerDoSEQ()

message("Fertig. Alle Ergebnisse unter: ", normalizePath(out_dir))

#XGB 
packages <- c("tidyverse", "caret", "pROC", "xgboost", "scales", "Matrix", "ggplot2")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

# Daten laden & Zielvariable
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

train_data$y <- factor(train_data$y, levels = c(0,1), labels = c("neg","pos"))
test_data$y  <- factor(test_data$y,  levels = c(0,1), labels = c("neg","pos"))

#Model
ctrl_none <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

#Hyperparameter
base_grid <- data.frame(
  nrounds = 200,
  max_depth = 6,
  eta = 0.1,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

model_xgb <- train(
  y ~ ., data = train_data,
  method = "xgbTree",
  trControl = ctrl_none,
  tuneGrid = base_grid,
  metric = "ROC",
  verbose = FALSE
)

print(model_xgb)

pred_train_prob  <- predict(model_xgb, train_data, type = "prob")[, "pos"]
pred_train_class <- predict(model_xgb, train_data)

pred_test_prob   <- predict(model_xgb, test_data,  type = "prob")[, "pos"]
pred_test_class  <- predict(model_xgb, test_data)

#ROC / AUC
roc_train <- pROC::roc(train_data$y, pred_train_prob, levels = c("neg","pos"), direction = "<")
auc_train <- as.numeric(pROC::auc(roc_train))

roc_test  <- pROC::roc(test_data$y,  pred_test_prob,  levels = c("neg","pos"), direction = "<")
auc_test  <- as.numeric(pROC::auc(roc_test))

cm_train <- caret::confusionMatrix(pred_train_class, train_data$y, positive = "pos")
cm_test  <- caret::confusionMatrix(pred_test_class,  test_data$y,  positive = "pos")

#Metriken
eps <- 1e-15

pp_tr  <- pmin(pmax(pred_train_prob, eps), 1 - eps)
y01_tr <- as.numeric(train_data$y == "pos")
logloss_tr <- -mean(y01_tr * log(pp_tr) + (1 - y01_tr) * log(1 - pp_tr))
brier_tr   <- mean((y01_tr - pred_train_prob)^2)

pp_te  <- pmin(pmax(pred_test_prob, eps), 1 - eps)
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

out_dir  <- "Models/XGB"
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

capture.output(cm_train, file = file.path(out_dir, "confusion_matrix_train.txt"))
capture.output(cm_test,  file = file.path(out_dir,  "confusion_matrix_test.txt"))

metrics_train_test %>%
  tibble::rownames_to_column("Split") %>%
  readr::write_csv(file.path(out_dir, "metrics_train_vs_test.csv"))

data.frame(Metric = names(metrics_test), Value = as.numeric(metrics_test)) %>%
  readr::write_csv(file.path(out_dir, "metrics_test_only.csv"))

#Plots: ROC/AUC
p_train <- pROC::ggroc(roc_train) +
  ggplot2::ggtitle(sprintf("ROC (TRAIN) — AUC = %.3f", auc_train)) +
  ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train.pdf"), plot = p_train, width = 6, height = 5)

p_test <- pROC::ggroc(roc_test) +
  ggplot2::ggtitle(sprintf("ROC (TEST) — AUC = %.3f", auc_test)) +
  ggplot2::theme_minimal()
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Test.pdf"), plot = p_test, width = 6, height = 5)

p_both <- pROC::ggroc(list(Train = roc_train, Test = roc_test)) +
  ggplot2::ggtitle(sprintf("ROC — Train (AUC = %.3f) vs. Test (AUC = %.3f)", auc_train, auc_test)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(linetype = "Split", color = "Split")
ggplot2::ggsave(filename = file.path(plot_dir, "ROC_Train_vs_Test.pdf"), plot = p_both, width = 6, height = 5)

#Modell speichern
saveRDS(model_xgb, file.path(out_dir, "model_xgb.rds"))

message("Fertig. Outputs liegen unter: ", normalizePath(out_dir))


#XGB Tuned
packages <- c("tidyverse","pROC","xgboost","ggplot2")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

# Daten laden & Zielvariable
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

y_train <- train_data$y
y_test  <- test_data$y
train_data$y <- NULL
test_data$y  <- NULL

y_train <- as.numeric(y_train == 1)
y_test  <- as.numeric(y_test == 1)

X_train <- as.matrix(train_data)
X_test  <- as.matrix(test_data)

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

out_dir  <- "Models/XGB_Tuned_"
plot_dir <- file.path(out_dir, "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

param_grid <- expand.grid(
  max_depth        = 2:6,
  eta              = c(0.01, 0.03, 0.05, 0.1),
  gamma            = c(0, 0.5, 1),
  colsample_bytree = c(0.6, 0.8, 1),
  min_child_weight = c(1, 3, 5),
  subsample        = c(0.7, 0.9, 1)
)

set.seed(123)
param_candidates <- param_grid[sample(1:nrow(param_grid), 30), ]  


# Cross Validation + Early Stopping
best_auc <- 0
best_model <- NULL
best_params <- NULL
best_nrounds <- NULL

for (i in 1:nrow(param_candidates)) {
  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = param_candidates$max_depth[i],
    eta = param_candidates$eta[i],
    gamma = param_candidates$gamma[i],
    colsample_bytree = param_candidates$colsample_bytree[i],
    min_child_weight = param_candidates$min_child_weight[i],
    subsample = param_candidates$subsample[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 1000,
    nfold = 5,
    early_stopping_rounds = 30,
    verbose = 0,
    maximize = TRUE,
    stratified = TRUE
  )
  
  mean_auc <- max(cv$evaluation_log$test_auc_mean)
  best_iter <- cv$best_iteration
  
  if (mean_auc > best_auc) {
    best_auc <- mean_auc
    best_params <- params
    best_nrounds <- best_iter
  }
}

cat("Beste AUC (CV):", best_auc, "\n")
cat("Beste Parameter:\n")
print(best_params)
cat("Beste Runden:", best_nrounds, "\n")

final_model <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 30,
  verbose = 1
)

pred_train_prob <- predict(final_model, dtrain)
pred_test_prob  <- predict(final_model, dtest)

#ROC / AUC
roc_train <- pROC::roc(y_train, pred_train_prob)
roc_test  <- pROC::roc(y_test,  pred_test_prob)
auc_train <- as.numeric(pROC::auc(roc_train))
auc_test  <- as.numeric(pROC::auc(roc_test))

ths <- seq(0.05, 0.95, by=0.01)
f1s <- sapply(ths, function(t){
  pr <- ifelse(pred_train_prob >= t, 1, 0)
  cm <- caret::confusionMatrix(factor(pr, levels=c(0,1)),
                               factor(y_train, levels=c(0,1)),
                               positive="1")
  cm$byClass["F1"]
})
best_thr <- ths[which.max(f1s)]
cat("Optimaler Cutoff (Train, F1-optimal):", best_thr, "\n")

pred_train_class <- ifelse(pred_train_prob >= best_thr, 1, 0)
pred_test_class  <- ifelse(pred_test_prob  >= best_thr, 1, 0)

#Confusion Matrix
cm_train <- caret::confusionMatrix(factor(pred_train_class, levels=c(0,1)),
                                   factor(y_train, levels=c(0,1)),
                                   positive="1")
cm_test  <- caret::confusionMatrix(factor(pred_test_class, levels=c(0,1)),
                                   factor(y_test, levels=c(0,1)),
                                   positive="1")

#Metriken
eps <- 1e-15
logloss <- function(y, p) {
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}
brier <- function(y, p) mean((y - p)^2)

metrics_train <- c(
  Accuracy    = cm_train$overall["Accuracy"],
  Precision   = cm_train$byClass["Precision"],
  Recall      = cm_train$byClass["Recall"],
  F1_Score    = cm_train$byClass["F1"],
  AUC         = auc_train,
  Log_Loss    = logloss(y_train, pred_train_prob),
  Brier_Score = brier(y_train, pred_train_prob)
)
metrics_test <- c(
  Accuracy    = cm_test$overall["Accuracy"],
  Precision   = cm_test$byClass["Precision"],
  Recall      = cm_test$byClass["Recall"],
  F1_Score    = cm_test$byClass["F1"],
  AUC         = auc_test,
  Log_Loss    = logloss(y_test, pred_test_prob),
  Brier_Score = brier(y_test, pred_test_prob)
)
metrics_train_test <- rbind(Train = metrics_train, Test = metrics_test) %>% as.data.frame()
print(round(metrics_train_test, 4))

#Speichern
saveRDS(final_model, file.path(out_dir, "model_xgb_tuned_cv.rds"))
readr::write_csv(
  tibble::rownames_to_column(metrics_train_test, "Split"),
  file.path(out_dir, "metrics_train_vs_test.csv")
)
capture.output(cm_train, file = file.path(out_dir, "confusion_matrix_train.txt"))
capture.output(cm_test,  file = file.path(out_dir, "confusion_matrix_test.txt"))

#ROC Plots
p_train <- pROC::ggroc(roc_train) +
  ggplot2::ggtitle(sprintf("ROC (TRAIN) — AUC = %.3f", auc_train)) +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(plot_dir, "ROC_Train.pdf"), p_train, width=6, height=5)

p_test <- pROC::ggroc(roc_test) +
  ggplot2::ggtitle(sprintf("ROC (TEST) — AUC = %.3f", auc_test)) +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(plot_dir, "ROC_Test.pdf"), p_test, width=6, height=5)

p_both <- pROC::ggroc(list(Train=roc_train, Test=roc_test)) +
  ggplot2::ggtitle(sprintf("ROC — Train (%.3f) vs. Test (%.3f)", auc_train, auc_test)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(linetype="Split", color="Split")
ggplot2::ggsave(file.path(plot_dir, "ROC_Train_vs_Test.pdf"), p_both, width=6, height=5)

message("Fertig. Ergebnisse unter: ", normalizePath(out_dir))
























#FOR THE PLOTS 
library(readr)
library(dplyr)
library(tidyr)

base_files <- c(
  "/Users/louisleicht/Statistical_Learning/Models/XGB/metrics_train_vs_test.csv",
  "/Users/louisleicht/Statistical_Learning/Models/RF/metrics_train_vs_test.csv",
  "/Users/louisleicht/Statistical_Learning/Models/NNs/NN/metrics_combined_final.csv",
  "/Users/louisleicht/Statistical_Learning/Models/LR/metrics_train_vs_test.csv"
)

tuned_files <- c(
  "/Users/louisleicht/Statistical_Learning/Models/XGB_Tuned_/metrics_train_vs_test.csv",
  "/Users/louisleicht/Statistical_Learning/Models/RF_Tuned/metrics_train_vs_test.csv",
  "/Users/louisleicht/Statistical_Learning/Models/NNs/NN_Tuned/metrics_combined_final_.csv",
  "/Users/louisleicht/Statistical_Learning/Models/LR_Tuned/RFE_Bootstrapping/metrics_train_vs_test.csv"
)

read_and_tag <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)  

  if ("Metric" %in% names(df)) {
    df <- df %>%
      pivot_wider(names_from = Metric, values_from = Test)
    df$Split <- "Test"
  }
  
  model_name <- basename(dirname(path))
  df <- df %>%
    mutate(Model = model_name, Source = path) %>%
    relocate(Model, .before = 1) %>%
    relocate(Source, .after = last_col())
  
  return(df)
}


base_df  <- bind_rows(lapply(base_files,  read_and_tag))
tuned_df <- bind_rows(lapply(tuned_files, read_and_tag))

out_dir <- "/Users/louisleicht/Statistical_Learning/Models/_Summary"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_base  <- file.path(out_dir, "metrics_standard_models.csv")
out_tuned <- file.path(out_dir, "metrics_tuned_models.csv")

write_csv(base_df,  out_base)
write_csv(tuned_df, out_tuned)

cat("Gespeichert:\n- ", normalizePath(out_base),
    "\n- ", normalizePath(out_tuned), "\n", sep = "")





# Neues Skript
library(readr)
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)

pretty_name <- function(x){
  dplyr::recode(x,
    "XGB"               = "eXtreme Gradient Boosting",
    "XGB_Tuned_"        = "eXtreme Gradient Boosting (Tuned)",
    "RF"                = "Random Forest",
    "RF_Tuned"          = "Random Forest (Tuned)",
    "NN"                = "Neuronales Netz",
    "NN_Tuned"          = "Neuronales Netz (Tuned)",
    "LR"                = "Logistische Regression",
    "RFE_Bootstrapping" = "Logistische Regression (RFE, Tuned)",
    .default = x
  )
}

make_pdf_table_latex_style <- function(csv_path, out_pdf){
  df <- read_csv(csv_path, show_col_types = FALSE)

  tab <- df %>%
    filter(tolower(Split) == "test") %>%
    transmute(
      Modell       = pretty_name(Model),
      Accuracy     = as.numeric(Accuracy),
      F1_Score     = as.numeric(F1_Score),
      AUC          = as.numeric(AUC),
      `Log Loss`   = as.numeric(Log_Loss),
      `Brier Score`= as.numeric(Brier_Score)
    ) %>%
    arrange(desc(AUC), `Log Loss`, `Brier Score`) %>%
    mutate(
      Accuracy      = sprintf("%.3f", Accuracy),
      F1_Score      = sprintf("%.3f", F1_Score),
      AUC           = sprintf("%.3f", AUC),
      `Log Loss`    = sprintf("%.3f", `Log Loss`),
      `Brier Score` = sprintf("%.3f", `Brier Score`)
    )

  tt <- ttheme_minimal(
    core = list(
      fg_params = list(fontsize = 10),
      hjust = c(0, rep(1, ncol(tab)-1)),
      x     = c(0.02, rep(0.98, ncol(tab)-1))
    ),
    colhead = list(
      fg_params = list(fontsize = 11, fontface = "bold"),
      hjust = c(0, rep(1, ncol(tab)-1)),
      x     = c(0.02, rep(0.98, ncol(tab)-1))
    )
  )

  tg <- tableGrob(tab, rows = NULL, theme = tt)

  ncols <- ncol(tg)
  add_line <- function(gt, row){
    line <- segmentsGrob(x0=unit(0,"npc"), x1=unit(1,"npc"),
                         y0=unit(1,"npc"), y1=unit(1,"npc"),
                         gp=gpar(lwd=1.2))
    gtable_add_grob(gt, line, t=row, l=1, r=ncols, z=Inf, name="line")
  }

  tg <- add_line(tg, 1)
  tg <- add_line(tg, 2)
  tg <- gtable_add_rows(tg, unit(1,"lines"))  
  tg <- add_line(tg, nrow(tg))

  pdf(out_pdf, width=7.5, height=3)
  grid.newpage(); grid.draw(tg)
  dev.off()
}

make_pdf_table_latex_style(
  "/Users/louisleicht/Statistical_Learning/Models/_Summary/metrics_standard_models.csv",
  "/Users/louisleicht/Statistical_Learning/Models/_Summary/Table_xTest_Standard.pdf"
)

make_pdf_table_latex_style(
  "/Users/louisleicht/Statistical_Learning/Models/_Summary/metrics_tuned_models.csv",
  "/Users/louisleicht/Statistical_Learning/Models/_Summary/Table_Test_Tuned.pdf"
)




#Neues Skript 
in_left  <- "/Users/louisleicht/Statistical_Learning/Models/LR/plots/ROC_Train_vs_Test.pdf"
in_right <- "/Users/louisleicht/Statistical_Learning/Models/LR_Tuned_Final/Final_ROC_Curves.pdf"
out_pdf  <- "/Users/louisleicht/Statistical_Learning/Models/Summary_ROC_SideBySide.pdf"

options(repos = c(CRAN = "https://cloud.r-project.org"))
pkgs <- c("magick", "pdftools")  
need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(need)) install.packages(need)
suppressPackageStartupMessages({
  library(magick)
  library(pdftools)
})


page_index   <- 1L   
dpi          <- 300  
trim_margins <- FALSE 

stopifnot(file.exists(in_left), file.exists(in_right))

img1 <- image_read_pdf(in_left,  density = dpi)[page_index]
img2 <- image_read_pdf(in_right, density = dpi)[page_index]

if (trim_margins) {
  img1 <- image_trim(img1)
  img2 <- image_trim(img2)
}

info1 <- image_info(img1)
info2 <- image_info(img2)
h_target <- max(info1$height, info2$height)

img1 <- image_resize(img1, paste0("x", h_target)) 
img2 <- image_resize(img2, paste0("x", h_target))

combined <- image_append(c(img1, img2), stack = FALSE)
image_write(combined, path = out_pdf, format = "pdf")

message("Fertig: ", normalizePath(out_pdf))








#Neues Skript 
in_left  <- "/Users/louisleicht/Statistical_Learning/Models/XGB/plots/ROC_Train_vs_Test.pdf"
in_right <- "/Users/louisleicht/Statistical_Learning/Models/XGB_Tuned_/plots/ROC_Train_vs_Test.pdf"
out_pdf  <- "/Users/louisleicht/Statistical_Learning/Models/Summary_ROC_SideBySide_XGB.pdf"

options(repos = c(CRAN = "https://cloud.r-project.org"))
pkgs <- c("magick", "pdftools")  # pdftools nötig, wenn Ghostscript im magick-Build deaktiviert ist
need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(need)) install.packages(need)
suppressPackageStartupMessages({
  library(magick)
  library(pdftools)
})

page_index   <- 1L   
dpi          <- 300  
trim_margins <- FALSE 

stopifnot(file.exists(in_left), file.exists(in_right))

img1 <- image_read_pdf(in_left,  density = dpi)[page_index]
img2 <- image_read_pdf(in_right, density = dpi)[page_index]

if (trim_margins) {
  img1 <- image_trim(img1)
  img2 <- image_trim(img2)
}

info1 <- image_info(img1)
info2 <- image_info(img2)
h_target <- max(info1$height, info2$height)

img1 <- image_resize(img1, paste0("x", h_target))  
img2 <- image_resize(img2, paste0("x", h_target))

combined <- image_append(c(img1, img2), stack = FALSE)
image_write(combined, path = out_pdf, format = "pdf")

message("Fertig: ", normalizePath(out_pdf))