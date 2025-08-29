#Data Preprocessing
library(dplyr)
library(readr)

#Daten einlesen
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

#Neues Skript
#Beginn Feature Engineering (vorübergehend)
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

#Neues Skript
#Beginn Data Prepration 
library(tidyverse)
library(dplyr)

input_file <- "Step1/FE_Step1.csv"
output_file <- "Step1/DP_Step1.csv"

read_csv(input_file) %>%
  
  #Nur Sruface Typ "Hard"
  filter(surface == "Hard") %>%
  
  #Entfernung von nicht benötigten Spalten
  select(-tourney_name, -best_of, -minutes, -player1_ioc, -player2_ioc, -surface) %>%
  
  #Alle Zeilen mit NA entfernen
  drop_na() %>%

  write_csv(output_file)

#Neues Skript
library(tidyverse)
library(dplyr)

input_file <- "Step1/DP_Step1.csv"
output_file <- "Step1/DP_Step1_Reordered.csv"

df <- read_csv(input_file)

#Wähle und ordne die Spalten neu an
df_reordered <- df %>%
  select(
    #Statische Spielerdaten
    player1_id,
    player2_id,
    player1_age,
    player2_age,
    player1_ht,
    player2_ht,
    p1_hand_R, p1_hand_L, p1_hand_U,
    p2_hand_R, p2_hand_L, p2_hand_U,
    date,
    
    # Ranking-Daten
    player1_rank,
    player1_rank_pts,
    player2_rank,
    player2_rank_pts,
    
    #Differenz-Features 
    log_rank_diff,
    rankpoints_diff,
    ace_diff,
    df_diff,
    in1_pct_diff,
    won1_pct_diff,
    sv_win_pct_diff,
    bp_saved_pct_diff,
    bp_per_game_diff,
    
    #Percent-Features
    p1_ace_percent, p1_doublefault_percent, p1_1in_percent, p1_1won_percent, p1_serve_win_percent, p1_breakp_saved, p1_breakp_succeed,
    p2_ace_percent, p2_doublefault_percent, p2_1in_percent, p2_1won_percent, p2_serve_win_percent, p2_breakp_saved, p2_breakp_succeed,
    
    #Originale Roh-Spalten
    player1_ace, player1_df, player1_svpt, player1_1stin, player1_1stwon, player1_2ndwon, player1_bpsaved, player1_bpfaced, player1_svgms,
    player2_ace, player2_df, player2_svpt, player2_1stin, player2_1stwon, player2_2ndwon, player2_bpsaved, player2_bpfaced, player2_svgms,

    #Zielvariable 'y'
    y
  )

  #Dataset chronologisch sortieren
  df_reordered <- df_reordered %>%
    arrange(date)

write_csv(df_reordered, output_file)

#Neues Skript
#Feature Engineering Schritte
library(tidyverse)
library(zoo)

df_original <- read_csv("Step1/DP_Step1_Reordered.csv")

df_sorted <- df_original %>%
  arrange(date) %>%
  mutate(match_id = row_number())

#Daten umstrukturieren (Wide-to-Long-Format)
stat_cols <- c(
  "ace", "df", "svpt", "1stin", "1stwon", "2ndwon",
  "bpsaved", "bpfaced", "svgms"
)
p1_data <- df_sorted %>%
  select(match_id, date, player1_id, player2_id, one_of(paste0("player1_", stat_cols))) %>%
  rename_with(~str_remove(., "player1_"), .cols = all_of(paste0("player1_", stat_cols))) %>%
  rename(player_id = player1_id, opponent_id = player2_id)
p2_data <- df_sorted %>%
  select(match_id, date, player2_id, player1_id, one_of(paste0("player2_", stat_cols))) %>%
  rename_with(~str_remove(., "player2_"), .cols = all_of(paste0("player2_", stat_cols))) %>%
  rename(player_id = player2_id, opponent_id = player1_id)
long_data <- bind_rows(p1_data, p2_data) %>%
  arrange(player_id, date, match_id)

write_csv(long_data, "Step2/FE_Step2_Intermediate_Long_Format.csv")

#Berechnung der historischen Werte
historical_data <- long_data %>%
  group_by(player_id) %>%
  mutate(across(all_of(stat_cols),
                ~lag(rollapplyr(.,
                                width = 10,
                                FUN = mean,
                                partial = TRUE,
                                fill = NA,
                                align = "right")),
                .names = "h_{.col}")) %>%
  mutate(across(starts_with("h_"), ~replace_na(., 0))) %>%
  ungroup()

write_csv(historical_data, "Step2/FE_Step2_Intermediate_With_Historical_Data.csv")

#Daten zurückführen (Long-to-Wide-Format)
historical_slim <- historical_data %>%
  select(match_id, player_id, starts_with("h_"))
df_final <- df_sorted %>%
  left_join(historical_slim, by = c("match_id", "player1_id" = "player_id")) %>%
  rename_with(~str_replace(., "h_", "player1_h_"), .cols = starts_with("h_"))
df_final <- df_final %>%
  left_join(historical_slim, by = c("match_id", "player2_id" = "player_id")) %>%
  rename_with(~str_replace(., "h_", "player2_h_"), .cols = starts_with("h_"))

#Neuberechnung aller Features
df_recalculated <- df_final %>%
  mutate(
    p1_ace_percent = ifelse(player1_h_svpt > 0, player1_h_ace / player1_h_svpt, 0),
    p1_doublefault_percent = ifelse(player1_h_svpt > 0, player1_h_df / player1_h_svpt, 0),
    p1_1in_percent = ifelse(player1_h_svpt > 0, player1_h_1stin / player1_h_svpt, 0),
    p1_1won_percent = ifelse(player1_h_1stin > 0, player1_h_1stwon / player1_h_1stin, 0),
    p1_serve_win_percent = ifelse(player1_h_svpt > 0, (player1_h_1stwon + player1_h_2ndwon) / player1_h_svpt, 0),
    p1_breakp_saved = ifelse(player1_h_bpfaced > 0, player1_h_bpsaved / player1_h_bpfaced, 0),
    
    p2_ace_percent = ifelse(player2_h_svpt > 0, player2_h_ace / player2_h_svpt, 0),
    p2_doublefault_percent = ifelse(player2_h_svpt > 0, player2_h_df / player2_h_svpt, 0),
    p2_1in_percent = ifelse(player2_h_svpt > 0, player2_h_1stin / player2_h_svpt, 0),
    p2_1won_percent = ifelse(player2_h_1stin > 0, player2_h_1stwon / player2_h_1stin, 0),
    p2_serve_win_percent = ifelse(player2_h_svpt > 0, (player2_h_1stwon + player2_h_2ndwon) / player2_h_svpt, 0),
    p2_breakp_saved = ifelse(player2_h_bpfaced > 0, player2_h_bpsaved / player2_h_bpfaced, 0),
    
    p1_breakp_succeed = ifelse(player1_h_svpt == 0, 0, 1 - p2_breakp_saved),
    p2_breakp_succeed = ifelse(player2_h_svpt == 0, 0, 1 - p1_breakp_saved),

    ace_diff = p1_ace_percent - p2_ace_percent,
    df_diff = p1_doublefault_percent - p2_doublefault_percent,
    in1_pct_diff = p1_1in_percent - p2_1in_percent,
    won1_pct_diff = p1_1won_percent - p2_1won_percent,
    sv_win_pct_diff = p1_serve_win_percent - p2_serve_win_percent,
    bp_saved_pct_diff = p1_breakp_saved - p2_breakp_saved,
    bp_per_game_diff = ifelse(player1_h_svgms > 0, player1_h_bpfaced / player1_h_svgms, 0) -
                       ifelse(player2_h_svgms > 0, player2_h_bpfaced / player2_h_svgms, 0)
  )

#Finales Dataset erstellen
final_columns <- df_recalculated %>%
  select(
    player1_id, player2_id, player1_age, player2_age, player1_ht, player2_ht,
    p1_hand_R, p1_hand_L, p1_hand_U, p2_hand_R, p2_hand_L, p2_hand_U,
    date, player1_rank, player1_rank_pts, player2_rank, player2_rank_pts,

    log_rank_diff, rankpoints_diff, ace_diff, df_diff, in1_pct_diff,
    won1_pct_diff, sv_win_pct_diff, bp_saved_pct_diff, bp_per_game_diff,

    p1_ace_percent, p1_doublefault_percent, p1_1in_percent, p1_1won_percent,
    p1_serve_win_percent, p1_breakp_saved, p1_breakp_succeed,
    p2_ace_percent, p2_doublefault_percent, p2_1in_percent, p2_1won_percent,
    p2_serve_win_percent, p2_breakp_saved, p2_breakp_succeed,

    starts_with("player1_h_"),
    starts_with("player2_h_"),

    y
  )

write_csv(final_columns, "Step2/FE_Step2_Historical_Try2.csv")

#Neues Skript
library(tidyverse)

input_file <- "Step2/FE_Step2_Historical_Try2.csv"
output_file <- "Step3/FE_Step3_Filtered.csv"

df <- read_csv(input_file)

#Zeilen filtern und Spalten entfernen wo keine Historie
df_final <- df %>%

  filter(player1_h_svpt > 0 & player2_h_svpt > 0) %>%

  select(-player1_id, -player2_id, -date)

write_csv(df_final, output_file)

library(tidyverse)
library(caret)

input_file <- "Step3/FE_Step3_Filtered.csv"
df <- read_csv(input_file)

#Daten aufteilen (Trainings- und Testset)
set.seed(123) 
train_indices <- createDataPartition(df$y, p = 0.7, list = FALSE)
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

#Prädiktoren für die Skalierung
cols_to_scale <- setdiff(names(train_data), c("player1_id", "player2_id", "date", "p1_hand_R", "p1_hand_L", "p1_hand_U", "p2_hand_R", "p2_hand_L", "p2_hand_U", "y"))

#Skalierungsparameter aus den Trainingsdaten lernen
scaler <- preProcess(train_data[cols_to_scale], method = c("center", "scale"))

#Skalierung auf Trainings- und Testdaten anwenden
train_data_scaled <- predict(scaler, train_data)
test_data_scaled <- predict(scaler, test_data)

write_csv(train_data_scaled, "Step3/train_data.csv")
write_csv(test_data_scaled, "Step3/test_data.csv")


#Machine Learning Models
#Neues Skript
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

#Neues Skript
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

#Neues Skript
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

#Neues Skript
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

#Neues Skript
#LR Basic Modell
packages <- c("tidyverse","caret","pROC","ggplot2")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

#Labels anpassen
train_data$y <- factor(train_data$y, levels = c(0,1), labels = c("Spieler2", "Spieler1"))
test_data$y  <- factor(test_data$y,  levels = c(0,1), labels = c("Spieler2", "Spieler1"))

# Modellstruktur
ctrl_none <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

model_lr <- train(
  y ~ ., data = train_data,
  method    = "glm",
  family    = "binomial",
  trControl = ctrl_none,
  metric    = "ROC"
)

print(model_lr)

#Train
pred_train_prob  <- predict(model_lr, train_data, type = "prob")[, "Spieler1"]
pred_train_class <- predict(model_lr, train_data)

#Test
pred_test_prob   <- predict(model_lr, test_data,  type = "prob")[, "Spieler1"]
pred_test_class  <- predict(model_lr, test_data)


# ROC / AUC
roc_train <- pROC::roc(train_data$y, pred_train_prob, levels = c("Spieler2", "Spieler1"), direction = "<")
auc_train <- as.numeric(pROC::auc(roc_train))

roc_test  <- pROC::roc(test_data$y,  pred_test_prob,  levels = c("Spieler2", "Spieler1"), direction = "<")
auc_test  <- as.numeric(pROC::auc(roc_test))

# Confusion Matrices
cm_train <- caret::confusionMatrix(pred_train_class, train_data$y, positive = "Spieler1")
cm_test  <- caret::confusionMatrix(pred_test_class,  test_data$y,  positive = "Spieler1")

# Metriken berechnen
eps <- 1e-15

#Train
pp_tr  <- pmin(pmax(pred_train_prob, eps), 1 - eps)
y01_tr <- as.numeric(train_data$y == "Spieler1")
logloss_tr <- -mean(y01_tr * log(pp_tr) + (1 - y01_tr) * log(1 - pp_tr))
brier_tr   <- mean((y01_tr - pred_train_prob)^2)

#Test
pp_te  <- pmin(pmax(pred_test_prob, eps), 1 - eps)
y01_te <- as.numeric(test_data$y == "Spieler1")
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


# Speicherung & Plots
out_dir  <- "Models/LR"
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

capture.output(cm_train, file = file.path(out_dir, "confusion_matrix_train.txt"))
capture.output(cm_test,  file = file.path(out_dir, "confusion_matrix_test.txt"))

metrics_train_test %>%
  tibble::rownames_to_column("Split") %>%
  readr::write_csv(file.path(out_dir, "metrics_train_vs_test.csv"))

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

saveRDS(model_lr, file.path(out_dir, "model_lr.rds"))


#Finale Test-Metriken speichern
final_metrics <- metrics_train_test %>%
  tibble::rownames_to_column("Split") %>%
  dplyr::filter(Split == "Test")

readr::write_csv(final_metrics, file.path(out_dir, "model_metrics.csv"))

#Neues Skript
#LR Tuned
packages <- c("tidyverse", "caret", "pROC", "ggplot2", "MLmetrics")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))


set.seed(123)

compute_metrics <- function(y_true_factor, prob_pos, class_pred, positive = "Spieler1") {
  eps <- 1e-15
  y_true <- as.numeric(y_true_factor == positive)
  pp     <- pmin(pmax(prob_pos, eps), 1 - eps)

  auc_val <- as.numeric(pROC::auc(y_true_factor, prob_pos,
                                  levels = c("Spieler2", "Spieler1"), direction = "<"))
  cm <- caret::confusionMatrix(class_pred, y_true_factor, positive = positive)

  tibble::tibble(
    Accuracy    = unname(cm$overall["Accuracy"]),
    Precision   = unname(cm$byClass["Precision"]),
    Recall      = unname(cm$byClass["Recall"]),
    F1_Score    = unname(cm$byClass["F1"]),
    AUC         = auc_val,
    Log_Loss    = MLmetrics::LogLoss(y_pred = pp, y_true = y_true),
    Brier_Score = mean((y_true - prob_pos)^2)
  )
}

#Zusammenfassung CV-Folds
summarize_resampling <- function(model, positive = "Spieler1") {
  preds <- model$pred
  resampled_metrics <- preds %>%
    dplyr::group_by(Resample) %>%
    dplyr::summarise(
      dplyr::as_tibble(compute_metrics(
        y_true_factor = obs,
        prob_pos      = .data[[positive]],
        class_pred    = pred,
        positive      = positive
      ))
    )
  aggregated_summary <- resampled_metrics %>%
    dplyr::select(-Resample) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Metric", values_to = "Value") %>%
    dplyr::group_by(Metric) %>%
    dplyr::summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD   = sd(Value, na.rm = TRUE),
      .groups = "drop"
    )
  return(aggregated_summary)
}

# Daten laden
train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

train_data$y <- factor(train_data$y, levels = c(0, 1), labels = c("Spieler2", "Spieler1"))
test_data$y  <- factor(test_data$y,  levels = c(0, 1), labels = c("Spieler2", "Spieler1"))


#Modell-Tuning mit RFE
message("Starte RFE-Prozess zur Feature-Selektion")

rfe_control <- rfeControl(
  functions = lrFuncs,
  method = "cv",
  number = 10,
  verbose = FALSE
)

rfe_profile <- rfe(
  y ~ .,
  data = train_data,
  sizes = 2:(ncol(train_data) - 1),
  rfeControl = rfe_control,
  metric = "ROC"
)

best_features <- predictors(rfe_profile)
print(rfe_profile)
message(paste("\nOptimale Anzahl an Features gefunden:", length(best_features)))
print(best_features)


#Zweite CV
message("\nStarte zweite CV zur Stabilitätsanalyse")

train_data_rfe <- train_data[, c("y", best_features)]

ctrl_cv_eval <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

model_final_cv <- train(
  y ~ ., data = train_data_rfe,
  method = "glm",
  family = "binomial",
  trControl = ctrl_cv_eval,
  metric = "ROC"
)

cv_stability_stats <- summarize_resampling(model_final_cv, positive = "Spieler1")

message("\n--- Ergebnisse der Stabilitätsanalyse (aus 10-facher CV) ---")
print(cv_stability_stats %>% dplyr::mutate(across(where(is.numeric), ~ round(., 4))))

#MODELL-EVALUATION
message("\nFINALE EVALUATION: Bewerte das finale Modell auf dem ungesehenen Test-Set...")

pred_train_prob  <- predict(model_final_cv, train_data_rfe, type = "prob")[, "Spieler1"]
pred_test_prob   <- predict(model_final_cv, test_data,      type = "prob")[, "Spieler1"]
pred_train_class <- predict(model_final_cv, train_data_rfe)
pred_test_class  <- predict(model_final_cv, test_data)

metrics_train <- compute_metrics(train_data$y, pred_train_prob, pred_train_class)
metrics_test  <- compute_metrics(test_data$y, pred_test_prob, pred_test_class)
metrics_summary <- dplyr::bind_rows(Train = metrics_train, Test = metrics_test, .id = "Split")

message("\n--- Finale Metriken (Train vs. Test) ---")
print(metrics_summary %>% dplyr::mutate(across(where(is.numeric), ~ round(., 4))))

#Ergebnisse speichern
out_dir  <- "Models/LR_RFE_with_2CV"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

metrics_summary_rounded <- metrics_summary %>%
  dplyr::mutate(across(where(is.numeric), ~ round(., 4)))
readr::write_csv(metrics_summary_rounded, file.path(out_dir, "final_model_metrics.csv"))

cv_stability_stats_rounded <- cv_stability_stats %>%
  dplyr::mutate(across(where(is.numeric), ~ round(., 4)))
readr::write_csv(cv_stability_stats_rounded, file.path(out_dir, "cv_stability_metrics.csv"))

capture.output(rfe_profile, file = file.path(out_dir, "rfe_results.txt"))

saveRDS(model_final_cv, file.path(out_dir, "model_final_cv.rds"))

pdf(file.path(out_dir, "RFE_Performance_Curve.pdf"), width = 8, height = 6)
plot(rfe_profile, type = c("g", "o"), main = "RFE Performance-Profil (Phase 1)")
dev.off()

jpeg(file.path(out_dir, "RFE_Performance_Curve.jpeg"), width = 800, height = 600)
plot(rfe_profile, type = c("g", "o"), main = "RFE Performance-Profil (Phase 1)")
dev.off()

roc_train <- pROC::roc(train_data$y, pred_train_prob, levels = c("Spieler2", "Spieler1"))
roc_test  <- pROC::roc(test_data$y,  pred_test_prob,  levels = c("Spieler2", "Spieler1"))
roc_plot <- pROC::ggroc(list(Train = roc_train, Test = roc_test)) +
  ggplot2::ggtitle(sprintf("Finale ROC-Kurven — Train (AUC=%.3f) vs. Test (AUC=%.3f)", roc_train$auc, roc_test$auc)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(color = "Datensatz")
ggplot2::ggsave(filename = file.path(out_dir, "Final_ROC_Curves.pdf"), plot = roc_plot, width = 7, height = 6)

message("\nSkript erfolgreich ausgeführt. Alle Ergebnisse unter: ", normalizePath(out_dir))

#Neues Skript
#NN Basic
packages <- c("keras3", "readr", "tidyverse", "caret", "pROC", "ggplot2")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

train_data <- readr::read_csv("Step3/train_data.csv")
test_data  <- readr::read_csv("Step3/test_data.csv")

x_train <- as.matrix(train_data[, -which(names(train_data) == "y")])
y_train <- as.matrix(train_data$y)

x_test <- as.matrix(test_data[, -which(names(test_data) == "y")])
y_test <- as.matrix(test_data$y)

y_train_factor <- factor(train_data$y, levels = c(0, 1), labels = c("Spieler 2", "Spieler 1"))
y_test_factor  <- factor(test_data$y,  levels = c(0, 1), labels = c("Spieler 2", "Spieler 1"))


#NN trainieren
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

# Vorhersagen generieren
prob_train <- as.array(predict(model, x_train))[,1]
prob_test  <- as.array(predict(model, x_test))[,1]

pred_class_train <- factor(ifelse(prob_train >= 0.5, "Spieler 1", "Spieler 2"), levels = c("Spieler 2", "Spieler 1"))
pred_class_test  <- factor(ifelse(prob_test >= 0.5, "Spieler 1", "Spieler 2"), levels = c("Spieler 2", "Spieler 1"))

# Auswertung mit caret und pROC
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


# Ergebnisse speichern
out_dir <- "NN"
plot_dir <- "NN/plots"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

capture.output(cm_test, file = file.path(out_dir, "confusion_matrix_test.txt"))

metrics_train_test %>%
  tibble::rownames_to_column("Metric") %>%
  readr::write_csv(file.path(out_dir, "metrics_train_vs_test_nn.csv"))

metrics_test_df <- round(data.frame(Value = metrics_test), 3)
metrics_test_df %>%
  tibble::rownames_to_column("Metric") %>%
  readr::write_csv(file.path(out_dir, "metrics_test_only_nn.csv"))
  
# Plots
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


#Neues Skript
#NN Tuned
packages <- c("keras3", "readr", "tidyverse", "caret", "pROC", "ggplot2")
installed <- rownames(installed.packages())
for (pkg in packages) if (!pkg %in% installed) install.packages(pkg)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123)

# Daten laden udn vorbereiten
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

x_train <- as.matrix(x_train_df)
x_test  <- as.matrix(x_test_df)
y_train <- as.matrix(train_data$y)
y_test <- as.matrix(test_data$y)



# Ordner für Ergebnisse
out_dir <- "Models/NNs/NN_Tuned"
plot_dir <- file.path(out_dir, "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)


# Hyperparameter-Tuning mit Grid Search
cat("\nStarte Hyperparameter-Tuning mit Grid Search...\n")

tune_grid <- expand.grid(
  units1 = c(128, 64),
  units2 = c(64, 32),
  dropout1 = c(0.2, 0.4),
  learning_rate = c(0.001, 0.0005)
)

tuning_results <- list()
set.seed(123)
cv_folds <- createFolds(y_train_factor, k = 5, returnTrain = TRUE)

for (i in 1:nrow(tune_grid)) {
  params <- tune_grid[i, ]
  cv_auc_scores <- c()
  
  cat(sprintf("Teste Kombination %d/%d: units1=%d, units2=%d, dropout1=%.2f, lr=%.4f\n", 
              i, nrow(tune_grid), params$units1, params$units2, params$dropout1, params$learning_rate))
  
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

best_params <- tuning_summary[which.max(tuning_summary$mean_auc), ]
cat("\nBeste Hyperparameter:\n")
print(best_params)

# Finales Modell mit den besten Hyperparametern trainieren
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

# Modell und Ergebnisse speichern
save_model(final_model, file.path(out_dir, "nn_model_final_tuned.keras"))
readr::write_csv(tuning_summary, file.path(out_dir, "tuning_summary.csv"))
cat("Getuntes Modell und Tuning-Ergebnisse wurden gespeichert in:", out_dir, "\n")

#Train/Test
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

#Cross Validation
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

#Finale Ergebnisse speichern
metrics_train_test %>%
  tibble::rownames_to_column("Metric") %>%
  readr::write_csv(file.path(out_dir, "metrics_train_vs_test_final.csv"))

readr::write_csv(cv_summary, file.path(out_dir, "metrics_cv10_final.csv"))

metrics_test_df <- data.frame(Metric = rownames(metrics_train_test), Test = as.numeric(metrics_train_test$Test))
cv_df <- cv_summary %>% select(Metric, Mean) %>% rename(CV10_Mean = Mean)
combined_results <- metrics_test_df %>% left_join(cv_df, by = "Metric") %>% mutate(across(-Metric, ~round(., 3)))
readr::write_csv(combined_results, file.path(out_dir, "metrics_combined_final.csv"))

cat("\nAlle finalen Auswertungen erfolgreich gespeichert in:", out_dir, "\n")
cat("Skript vollständig ausgeführt.\n")




#FOR THE PLOTS 
#Neues Skript
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

#Neues Skript
#Plot Sruface
library(ggplot2)
library(readr)

df <- read_delim("ATP_ViLo_.csv", delim = ";", show_col_types = FALSE, trim_ws = TRUE)

p <- ggplot(df, aes(x = surface)) +
  geom_bar(fill = "grey", color = "black", width = 0.7) +
  labs(
    x = "Surface",
    y = "Number of Matches"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black")
  )

ggsave("games_per_surface.jpg", plot = p, width = 6, height = 4, dpi = 300)

#Neues Skript
#Log_Rank Plot
library(tidyverse)
library(ggrepel)

# Data
max_rank <- 500
df <- tibble(rank = 1:max_rank) %>%
  mutate(log_val = log(rank))

# Ranks to label
marks <- c(1, 2, 3, 5, 10, 50, 100, 200)
lab_df <- df %>% filter(rank %in% marks)

set.seed(42)

p <- ggplot(df, aes(x = rank, y = log_val)) +
  geom_line(color = "#616163ff", linewidth = 1) +
  geom_point(data = lab_df, aes(x = rank, y = log_val),
             color = "#000000ff", size = 2) +
  geom_text_repel(
    data = lab_df,
    aes(x = rank, y = log_val, label = paste0("r=", rank)),
    color = "#000000ff", size = 3,
    box.padding = 0.3, point.padding = 0.2, segment.size = 0.2
  ) +
  labs(x = "Rank", y = "log(rank)") +
  coord_cartesian(xlim = c(1, 200)) +
  theme_minimal(base_size = 12)

ggsave("log_rank_plot.jpeg", p, width = 8, height = 5, dpi = 300)

#Neues Skript
#Density age and height plot
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(dplyr)
})

path <- "Step1/DP_Step1_Reordered.csv" 
dt <- fread(path)

req <- c("player1_age","player2_age","player1_ht","player2_ht")
missing <- setdiff(req, names(dt))
if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))

demo_long <- rbindlist(list(
  dt[, .(who = "Player1", variable = "Age (years)",   value = as.numeric(player1_age))],
  dt[, .(who = "Player2", variable = "Age (years)",   value = as.numeric(player2_age))],
  dt[, .(who = "Player1", variable = "Height (cm)",   value = as.numeric(player1_ht))],
  dt[, .(who = "Player2", variable = "Height (cm)",   value = as.numeric(player2_ht))]
)) %>%
  filter(!is.na(value))
---
p_demo <- ggplot(demo_long, aes(x = value, fill = who)) +
  geom_density(alpha = 0.35) +
  facet_wrap(~ variable, scales = "free_x", ncol = 2) +
  labs(
    x = NULL, y = "Density", fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )

ggsave("density_age_height_combined.jpeg", p_demo, width = 9, height = 4.8, dpi = 300)

#Neues Skript
#Player Participation Plots
library(tidyverse)
library(data.table)
library(scales)
library(gt)     

path <- "Step1/DP_Step1_Reordered.csv"
dt <- fread(path)

players_long <- rbindlist(list(
  dt[, .(player_id = player1_id, role = "P1")],
  dt[, .(player_id = player2_id, role = "P2")]
))

n_unique_players <- players_long[, uniqueN(player_id)]

matches_per_player <- players_long[, .N, by = player_id][order(-N)]
setnames(matches_per_player, "N", "matches")

summary_stats <- matches_per_player[, .(
  `Unique Players` = n_unique_players,
  `Average Matches per Player` = mean(matches),
  `Median Matches per Player`  = median(matches),
  `Minimum Matches per Player` = min(matches),
  `Maximum Matches per Player` = max(matches)
)]

#Summary table
table_gt <- summary_stats |>
  gt() |>
  fmt_number(
    columns = everything(),
    decimals = 1
  )

gtsave(table_gt, "table_player_stats.pdf")


#Cumulative distribution
matches_per_player[, prop_matches := matches / sum(matches)]
matches_per_player <- matches_per_player[order(-matches)][
  , `:=`(
    cum_players = seq_len(.N) / .N,
    cum_matches = cumsum(prop_matches)
  )
]

p_cdf <- ggplot(matches_per_player, aes(x = cum_players, y = cum_matches)) +
  geom_line(size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Cumulative share of players",
    y = "Cumulative share of matches"
  ) +
  theme_minimal()
ggsave("cdf_matches_per_player.jpeg", p_cdf, width = 6.5, height = 4.5, dpi = 300)

role_counts <- players_long[, .N, by = role]
print(role_counts)

#Neues Skript
#RFE Curve Plot
packages <- c("tidyverse", "ggrepel")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))

message("Reading 'rfe_results.txt'...")

input_file <- "Models/LR_Tuned_Final/rfe_results.txt"

if (!file.exists(input_file)) {
  stop(paste("The file was not found at the specified path:", input_file))
}

lines <- readr::read_lines(input_file)

start_index <- which(grepl("^\\s*Variables\\s+Accuracy", lines))

if (length(start_index) == 0) {
  stop("Could not find the table header 'Variables Accuracy' in the file.")
}

table_text <- lines[start_index:length(lines)]

rfe_data <- readr::read_table(paste(table_text, collapse = "\n"), col_types = readr::cols()) %>%
  dplyr::filter(!is.na(Variables)) %>%
  dplyr::mutate(dplyr::across(-Selected, as.numeric))

#Optimale Points
optimal_point <- rfe_data %>%
  dplyr::filter(Selected == "*")

if (nrow(optimal_point) == 0) {
  message("No optimal point ('*') found. Selecting the maximum accuracy instead.")
  optimal_point <- rfe_data %>%
    dplyr::filter(Accuracy == max(Accuracy, na.rm = TRUE)) %>%
    dplyr::filter(Variables == min(Variables, na.rm = TRUE))
}

opt_size     <- optimal_point$Variables
opt_accuracy <- optimal_point$Accuracy

message(paste("Optimal point found at", opt_size, "variables with an accuracy of", round(opt_accuracy, 4)))

#RFE Curve erstellen
message("Creating the final RFE performance curve...")

rfe_plot <- ggplot2::ggplot(rfe_data, ggplot2::aes(x = Variables, y = Accuracy)) +
  ggplot2::geom_line(color = "steelblue", linewidth = 1, alpha = 0.8) +
  ggplot2::geom_point(color = "steelblue", size = 2.5, alpha = 0.8) +
  
  ggplot2::geom_segment(
    data = optimal_point,
    ggplot2::aes(x = opt_size, xend = opt_size, y = min(rfe_data$Accuracy), yend = opt_accuracy),
    linetype = "dashed",
    color = "black"
  ) +

  ggplot2::geom_point(data = optimal_point, color = "black", size = 4) +
  
  ggrepel::geom_text_repel(
    data = optimal_point,
    ggplot2::aes(label = paste0("Optimum\n", opt_size, " Variables\nAccuracy = ", sprintf("%.4f", opt_accuracy))),
    color = "black", 
    nudge_y = 0.003,
    min.segment.length = 0,
    seed = 123
  ) +
  
  ggplot2::labs(
    x = "Number of Selected Variables",
    y = "Model Accuracy"
  ) +
  
  ggplot2::theme_minimal(base_size = 14)

# Plot Speichern
ggplot2::ggsave(
  filename = "Models/LR_Tuned_Final/RFE_Performance_Curve.pdf",
  plot = rfe_plot,
  width = 10,
  height = 6,
  device = "pdf"
)
ggplot2::ggsave(
  filename = "Models/LR_Tuned_Final/RFE_Performance_Curve.jpg",
  plot = rfe_plot,
  width = 10,
  height = 6,
  dpi = 300
)


#Neues Skript
#Ranking information
library(tidyverse)
library(data.table)

path <- "Step1/DP_Step1_Reordered.csv"
dt <- fread(path)

req_cols <- c("player1_rank", "player2_rank", "y")
missing <- setdiff(req_cols, names(dt))
if (length(missing) > 0) {
  stop("Missing required columns: ", paste(missing, collapse = ", "))
}

dt[, `:=`(
  player1_rank = as.numeric(player1_rank),
  player2_rank = as.numeric(player2_rank),
  y            = as.numeric(y)
)]

dt <- dt[!is.na(player1_rank) & !is.na(player2_rank) & !is.na(y)]

cat("Player1 rank summary:\n"); print(summary(dt$player1_rank))
cat("\nPlayer2 rank summary:\n"); print(summary(dt$player2_rank))

dt[, rank_diff := player1_rank - player2_rank]

p_rd <- ggplot(dt[!is.na(rank_diff)], aes(x = rank_diff)) +
  geom_histogram(bins = 80) +
  labs(
    x = "Ranking difference",
    y = "Number of matches"
  ) +
  theme_minimal()

ggsave("hist_rank_diff.jpeg", p_rd, width = 7, height = 4.5, dpi = 300)

#Higher-ranked win probability vs |rank difference|
dt[, higher_ranked_is_p1 := as.integer(player1_rank < player2_rank)]
dt[, higher_ranked_won   := as.integer(
  (higher_ranked_is_p1 == 1 & y == 1) |
  (higher_ranked_is_p1 == 0 & y == 0)
)]
dt[, abs_rank_gap := abs(rank_diff)]

bin_width <- 10
dt[, gap_bin_floor := floor(abs_rank_gap / bin_width) * bin_width]    
dt[, gap_bin_label := paste0("[", gap_bin_floor, ", ", gap_bin_floor + bin_width, ")")]

winprob_by_bin <- dt[, .(
  n = .N,
  win_prob = mean(higher_ranked_won, na.rm = TRUE),
  gap_mid  = gap_bin_floor + bin_width/2
), by = gap_bin_label][n >= 50][order(gap_mid)] 

p_wp <- ggplot(winprob_by_bin, aes(x = gap_mid, y = win_prob)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span = 0.8, color = "grey40") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Absolute ranking difference",
    y = "Win probability (higher-ranked)"
  ) +
  theme_minimal()

ggsave("rankdiff_vs_winprob.jpeg", p_wp, width = 7, height = 4.5, dpi = 300)

cat("\nWin probability by absolute rank-gap bin (first rows):\n")
print(head(winprob_by_bin, 10))
