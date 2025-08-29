# 03_ranking.R  — all outputs as JPEG
library(tidyverse)
library(data.table)

# ---- Load ----
# Adjust the path to your local file if needed
path <- "Step1/DP_Step1_Reordered.csv"
dt <- fread(path)

# ---- Ensure key columns exist and are numeric ----
req_cols <- c("player1_rank", "player2_rank", "y")
missing <- setdiff(req_cols, names(dt))
if (length(missing) > 0) {
  stop("Missing required columns: ", paste(missing, collapse = ", "))
}

dt[, `:=`(
  player1_rank = as.numeric(player1_rank),
  player2_rank = as.numeric(player2_rank),
  y            = as.numeric(y)  # 1 if Player1 won, 0 otherwise
)]

# Drop rows with missing ranks or outcomes
dt <- dt[!is.na(player1_rank) & !is.na(player2_rank) & !is.na(y)]

# ---- Quick text summaries (prints to console) ----
cat("Player1 rank summary:\n"); print(summary(dt$player1_rank))
cat("\nPlayer2 rank summary:\n"); print(summary(dt$player2_rank))

# ---- Ranking difference (P1 - P2) ----
dt[, rank_diff := player1_rank - player2_rank]

p_rd <- ggplot(dt[!is.na(rank_diff)], aes(x = rank_diff)) +
  geom_histogram(bins = 80) +
  labs(
    #title = "Distribution of Ranking Difference (Player1 - Player2)",
    x = "Ranking difference",
    y = "Number of matches"
  ) +
  theme_minimal()

ggsave("hist_rank_diff.jpeg", p_rd, width = 7, height = 4.5, dpi = 300)

# ---- Higher-ranked win probability vs |rank difference| ----
# Identify which player is higher-ranked (lower rank number)
dt[, higher_ranked_is_p1 := as.integer(player1_rank < player2_rank)]
dt[, higher_ranked_won   := as.integer(
  (higher_ranked_is_p1 == 1 & y == 1) |
  (higher_ranked_is_p1 == 0 & y == 0)
)]
dt[, abs_rank_gap := abs(rank_diff)]

# Bin by fixed width and compute win prob per bin (require min N for stability)
bin_width <- 10
dt[, gap_bin_floor := floor(abs_rank_gap / bin_width) * bin_width]      # 0,10,20,...
dt[, gap_bin_label := paste0("[", gap_bin_floor, ", ", gap_bin_floor + bin_width, ")")]

winprob_by_bin <- dt[, .(
  n = .N,
  win_prob = mean(higher_ranked_won, na.rm = TRUE),
  gap_mid  = gap_bin_floor + bin_width/2
), by = gap_bin_label][n >= 50][order(gap_mid)]  # keep bins with >= 50 matches

p_wp <- ggplot(winprob_by_bin, aes(x = gap_mid, y = win_prob)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span = 0.8, color = "grey40") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    #title = "Higher-Ranked Player Win Probability vs |Ranking Difference|",
    x = "Absolute ranking difference",
    y = "Win probability (higher-ranked)"
  ) +
  theme_minimal()

ggsave("rankdiff_vs_winprob.jpeg", p_wp, width = 7, height = 4.5, dpi = 300)

# ---- Optional: print the first rows of the bin table to console ----
cat("\nWin probability by absolute rank-gap bin (first rows):\n")
print(head(winprob_by_bin, 10))
