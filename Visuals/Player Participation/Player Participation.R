# 01_structure.R (all outputs as JPEG)
library(tidyverse)
library(data.table)
library(scales)
library(gt)        # for academic-style tables

# ---- Load ----
path <- "Step1/DP_Step1_Reordered.csv"
dt <- fread(path)

# ---- Build long player list (one row per player-appearance) ----
players_long <- rbindlist(list(
  dt[, .(player_id = player1_id, role = "P1")],
  dt[, .(player_id = player2_id, role = "P2")]
))

# ---- Unique players ----
n_unique_players <- players_long[, uniqueN(player_id)]

# ---- Matches per player ----
matches_per_player <- players_long[, .N, by = player_id][order(-N)]
setnames(matches_per_player, "N", "matches")

summary_stats <- matches_per_player[, .(
  `Unique Players` = n_unique_players,
  `Average Matches per Player` = mean(matches),
  `Median Matches per Player`  = median(matches),
  `Minimum Matches per Player` = min(matches),
  `Maximum Matches per Player` = max(matches)
)]

# ---- Save summary table as JPEG ----
table_gt <- summary_stats |>
  gt() |>
  tab_header(
    title = "Summary of Player Participation Statistics"
  ) |>
  fmt_number(
    columns = everything(),
    decimals = 1
  )

gtsave(table_gt, "table_player_stats.pdf")

# ---- Histogram: matches per player ----
p_hist <- ggplot(matches_per_player, aes(x = matches)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(labels = comma) +
  labs(
    #title = "Distribution of Matches per Player",
    x = "Matches per player", y = "Count of players"
  ) +
  theme_minimal()
ggsave("hist_matches_per_player.jpeg", p_hist, width = 7, height = 4.5, dpi = 300)

# ---- Cumulative distribution (Lorenz-style) ----
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
    title = "Cumulative Distribution of Matches Across Players",
    x = "Cumulative share of players",
    y = "Cumulative share of matches"
  ) +
  theme_minimal()
ggsave("cdf_matches_per_player.jpeg", p_cdf, width = 6.5, height = 4.5, dpi = 300)

# ---- Role balance sanity check ----
role_counts <- players_long[, .N, by = role]
print(role_counts)

