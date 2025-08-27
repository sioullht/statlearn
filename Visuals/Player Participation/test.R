# log_histogram_matches.R
# Visualize distribution of matches per player (log-scaled x-axis)

library(tidyverse)
library(data.table)

# ---- Load dataset ----
path <- "Step1/DP_Step1_Reordered.csv"
dt <- fread(path)

# ---- Count matches per player ----
# Combine Player1 and Player2 appearances
all_players <- c(dt$player1_id, dt$player2_id)   # or use IDs if available

# Create a proper data frame from frequency table
player_match_counts <- as.data.frame(table(all_players))
colnames(player_match_counts) <- c("player", "matches")
player_match_counts$matches <- as.numeric(player_match_counts$matches)

# ---- Log-scaled histogram ----
p_loghist <- ggplot(player_match_counts, aes(x = matches)) +
  geom_histogram(bins = 50, fill = "darkgrey", color = "black") +
  scale_x_log10() +
  labs(
    title = "Distribution of Matches per Player (log scale)",
    x = "Matches per player (log10 scale)",
    y = "Count of players"
  ) +
  theme_minimal(base_size = 14)

# ---- Save JPEG ----
ggsave("hist_matches_logscale.jpeg", p_loghist, width = 7.5, height = 5, dpi = 300)

# ---- Save PDF ----
ggsave("hist_matches_logscale.pdf", p_loghist, width = 7.5, height = 5)

cat("Saved: hist_matches_logscale.jpeg and hist_matches_logscale.pdf\n")
