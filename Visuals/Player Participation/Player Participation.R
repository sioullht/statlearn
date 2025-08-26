library(tidyverse)

# I/O
infile <- "Step1/DP_Step1_Reordered.csv"
outdir <- "Visuals/Player Participation"
dir.create(outdir, showWarnings = FALSE)

df <- readr::read_csv(infile, show_col_types = FALSE)

# ==== COLUMN MAPPING (edit here if your names differ) ====
# Unique player ID column (per player row); if you only have names, use that.
# This script assumes a WIDE match-level dataset with Player1/Player2 IDs.
id_p1  <- "player1_id"
id_p2  <- "player2_id"

# Optional: if you have Player1/Player2 *names* instead of IDs, replace above.
# ===============================================

# Build a long table to count matches per player ID
players_long <- df %>%
  transmute(p1 = .data[[id_p1]], p2 = .data[[id_p2]]) %>%
  pivot_longer(cols = everything(), names_to = "role", values_to = "player_id") %>%
  filter(!is.na(player_id))

# 1) Histogram of matches per player
matches_per_player <- players_long %>%
  count(player_id, name = "matches") 

p_hist <- ggplot(matches_per_player, aes(x = matches)) +
  geom_histogram(bins = 40, color = "white") +
  labs(title = "Matches per Player", x = "Matches", y = "Count of Players") +
  theme_minimal(base_size = 12)

ggsave(file.path(outdir, "matches_per_player_hist.jpeg"), p_hist, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(outdir, "matches_per_player_hist.pdf"),  p_hist, width = 7, height = 4.5)

# 2) Cumulative distribution (long-tail)
matches_cum <- matches_per_player %>%
  arrange(desc(matches)) %>%
  mutate(
    player_rank = row_number(),
    players_cum_share = player_rank / n(),
    matches_cum = cumsum(matches),
    matches_cum_share = matches_cum / sum(matches)
  )

p_cdf <- ggplot(matches_cum, aes(x = players_cum_share, y = matches_cum_share)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Cumulative Share of Matches by Share of Players",
       x = "Cumulative share of players",
       y = "Cumulative share of matches") +
  theme_minimal(base_size = 12)

ggsave(file.path(outdir, "matches_per_player_cdf.jpeg"), p_cdf, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(outdir, "matches_per_player_cdf.pdf"),  p_cdf, width = 7, height = 4.5)

# 3) Player1 vs Player2 role balance
role_balance <- players_long %>% count(role)

p_role <- ggplot(role_balance, aes(x = role, y = n, fill = role)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.3) +
  labs(title = "Role Balance (Player1 vs Player2)",
       x = NULL, y = "Number of appearances") +
  theme_minimal(base_size = 12)

ggsave(file.path(outdir, "role_balance_p1_p2.jpeg"), p_role, width = 6, height = 4, dpi = 300)
ggsave(file.path(outdir, "role_balance_p1_p2.pdf"),  p_role, width = 6, height = 4)
