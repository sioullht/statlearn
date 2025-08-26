# ---- Libraries ----
library(tidyverse)
library(ggrepel)

# Data
max_rank <- 500
df <- tibble(rank = 1:max_rank) %>%
  mutate(log_val = log(rank))

# Ranks to label
marks <- c(1, 2, 3, 5, 10, 50, 100, 200)
lab_df <- df %>% filter(rank %in% marks)

# Plot only log(rank)
p <- ggplot(df, aes(x = rank, y = log_val)) +
  geom_line(color = "#3366CC", linewidth = 1) +
  geom_point(data = lab_df, aes(x = rank, y = log_val), color = "#3366CC", size = 2) +
  # labels for all marks EXCEPT 50 (no nudge)
  geom_text_repel(
    data = lab_df %>% filter(rank != 50),
    aes(x = rank, y = log_val, label = paste0("r=", rank)),
    color = "#3366CC", size = 3, box.padding = 0.3, point.padding = 0.2,
    segment.size = 0.2
  ) +
  # label for r = 50 only (nudged so it doesn't touch the line)
  geom_text_repel(
    data = lab_df %>% filter(rank == 50),
    aes(x = rank, y = log_val, label = "r=50"),
    color = "#3366CC", size = 3, box.padding = 0.3, point.padding = 0.2,
    segment.size = 0.2, nudge_y = 0.4, nudge_x = 6
  ) +
  labs(
    x = "Rank",
    y = "log(rank)"
  ) +
  coord_cartesian(xlim = c(1, 200)) +   # zoom into the top 200 ranks (adjust as needed)
  theme_minimal(base_size = 12)

# Save as PNG
#ggsave("log_rank_plot.png", p, width = 8, height = 5, dpi = 300)

# If you want PDF or JPEG instead, just change the file extension:
# ggsave("log_rank_plot.pdf", p, width = 8, height = 5)
 ggsave("log_rank_plot.jpeg", p, width = 8, height = 5, dpi = 300)
