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

set.seed(42)  # optional: stable label placement

# Plot only log(rank)
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

