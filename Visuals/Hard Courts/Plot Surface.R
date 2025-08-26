# Load libraries
library(ggplot2)
library(readr)

# Read the CSV file
df <- read_csv("ATP_ViLo_.csv")

# Create the bar chart with blue fill and no background lines
p <- ggplot(df, aes(x = surface)) +
  geom_bar(fill = "steelblue", color = "black", width = 0.7) +
  labs(
    x = "Surface",
    y = "Number of Games"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid = element_blank(),              # remove grid lines
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12, face = "bold", color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "black")
  )

# Save as JPG
ggsave("games_per_surface.jpg", plot = p, width = 6, height = 4, dpi = 300)

