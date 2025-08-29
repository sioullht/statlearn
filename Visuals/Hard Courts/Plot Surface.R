# Libraries
library(ggplot2)
library(readr)

# CSV korrekt einlesen (Semikolon als Trennzeichen)
df <- read_delim("ATP_ViLo_.csv", delim = ";", show_col_types = FALSE, trim_ws = TRUE)
# Alternative: df <- read_csv2("ATP_ViLo_.csv", show_col_types = FALSE)

# Optional: prüfen, ob 'surface' existiert
# print(names(df))

# Balkendiagramm
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

