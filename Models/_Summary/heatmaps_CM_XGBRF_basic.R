install.packages("patchwork")
install.packages("gridExtra")

library(tidyverse)
library(ggplot2)
library(patchwork)

# CM: XGB  (neg/pos)
cm_xgb <- matrix(c(2408, 1386,
                   1404, 2485),
                 nrow = 2, byrow = TRUE,
                 dimnames = list(Prediction = c("0","1"),
                                 Reference  = c("0","1")))

# CM: Random Forest  (neg/pos)
cm_rf <- matrix(c(2441, 1308,
                  1371, 2563),
                nrow = 2, byrow = TRUE,
                dimnames = list(Prediction = c("0","1"),
                                Reference  = c("0","1")))

#Heatmap
plot_cm <- function(cm, title){
  df <- as.data.frame(as.table(cm))
  ggplot(df, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 2.6) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    scale_x_discrete(expand = expansion(mult = c(0.35, 0.35))) +  # Innen-Padding
    scale_y_discrete(expand = expansion(mult = c(0.35, 0.35))) +
    coord_fixed(ratio = 1) +
    labs(title = title, x = "Reference", y = "Prediction") +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 10),
      plot.margin = margin(10, 10, 10, 10)
    )
}

#Plots CM
p1 <- plot_cm(cm_xgb, "XGBoost (Test)")
p2 <- plot_cm(cm_rf,  "Random Forest (Test)")
combined_plot <- p1 + p2
print(combined_plot)

#Speichern
out_dir <- "Models/_Summary"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "ConfusionMatrix_RF_vs_XGB_Basic.pdf"),
       combined_plot, width = 8, height = 4)
