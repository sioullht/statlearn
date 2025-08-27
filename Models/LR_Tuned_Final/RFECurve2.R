# -------------------------------------------------------
# Load required packages
# -------------------------------------------------------
# Ensures all necessary packages are installed and loaded.
packages <- c("tidyverse", "ggrepel")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))


# -------------------------------------------------------
# 1. Read and parse data from the text file
# -------------------------------------------------------
message("Reading 'rfe_results.txt'...")

# Input file path (as specified by you)
input_file <- "Models/LR_Tuned_Final/rfe_results.txt"

if (!file.exists(input_file)) {
  stop(paste("The file was not found at the specified path:", input_file))
}

# Read all lines from the file
lines <- readr::read_lines(input_file)

# Find the starting line of the table (the header)
start_index <- which(grepl("^\\s*Variables\\s+Accuracy", lines))

if (length(start_index) == 0) {
  stop("Could not find the table header 'Variables Accuracy' in the file.")
}

# Extract the raw text of the table
table_text <- lines[start_index:length(lines)]

# Read the extracted text into a data frame
rfe_data <- readr::read_table(paste(table_text, collapse = "\n"), col_types = readr::cols()) %>%
  # Remove rows created from parsing the footer text
  dplyr::filter(!is.na(Variables)) %>%
  # Convert data columns to numeric format to prevent errors
  dplyr::mutate(dplyr::across(-Selected, as.numeric))


# -------------------------------------------------------
# 2. Identify the optimal point
# -------------------------------------------------------
# Find the row marked with an asterisk
optimal_point <- rfe_data %>%
  dplyr::filter(Selected == "*")

# If no asterisk is found, use the point with the highest accuracy
if (nrow(optimal_point) == 0) {
  message("No optimal point ('*') found. Selecting the maximum accuracy instead.")
  optimal_point <- rfe_data %>%
    dplyr::filter(Accuracy == max(Accuracy, na.rm = TRUE)) %>%
    # If there are ties, take the one with the fewest variables
    dplyr::filter(Variables == min(Variables, na.rm = TRUE))
}

# Extract coordinates of the optimal point for plotting
opt_size     <- optimal_point$Variables
opt_accuracy <- optimal_point$Accuracy

message(paste("Optimal point found at", opt_size, "variables with an accuracy of", round(opt_accuracy, 4)))


# -------------------------------------------------------
# 3. Create the final RFE curve with ggplot2
# -------------------------------------------------------
message("Creating the final RFE performance curve...")

rfe_plot <- ggplot2::ggplot(rfe_data, ggplot2::aes(x = Variables, y = Accuracy)) +
  # Draw the connecting line and points
  ggplot2::geom_line(color = "steelblue", linewidth = 1, alpha = 0.8) +
  ggplot2::geom_point(color = "steelblue", size = 2.5, alpha = 0.8) +
  
  # Add a dashed segment from the x-axis to the optimal point
  ggplot2::geom_segment(
    data = optimal_point,
    ggplot2::aes(x = opt_size, xend = opt_size, y = min(rfe_data$Accuracy), yend = opt_accuracy),
    linetype = "dashed",
    color = "black" # Changed to black
  ) +

  # Add a larger point to highlight the optimum
  ggplot2::geom_point(data = optimal_point, color = "black", size = 4) + # Changed to black
  
  # Add a text label pointing to the optimum
  ggrepel::geom_text_repel(
    data = optimal_point,
    ggplot2::aes(label = paste0("Optimum\n", opt_size, " Variables\nAccuracy = ", sprintf("%.4f", opt_accuracy))),
    color = "black", # Changed to black
    nudge_y = 0.003,
    min.segment.length = 0,
    seed = 123
  ) +
  
  # Define axis labels
  ggplot2::labs(
    x = "Number of Selected Variables",
    y = "Model Accuracy"
  ) +
  
  # Use a clean, minimal theme
  ggplot2::theme_minimal(base_size = 14)


# -------------------------------------------------------
# 4. Save the plot as both PDF and JPG
# -------------------------------------------------------
# Save as PDF (vector format, best for documents)
ggplot2::ggsave(
  filename = "Models/LR_Tuned_Final/RFE_Performance_Curve.pdf",
  plot = rfe_plot,
  width = 10,
  height = 6,
  device = "pdf"
)

# Save as JPG (pixel format, best for web/presentations)
ggplot2::ggsave(
  filename = "Models/LR_Tuned_Final/RFE_Performance_Curve.jpg",
  plot = rfe_plot,
  width = 10,
  height = 6,
  dpi = 300 # Set a good resolution
)

message("\nDone! Plots were saved to your 'Models/LR_Tuned_Final/' directory.")