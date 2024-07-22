# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Source scripts for constants and data
source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

# Define focal columns and prepare numerical data
focal_columns <- c("Degree", "Close", "Eigen", "Between", "boutDegree", "boutBetween", "boutClose", "boutEigen", "bodyDegree", "bodyBetween", "bodyClose", "bodyEigen", "mean_vel", "move_perc", "N90.Day4", "Initiation.Freq", "clust")
numerical_data <- bds_means[, focal_columns]
numerical_data <- numerical_data[complete.cases(numerical_data), ]
data_normalized <- scale(numerical_data)

# New human-readable column names
human_readable_names <- c(
  "Degree Centrality", "Closeness Centrality", "Eigenvector Centrality", "Betweenness Centrality",
  "Bout Degree Centrality", "Bout Betweenness Centrality", "Bout Closeness Centrality", "Bout Eigenvector Centrality",
  "Body Degree Centrality", "Body Betweenness Centrality", "Body Closeness Centrality", "Body Eigenvector Centrality",
  "Mean Velocity", "Movement Percentage", "N90 Day 4", "Initiation Frequency", "Clustering Coefficient"
)

# Assign new human-readable names to the columns of numerical_data
colnames(numerical_data) <- human_readable_names
# Perform PCA
data.pca <- prcomp(data_normalized, scale = TRUE)
pca_data <- as.data.frame(data.pca$x)
rownames(data.pca$rotation) <- human_readable_names

# Create a data frame for PCA variable loadings with updated, human-readable variable names
loadings <- as.data.frame(data.pca$rotation)
loadings$variables <- rownames(loadings)


# Custom theme
CONSISTENT_THEME <- theme_minimal() +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = "top",
    legend.direction = "horizontal"
  )

# Create PCA biplot using ggplot2
library(ggrepel)

pca_biplot <- ggplot() +
  geom_segment(
    data = loadings, aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5),
    arrow = arrow(length = unit(0.1, "cm")), color = "black"
  ) +
  geom_label_repel(
    data = loadings, aes(x = PC1 * 5, y = PC2 * 5, label = variables),
    box.padding = 0.1, # Increase padding around text
    point.padding = 0.1, # Increase space between the label and its point
    segment.color = "grey50",
    min.segment.length = 0,
    max.iter = 10000,
    size = 2.25, # Increase label size
    fill = "#FFFFFFE6", # Semi-transparent white background for the labels
    label.size = 0.1, # Size of the border around the label
    label.padding = unit(0.1, "lines") # Padding inside the label
  ) +
  labs(
    x = paste0("Principal Component 1 (", round(summary(data.pca)$importance[2, 1] * 100, 2), "%)"),
    y = paste0("Principal Component 2 (", round(summary(data.pca)$importance[2, 2] * 100, 2), "%)"),
    # title = "PCA Biplot"
  ) +
  CONSISTENT_THEME
# Save the PCA biplot as figure_s7.jpeg
ggsave("figures/manuscript/si/figure_s7.jpeg", pca_biplot, width = 8.5, height = 4.5, units = "in", dpi = 600)
