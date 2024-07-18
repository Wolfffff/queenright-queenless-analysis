# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(patchwork)
library(factoextra)
library(ggplot2)
library(cowplot)

# Source scripts for constants and data
source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

# Define focal columns and prepare numerical data
focal_columns <- c("Degree", "Close", "Eigen", "Between", "boutDegree", "boutBetween", "boutClose", "boutEigen", "bodyDegree", "bodyBetween", "bodyClose", "bodyEigen", "mean_vel", "move_perc", "N90.Day4", "Initiation.Freq", "clust")
numerical_data <- bds_means[, focal_columns]
numerical_data <- numerical_data[complete.cases(numerical_data), ]
data_normalized <- scale(numerical_data)

# Perform PCA
data.pca <- princomp(data_normalized)
pve <- summary(data.pca)$sdev^2 / sum(summary(data.pca)$sdev^2)

# Plot Eigenvalues
fviz_eig(data.pca, addlabels = TRUE)

# Plot PCA variables
fviz_pca_var(data.pca, col.var = "black")

# Add QR_Queen_Condition and Trial columns
numerical_data$QR_Queen_Condition <- bds_means[complete.cases(numerical_data), ]$QR_Queen_Condition
numerical_data$Trial <- bds_means[complete.cases(numerical_data), ]$Trial

# Plot PCA individuals
pca_ind_plot <- fviz_pca_ind(data.pca,
    label = "none", habillage = numerical_data$QR_Queen_Condition,
    addEllipses = TRUE, ellipse.level = 0.95, palette = "Dark2"
) + theme_minimal() + labs(title = "Standardized PCA")

# Get PCA scores and add to the data
pca_scores <- data.pca$scores
data_with_pca <- data.frame(bds_means, PC1 = pca_scores[, 1], PC2 = pca_scores[, 2])

# Modify data for plotting
data_with_pca$PointSize <- ifelse(data_with_pca$QR_Queen_Condition %in% c("Queen"), "big", "small")
data_with_pca$Alpha <- ifelse(data_with_pca$QR_Queen_Condition %in% c("Queen"), 1, 0.4)
data_with_pca$Alpha <- as.numeric(data_with_pca$Alpha)

# Create PCA plot
pca_plot <- ggplot(data_with_pca, aes(x = PC1, y = PC2, color = Trial, size = PointSize)) +
    geom_point(alpha = data_with_pca$Alpha, stroke = 0) +
    scale_size_manual(values = c("big" = 2.5, "small" = 1.5), guide = "none") +
    scale_color_manual(
        labels = c("Col 1", "Col 2", "Col 3", "Col 4", "Col 5"),
        values = COLONY_COLORS,
        guide = guide_legend(direction = "horizontal", title = "")
    ) +
    labs(x = paste0("Principal Component 1 (", round(100 * pve[1], 2), "%)"), y = paste0("Principal Component 2 (", round(100 * pve[2], 2), "%)")) +
    CONSISTENT_THEME +
    stat_ellipse(alpha = 1, type = "t", level = 0.95, linetype = "dashed", size = 0.5) +
    scale_alpha_continuous() +
    theme(
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "top",
        legend.direction = "horizontal",
    ) +
    REMOVE_HASH_MARKS


ggsave("figures/manuscript/si/figure_s8.jpeg", pca_plot, width = 5.5, height = 5.5, units = "in", dpi = 1200)
