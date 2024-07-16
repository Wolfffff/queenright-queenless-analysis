# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(patchwork)
library(factoextra)
library(ggplot2)
library(cowplot)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

focal_columns <- colnames(bds_means)[c(2:5, 8:15, 19:21, 23:24)]
numerical_data <- bds_means[, focal_columns]
numerical_data <- numerical_data[complete.cases(numerical_data), ]
data_normalized <- scale(numerical_data)
data.pca <- princomp(data_normalized)

pve <- summary(data.pca)$sdev^2 / sum(summary(data.pca)$sdev^2)
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")

numerical_data$Q_QRW_QLW_Keystone <- bds_means[complete.cases(bds_means), ]$Q_QRW_QLW_Keystone
numerical_data$Trial <- bds_means[complete.cases(bds_means), ]$Trial


fviz_pca_ind(data.pca,
    label = "none", habillage = numerical_data$Q_QRW_QLW_Keystone,
    addEllipses = TRUE, ellipse.level = 0.95, palette = "Dark2"
) + theme_minimal() + labs(title = "Standardized PCA")

pca_scores <- data.pca$scores
data_with_pca <- data.frame(bds_means, PC1 = pca_scores[, 1], PC2 = pca_scores[, 2])

# Create the plot
data_with_pca$PointSize <- ifelse(data_with_pca$Q_QRW_QLW_Keystone %in% c("Keystone", "Queen"), "big", "small")
data_with_pca$Alpha <- ifelse(data_with_pca$Q_QRW_QLW_Keystone %in% c("Keystone", "Queen"), 1, 0.4)
data_with_pca$Alpha <- as.numeric(data_with_pca$Alpha)

pca_plot <- ggplot(data_with_pca, aes(x = PC1, y = PC2, color = Q_QRW_QLW_Keystone, size = PointSize)) +
    geom_point(alpha = data_with_pca$Alpha, stroke = 0) +
    scale_size_manual(values = c("big" = 2.5, "small" = 1.5), guide = "none") +
    theme_minimal() +
    scale_color_manual(
        labels = c("Queen", "Queenright\nWorker", "Keystone\nIndividual", "Queenless\nWorker"),
        values = c(Q_QRW_KEY_QLW$Q, Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$KEY, Q_QRW_KEY_QLW$QLW),
        guide = guide_legend(direction = "horizontal",title="")
    ) +
    labs(x = paste0("Principal Component 1 (", round(100 * pve[1], 2), "%)"), y = paste0("Principal Component 2 (", round(100 * pve[2], 2), "%)")) +
    CONSISTENT_THEME +
    stat_ellipse(alpha = 1, type = "t", level = 0.95, linetype = "dashed", size = 0.5) +
    scale_alpha_continuous() +
    theme(
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "top"
    ) +
    REMOVE_HASH_MARKS


# Get density plot grouped by QR_Queen_Condition, only workers
data_with_pca_workers <- data_with_pca %>%
    filter(Queen == 0)

plot_degree <- ggplot(data_with_pca_workers, aes(x = Degree, fill = QR_Queen_Condition)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c(Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$QLW)) +
    theme_minimal() +
    labs(x = "Std. Interactions per Hour", y = "Density") +
    CONSISTENT_THEME +
    theme(
        aspect.ratio = 1,
        plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_alpha_continuous() +
    REMOVE_HASH_MARKS

# Density plot for N90.Day4 (Dispersion)
plot_dispersion <- ggplot(data_with_pca_workers, aes(x = N90.Day4, fill = QR_Queen_Condition)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c(Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$QLW)) +
    theme_minimal() +
    labs(x = "N90 (Dispersion)", y = "Density") +
    CONSISTENT_THEME +
    theme(
        aspect.ratio = 1,
        plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_alpha_continuous() +
    REMOVE_HASH_MARKS

# Density plot for move_perc (Percent of Time Moving)
plot_move_perc <- ggplot(data_with_pca_workers, aes(x = move_perc*100, fill = QR_Queen_Condition)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c(Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$QLW)) +
    theme_minimal() +
    labs(x = "Percent of Time Moving", y = "Density") +
    CONSISTENT_THEME +
    theme(
        aspect.ratio = 1,
        plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_alpha_continuous() +
    REMOVE_HASH_MARKS

# Density plot for clust (Clustering Coefficient)
plot_clustering <- ggplot(data_with_pca_workers, aes(x = clust, fill = QR_Queen_Condition)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c(Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$QLW)) +
    theme_minimal() +
    labs(x = "Clustering Coefficient", y = "Density") +
    CONSISTENT_THEME +
    theme(
        aspect.ratio = 1,
        plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_alpha_continuous() +
    REMOVE_HASH_MARKS


# Create the top row with two plots side by side
grid <- plot_grid(plot_degree, plot_move_perc, plot_dispersion, plot_clustering, ncol = 2, nrow = 2, align = 'hv', axis = 'tblr')

# Stack pca_plot above or below the grid, ensuring vertical alignment of x axes
final_layout <- plot_grid(pca_plot, grid, ncol = 2, align = 'tb', rel_heights = c(1, 2))

# Save the final layout
# Assuming final_layout is a ggplot object
final_layout <- final_layout + theme(plot.margin = margin(1, 1, 1, 1, "cm")) # Add 1cm margins

# Now save the plot with ggsave without tight_layout()
ggsave("figures/manuscript/figure_3.jpeg", final_layout, width = 8.5, height = 4.25, units = "in", dpi = 600)

