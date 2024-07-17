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
pca_plot <- ggplot(data_with_pca, aes(x = PC1, y = PC2, color = QR_Queen_Condition, size = PointSize)) +
    geom_point(alpha = data_with_pca$Alpha, stroke = 0) +
    scale_size_manual(values = c("big" = 2.5, "small" = 1.5), guide = "none") +

    scale_color_manual(
        labels = c("Queen", "Queenright Workers", "Queenless Worker"),
        values = c(Q_QRW_KEY_QLW$Q, Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$QLW),
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

# Filter data for workers
data_with_pca_workers <- data_with_pca %>%
    filter(Queen == 0)

# Create density plots for various metrics
plot_degree <- ggplot(data_with_pca_workers, aes(x = Degree, fill = QR_Queen_Condition)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(labels = c("Queenright Workers", "Queenless Workers"), values = c(Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$QLW)) +
    labs(x = "Std. Interactions per Hour", y = "Density") +
    CONSISTENT_THEME +
    theme(
        aspect.ratio = 1,
        plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_alpha_continuous() +
    REMOVE_HASH_MARKS +
        theme(
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "right",
        legend.direction = "horizontal",
        legend.title = element_blank()
    )

plot_dispersion <- ggplot(data_with_pca_workers, aes(x = N90.Day4, fill = QR_Queen_Condition)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c(Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$QLW)) +
    labs(x = "N90 (Dispersion)", y = "Density") +
    CONSISTENT_THEME +
    theme(
        aspect.ratio = 1,
        plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_alpha_continuous() +
    REMOVE_HASH_MARKS

plot_move_perc <- ggplot(data_with_pca_workers, aes(x = move_perc * 100, fill = QR_Queen_Condition)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c(Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$QLW)) +
    labs(x = "Percent of Time Moving", y = "Density") +
    CONSISTENT_THEME +
    theme(
        aspect.ratio = 1,
        plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_alpha_continuous() +
    REMOVE_HASH_MARKS

plot_clustering <- ggplot(data_with_pca_workers, aes(x = clust, fill = QR_Queen_Condition)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c(Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$QLW)) +
    labs(x = "Clustering Coefficient", y = "Density") +
    CONSISTENT_THEME +
    theme(
        aspect.ratio = 1,
        plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_alpha_continuous() +
    REMOVE_HASH_MARKS


legend <- get_legend(plot_degree)

# Step 2: Remove the legend from the first plot
plot_degree <- plot_degree + theme(legend.position = "none")

# Step 3: Create a plot grid without legends
grid_no_legend <- plot_grid(
    plot_degree,
    plot_move_perc,
    plot_dispersion,
    plot_clustering,
    ncol = 2,
    nrow = 2,
    align = "hv",
    axis = "tblr"
)

pca_legend <- get_legend(pca_plot + theme(legend.position = "right"))

# Step 2: Remove the legend from the first plot
pca_plot_no_legend <- pca_plot + theme(legend.position = "none")

# # Step 4: Combine the extracted legend with the plot grid
final_grid <- plot_grid(
    legend,
    grid_no_legend,
    ncol = 1,
    rel_heights = c(0.1, 1) # Adjust the relative heights if necessary
)

final_pca_plot <- plot_grid(pca_legend,pca_plot_no_legend, ncol = 1, align = "tb", rel_heights = c(0.1, 1))

final_layout <- plot_grid(final_pca_plot, final_grid, ncol = 2, align = "tb", rel_heights = c(1, 1))



ggsave("figures/manuscript/figure_3.jpeg", final_layout, width = 8.5, height = 4.25, units = "in", dpi = 600)
