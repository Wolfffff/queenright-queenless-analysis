# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(factoextra)
library(ggplot2)
library(cowplot)
library(ggbeeswarm)

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

# Add QR_Queen_Condition, Q_QRW_QLW_Keystone and Trial columns
numerical_data$QR_Queen_Condition <- bds_means[complete.cases(numerical_data), ]$QR_Queen_Condition
numerical_data$Q_QRW_QLW_Keystone <- bds_means[complete.cases(numerical_data), ]$Q_QRW_QLW_Keystone
numerical_data$Trial <- bds_means[complete.cases(numerical_data), ]$Trial

# Get PCA scores and add to the data
pca_scores <- data.pca$scores
data_with_pca <- data.frame(bds_means[complete.cases(bds_means[, focal_columns]), ], PC1 = pca_scores[, 1], PC2 = pca_scores[, 2])

# Modify data for plotting - use Q_QRW_QLW_Keystone to include hub bees
data_with_pca$PointSize <- ifelse(data_with_pca$Q_QRW_QLW_Keystone %in% c("Queen", "Keystone"), "big", "small")
data_with_pca$Alpha <- ifelse(data_with_pca$Q_QRW_QLW_Keystone %in% c("Queen", "Keystone"), 1, 0.4)
data_with_pca$Alpha <- as.numeric(data_with_pca$Alpha)
data_with_pca$PointShape <- ifelse(data_with_pca$Q_QRW_QLW_Keystone == "Keystone", "diamond", "circle")

# Create a grouping variable for PCA ellipses that includes hub bees with queenless workers
data_with_pca$PCA_Group <- case_when(
  data_with_pca$Q_QRW_QLW_Keystone == "Queen" ~ "Queen",
  data_with_pca$Q_QRW_QLW_Keystone == "Queenright" ~ "Queenright",
  data_with_pca$Q_QRW_QLW_Keystone %in% c("Keystone", "Queenless") ~ "Queenless"
)

# Create PCA plot with legend on top, highlighting hub bees
pca_plot <- ggplot(data_with_pca, aes(x = PC1, y = PC2, color = Q_QRW_QLW_Keystone, size = PointSize, shape = PointShape)) +
  geom_point(alpha = data_with_pca$Alpha, stroke = 0.5) +
  scale_size_manual(values = c("big" = 2.5, "small" = 1.5), guide = "none") +
  scale_shape_manual(values = c("circle" = 16, "diamond" = 18), guide = "none") +
  scale_color_manual(
    labels = c("Queen", "Queenright Worker", "Queenless Outliers", "Queenless Worker"),
    values = c("Queen" = Q_QRW_KEY_QLW$Q, "Queenright" = Q_QRW_KEY_QLW$QRW, "Keystone" = Q_QRW_KEY_QLW$KEY, "Queenless" = Q_QRW_KEY_QLW$QLW),
    guide = guide_legend(direction = "horizontal", title = "", override.aes = list(alpha = 1, size = 2))
  ) +
  labs(x = paste0("Principal Component 1 (", round(100 * pve[1], 2), "%)"), y = paste0("Principal Component 2 (", round(100 * pve[2], 2), "%)")) +
  CONSISTENT_THEME +
  stat_ellipse(aes(group = PCA_Group), alpha = 1, type = "t", level = 0.95, linetype = "dashed", size = 0.5, color = "black") +
  scale_alpha_continuous() +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = c(1, 1.02),
    legend.justification = c(0.5, 0),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.5, "cm"),
    legend.margin = margin(t = 5, r = 0, b = 10, l = 0)
  ) +
  REMOVE_HASH_MARKS

# Filter data for Worker and Queen (including keystones in worker category for density plot)
data_with_pca_worker <- data_with_pca %>%
  filter(Queen == 0)

data_with_pca_queen <- data_with_pca %>%
  filter(Queen == 1)

# Create beeswarm plot for ovary index, including hub bees as separate group
plot_degree <- ggplot(data_with_pca_worker, aes(x = Q_QRW_QLW_Keystone, y = ovary_idx, fill = Q_QRW_QLW_Keystone, color = Q_QRW_QLW_Keystone)) +
  geom_beeswarm(alpha = 0.7, size = 1.5) +
  scale_fill_manual(
    labels = c("Queenright Worker", "Keystone/Hub Bee", "Queenless Worker"), 
    values = c("Queenright" = Q_QRW_KEY_QLW$QRW, "Keystone" = Q_QRW_KEY_QLW$KEY, "Queenless" = Q_QRW_KEY_QLW$QLW)
  ) +
  scale_color_manual(
    labels = c("Queenright Worker", "Keystone/Hub Bee", "Queenless Worker"), 
    values = c("Queenright" = Q_QRW_KEY_QLW$QRW, "Keystone" = Q_QRW_KEY_QLW$KEY, "Queenless" = Q_QRW_KEY_QLW$QLW)
  ) +
  scale_x_discrete(labels = c("Queenright" = "QR", "Keystone" = "Key", "Queenless" = "QL")) +
  labs(x = "Group", y = "Ovary Index") +
  CONSISTENT_THEME +
  theme(
    plot.margin = unit(c(1, 0, 0, 0), "cm"),
    legend.position = "none"
  ) +
  scale_alpha_continuous() +
  REMOVE_HASH_MARKS

# Combine the two plots
final_layout <- plot_grid(pca_plot, plot_degree, ncol = 2, align = "hv", axis = "tblr")

# Save the figure (taller to accommodate legend)
ggsave("figures/manuscript/figure_3_simplified.jpeg", final_layout, width = 6, height = 3.5, units = "in", dpi = 600)