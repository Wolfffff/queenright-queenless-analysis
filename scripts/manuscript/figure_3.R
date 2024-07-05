# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(patchwork)
library(factoextra)
library(ggplot2)

source("scripts/manuscript/constants.R")

# Read the data
bds <- read_csv("data/BigDataSheet.csv")
bds <- bds %>%
    mutate(Trial = str_extract(Bee, ".+?(?=_)"))

bds <- bds %>%
    mutate(QR_Queen_Condition = case_when(
        QR == 0 & Queen == 0 ~ "Queenless",
        QR == 1 & Queen == 0 ~ "Queenright",
        Queen == 1 ~ "Queen"
    )) %>%
    mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queenright", "Queenless", "Queen")))

# Calculate mean values of QR, Queen, and Degree for each Bee (QR and Queen are binary so mean is the value itself)
bds_means <- bds %>%
    group_by(Bee) %>%
    summarise(across(c(Degree, Close, Eigen, Between, QR, Queen, boutDegree, boutBetween, boutClose, boutEigen, bodyDegree, bodyBetween, bodyClose, bodyEigen, AverageBoutLength, Presence, AntPresence, mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq, clust), mean, na.rm = TRUE))

# Extract Trial information from Bee column
bds_means <- bds_means %>%
    mutate(Trial = str_extract(Bee, ".+?(?=_)"))

# Define QR_Queen_Condition based on QR and Queen values
bds_means <- bds_means %>%
    mutate(QR_Queen_Condition = case_when(
        QR == 0 & Queen == 0 ~ "Queenless",
        QR == 1 & Queen == 0 ~ "Queenright",
        Queen == 1 ~ "Queen"
    )) %>%
    mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queenright", "Queenless", "Queen")))

# Create ID column for aggregation
bds_means <- bds_means %>%
    mutate(ID = paste(Trial, QR_Queen_Condition))

# Calculate mean of means for each ID
bds_mean_of_means <- bds_means %>%
    group_by(ID) %>%
    summarise(across(c(Degree, Close, Eigen, Between, QR, Queen, boutDegree, boutBetween, boutClose, boutEigen, bodyDegree, bodyBetween, bodyClose, bodyEigen, AverageBoutLength, Presence, AntPresence, mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq, clust), mean, na.rm = TRUE))

# Extract Trial information from ID column
bds_mean_of_means <- bds_mean_of_means %>%
    mutate(Trial = str_extract(ID, ".+?(?= )"))

# Define QR_Queen_Condition based on QR and Queen values for mean of means
bds_mean_of_means <- bds_mean_of_means %>%
    mutate(QR_Queen_Condition = case_when(
        QR == 0 & Queen == 0 ~ "Queenless",
        QR == 1 & Queen == 0 ~ "Queenright",
        Queen == 1 ~ "Queen"
    )) %>%
    mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queenright", "Queenless", "Queen")))

focal_columns <- colnames(bds_means)[c(2:5, 8:15, 19:21, 23:24)]
numerical_data <- bds_means[, focal_columns]
numerical_data <- numerical_data[complete.cases(numerical_data), ]
data_normalized <- scale(numerical_data)
data.pca <- princomp(data_normalized)
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")
numerical_data$QR <- bds_means[complete.cases(bds_means), ]$QR
numerical_data$Trial <- bds_means[complete.cases(bds_means), ]$Trial
numerical_data$Queen <- bds_means[complete.cases(bds_means), ]$Queen

numerical_data <- numerical_data %>%
    mutate(QR_Queen_Condition = case_when(
        QR != 1 & Queen != 1 ~ "Queenless",
        QR == 1 & Queen != 1 ~ "Queenright",
        Queen == 1 ~ "Queen"
    )) %>%
    mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queenright", "Queenless", "Queen")))


fviz_pca_ind(data.pca,
    label = "none", habillage = numerical_data$QR_Queen_Condition,
    addEllipses = TRUE, ellipse.level = 0.95, palette = "Dark2"
) + theme_minimal() + labs(title = "Standardized PCA")


fviz_pca_ind(data.pca,
    label = "none", habillage = numerical_data$Trial,
    addEllipses = TRUE, ellipse.level = 0.95, palette = "Dark2"
) + theme_minimal() + labs(title = "Standardized PCA")

pca_scores <- data.pca$scores
data_with_pca <- data.frame(bds_means, PC1 = pca_scores[, 1], PC2 = pca_scores[, 2])

# Create the plot
data_with_pca$Queen <- as.logical(data_with_pca$Queen)
data_with_pca$PointSize <- ifelse(data_with_pca$Queen, "big", "small")
data_with_pca$Alpha <- ifelse(data_with_pca$Queen, 1, 0.4)
data_with_pca$Alpha <- as.numeric(data_with_pca$Alpha)

pca_plot <- ggplot(data_with_pca, aes(x = PC1, y = PC2, color = QR_Queen_Condition, size = PointSize)) +
    geom_point(alpha = data_with_pca$Alpha, stroke = 0) +
    scale_size_manual(values = c("big" = 2.5, "small" = 1.5)) +
    theme_minimal() +
    scale_color_manual(
        labels = c("Queenright Worker", "Queenless Worker", "Queen"),
        values = c(Q_QRW_INT_QLW$QRW, Q_QRW_INT_QLW$QLW, Q_QRW_INT_QLW$Q),
        guide = guide_legend(direction = "horizontal")
    ) +
    labs(x = "Principal Component 1", y = "Principal Component 2") +
    CONSISTENT_THEME +
    stat_ellipse(alpha = 1, type = "t", level = 0.95, linetype = "dashed", size = .5) +
    scale_alpha_continuous() + # Explicit control over alpha scale theme(
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Get density plot grouped by QR_Queen_Condition, only workers
data_with_pca_workers <- data_with_pca %>%
    filter(Queen == 0)
plot_degree <- ggplot(data_with_pca_workers, aes(x = Degree, fill = QR_Queen_Condition)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c(Q_QRW_INT_QLW$QRW, Q_QRW_INT_QLW$QLW)) +
    theme_minimal() +
    labs(x = "Std. Number of Int. per Hour", y = "Density") +
    CONSISTENT_THEME +
    theme(
        aspect.ratio = 1,
        plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_alpha_continuous() # Explicit control over alpha scale

# Density plot for N90.Day4 (Dispersion)
plot_dispersion <- ggplot(data_with_pca_workers, aes(x = N90.Day4, fill = QR_Queen_Condition)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c(Q_QRW_INT_QLW$QRW, Q_QRW_INT_QLW$QLW)) +
    theme_minimal() +
    labs(x = "N90 (Dispersion)", y = "Density") +
    CONSISTENT_THEME +
    theme(
        aspect.ratio = 1,
        plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_alpha_continuous() # Explicit control over alpha scale

# Density plot for move_perc (Percent of Time Moving)
plot_move_perc <- ggplot(data_with_pca_workers, aes(x = move_perc, fill = QR_Queen_Condition)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c(Q_QRW_INT_QLW$QRW, Q_QRW_INT_QLW$QLW)) +
    theme_minimal() +
    labs(x = "Percent of Time Moving", y = "Density") +
    CONSISTENT_THEME +
    theme(
        aspect.ratio = 1,
        plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_alpha_continuous() # Explicit control over alpha scale

# Density plot for clust (Clustering Coefficient)
plot_clustering <- ggplot(data_with_pca_workers, aes(x = clust, fill = QR_Queen_Condition)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c(Q_QRW_INT_QLW$QRW, Q_QRW_INT_QLW$QLW)) +
    theme_minimal() +
    labs(x = "Clustering Coefficient", y = "Density") +
    CONSISTENT_THEME +
    theme(
        aspect.ratio = 1,
        plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_alpha_continuous() # Explicit control over alpha scale

# Create the top row with two plots side by side
grid <- plot_grid(plot_degree, plot_move_perc, plot_dispersion, plot_clustering, ncol = 2, nrow = 2, align = 'hv', axis = 'tblr')

# Stack pca_plot above or below the grid, ensuring vertical alignment of x axes
final_layout <- plot_grid(pca_plot, grid, ncol = 2, align = 'tb', rel_heights = c(1, 2))

# Save the final layout
# Assuming final_layout is a ggplot object
final_layout <- final_layout + theme(plot.margin = margin(1, 1, 1, 1, "cm")) # Add 1cm margins

# Now save the plot with ggsave without tight_layout()
ggsave("figures/manuscript/figure_3.jpeg", final_layout, width = 8.5, height = 4.25, units = "in", dpi = 600)
