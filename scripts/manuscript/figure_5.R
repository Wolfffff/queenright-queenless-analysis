# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(patchwork)
library(factoextra)
library(ggplot2)
library(ggbeeswarm)
library(cowplot)

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
  summarise(across(c(Degree, Close, Eigen, Between, QR, Queen, boutDegree, boutBetween, boutClose, boutEigen, bodyDegree, bodyBetween, bodyClose, bodyEigen, AverageBoutLength, Presence, AntPresence, mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq, clust, Infl), mean, na.rm = TRUE))

# Extract Trial information from Bee column
bds_means <- bds_means %>%
  mutate(Trial = str_extract(Bee, ".+?(?=_)"))

# Define QR_Queen_Condition based on QR and Queen values
bds_means <- bds_means %>%
  mutate(QR_Queen_Inf = case_when(
    QR == 0 & Infl == 0 ~ "Queenless Worker",
    QR == 1 & Queen == 0 ~ "Queenright Worker",
    Queen == 1 ~ "Queen",
    QR == 0 & Infl == 1 ~ "Influencer",
    TRUE ~ NA_character_ # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Inf = factor(QR_Queen_Inf, levels = c("Queenless Worker", "Queenright Worker", "Queen", "Influencer")))

# Create ID column for aggregation
bds_means <- bds_means %>%
  mutate(ID = paste(Trial, QR_Queen_Inf))

# Calculate mean of means for each ID
bds_mean_of_means <- bds_means %>%
  group_by(ID) %>%
  summarise(across(c(Infl, Degree, Close, Eigen, Between, QR, Queen, boutDegree, boutBetween, boutClose, boutEigen, bodyDegree, bodyBetween, bodyClose, bodyEigen, AverageBoutLength, Presence, AntPresence, mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq, clust), mean, na.rm = TRUE))

# Extract Trial information from ID column
bds_mean_of_means <- bds_mean_of_means %>%
  mutate(Trial = str_extract(ID, ".+?(?= )"))

# Define QR_Queen_Inf based on QR and Queen values for mean of means
bds_mean_of_means <- bds_mean_of_means %>%
  mutate(QR_Queen_Inf = case_when(
    QR == 0 & Infl == 0 ~ "Queenless Worker",
    QR == 1 & Queen == 0 ~ "Queenright Worker",
    Queen == 1 ~ "Queen",
    QR == 0 & Infl == 1 ~ "Influencer",
    TRUE ~ NA_character_ # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Inf = factor(QR_Queen_Inf, levels = c("Queenright Worker", "Queenless Worker", "Queen", "Influencer")))

# Sample data for beeswarm plot
TotalCentSum <- bds_means %>%
  filter(QR_Queen_Inf %in% c("Queenless Worker", "Queenright Worker"))

# Set order of levels
TotalCentSum$QR_Queen_Inf <- factor(TotalCentSum$QR_Queen_Inf, levels = c("Queenright Worker", "Queenless Worker"))

grouped_sum <- TotalCentSum %>%
  group_by(Trial, QR_Queen_Inf) %>%
  summarise(Degree = mean(Degree), .groups = "drop")

# Plot the data
plot_swarm <- ggplot(TotalCentSum, aes(x = QR_Queen_Inf, y = Degree)) +
  geom_beeswarm(aes(color = Trial), stroke = 0, size = 1, alpha = .3, method = "center") +
  geom_line(data = grouped_sum[grouped_sum$QR_Queen_Inf %in% c("Queenless Worker", "Queenright Worker"), ], aes(group = Trial), linetype = "solid", color = "darkgray") +
  geom_point(data = grouped_sum[grouped_sum$QR_Queen_Inf %in% c("Queenless Worker", "Queenright Worker"), ], aes(group = Trial, color = Trial), size = 3) +
  scale_color_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Std. Number of Int. per Hour") +
  theme_minimal() +
  CONSISTENT_THEME +
  scale_x_discrete(labels = c("Queenright\nWorker", "Queenless\nWorker")) + # Update x-axis labels to be two lines
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

# Note: Adjust the labels in scale_x_discrete to match your actual QR_Queen_Inf values.
# Betweenness Centrality vs Degree Centrality
bds_means$Point_Size <- ifelse(bds_means$QR_Queen_Inf == "Queen", 3.5, 2)


plot_centrality <- ggplot(bds_means, aes(x = Degree, y = Between, group = QR_Queen_Inf)) +
  geom_point(aes(color = QR_Queen_Inf), alpha = .75, size = bds_means$Point_Size, stroke = 0) +
  scale_color_manual(
    labels = c("Queenright Worker", "Queenless Worker", "Queen", "Influencer"),
    values = c(Q_QRW_INT_QLW$QRW, Q_QRW_INT_QLW$QLW, Q_QRW_INT_QLW$Q, Q_QRW_INT_QLW$INF),
  ) +
  scale_x_continuous() + # Use a continuous scale for x
  scale_y_continuous() + # Use a continuous scale for y
  xlab("Std. Number of Int. per Hour") +
  ylab("Betweenness Centrality") + # Adjust axis labels
  CONSISTENT_THEME


# Combine the plots using cowplot's plot_grid
aligned_plots <- plot_grid(plot_trans, plot_eff, plot_assort, plot_centrality, align = "hv", ncol = 4)

# Save the combined plot using cowplot's save_plot function for better handling of cowplot objects
save_plot("figures/manuscript/figure_5.jpeg", aligned_plots + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")), base_width = 8.5, base_height = 2.125, dpi = 1200)
