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
source("scripts/manuscript/load_data.R")


# Read the data
nwp <- read_csv("data/TotalNWP.csv")

nwp_means <- nwp %>%
    group_by(Trial, QR, params) %>%
    summarise(mean_value = mean(values, na.rm = TRUE), .groups = "drop")


# nwp_means_transitivity
nwp_means_transitivity <- nwp_means %>%
    filter(params == "Transitivity")

plot_trans <- ggplot(nwp_means_transitivity, aes(x = QR, y = mean_value)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
    geom_point(aes(color = Trial), size = 3) +
    scale_color_manual(values = COLONY_COLORS) +
    xlab("") +
    labs(color = "Source Colony") +
    ylab("Transitivity") +
    theme_minimal() +
    CONSISTENT_THEME +
    scale_x_discrete(labels = c("QR Network", "QL Network")) + # Update x-axis labels to be two lines
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
# nwp_means_efficiency

nwp_means_eff <- nwp_means %>%
    filter(params == "GlobalEfficiency")

plot_eff <- ggplot(nwp_means_eff, aes(x = QR, y = mean_value)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
    geom_point(aes(color = Trial), size = 3) +
    scale_color_manual(values = COLONY_COLORS) +
    xlab("") +
    labs(color = "Source Colony") +
    ylab("Efficiency") +
    theme_minimal() +
    CONSISTENT_THEME +
    scale_x_discrete(labels = c("QR Network", "QL Network")) + # Update x-axis labels to be two lines
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

nwp_means_assortativity <- nwp_means %>%
    filter(params == "Assortativity")

plot_assort <- ggplot(nwp_means_assortativity, aes(x = QR, y = mean_value)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
    geom_point(aes(color = Trial), size = 3) +
    scale_color_manual(values = COLONY_COLORS) +
    xlab("") +
    labs(color = "Source Colony") +
    ylab("Assortativity") +
    theme_minimal() +
    CONSISTENT_THEME +
    scale_x_discrete(labels = c("QR Network", "QL Network")) + # Update x-axis labels to be two lines
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

# Sample data for beeswarm plot
TotalCentSum <- bds_means
# Set order of levels
TotalCentSum$Q_QRW_QLW_Keystone
grouped_sum <- TotalCentSum %>%
  group_by(Trial, Q_QRW_QLW_Keystone) %>%
  summarise(Degree = mean(Degree), .groups = "drop")

# Note: Adjust the labels in scale_x_discrete to match your actual Q_QRW_QLW_Keystone values.
# Betweenness Centrality vs Degree Centrality
bds_means$Point_Size <- ifelse(bds_means$Q_QRW_QLW_Keystone %in% c("Queen","Keystone"), 3, 1)


plot_centrality <- ggplot(bds_means, aes(y = Between, x = Degree, group = Q_QRW_QLW_Keystone)) +
  geom_point(aes(color = Q_QRW_QLW_Keystone), alpha = .75, size = bds_means$Point_Size, stroke = 0) +
  scale_color_manual(
    labels = c("Queen", "Queenright Worker", "Keystone Individual", "Queenless Worker"),
    values = c(Q_QRW_KEY_QLW$Q, Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$KEY, Q_QRW_KEY_QLW$QLW),
  ) +
  scale_x_continuous() + # Use a continuous scale for x
  scale_y_continuous() + # Use a continuous scale for y
  xlab("Std. Interactions per Hour") +
  ylab("Betweenness Centrality") + # Adjust axis labels
  CONSISTENT_THEME


# Degree by move_perc with linear models for
plot_lm <- ggplot(bds_means, aes(y = move_perc*100, x = Degree, color = Q_QRW_QLW_Keystone)) +
  geom_point(aes(size = Q_QRW_QLW_Keystone %in% c("Queen", "Keystone")),stroke=0,alpha = .75) +  # Conditional size for "Queen"
  geom_smooth(data = subset(bds_means, Q_QRW_QLW_Keystone %in% c("Queenright", "Queen")), method = "lm", se = TRUE, color = "#642076") +  # Red line for Queen and Queenright Worker combined
  geom_smooth(data = subset(bds_means, Q_QRW_QLW_Keystone %in% c("Queenless", "Keystone")), method = "lm", se = TRUE, color = "#E68200") +  # Green line for Queenless Worker and Influencer combined
  scale_color_manual(
    labels = c("Queen", "Queenright Worker", "Keystone Individual", "Queenless Worker"),
    values = c(Q_QRW_KEY_QLW$Q, Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$KEY, Q_QRW_KEY_QLW$QLW),
  ) +
  scale_size_manual(values = c(`TRUE` = 3, `FALSE` = 1)) +  # Adjust the size for "Queen"
  guides(size = FALSE) +  # Optionally hide the size legend
  ylab("Percent of Time Moving") +
  xlab("Std. Interactions per Hour") +
  CONSISTENT_THEME

# Create the top row with plot_centrality and plot_lm
top_row <- plot_grid(plot_lm,plot_centrality, ncol = 2, align = 'h')

# Create the bottom row with plot_trans, plot_eff, and plot_assort
bottom_row <- plot_grid(plot_trans, plot_eff, plot_assort, ncol = 3, align = 'h')

# Combine the top and bottom rows into the final layout
final_layout <- plot_grid(top_row, bottom_row, ncol = 1, align = 'hv', rel_heights = c(1.5, 1))

# Save the final layout with adjusted margins
save_plot("figures/manuscript/figure_5.jpeg", final_layout + theme(plot.margin = unit(c(0,0,0,0), "cm")), base_width = 8.5, base_height = 6, dpi = 600)

