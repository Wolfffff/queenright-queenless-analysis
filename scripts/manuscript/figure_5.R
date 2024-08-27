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

nwp_means <- nwp %>%
  group_by(Trial, QR, params) %>%
  summarise(mean_value = mean(values, na.rm = TRUE), .groups = "drop")

# nwp_means_transitivity
nwp_means_transitivity <- nwp_means %>%
  filter(params == "Transitivity")

plot_trans <- ggplot(nwp_means_transitivity, aes(x = QR, y = mean_value)) +
  geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
  geom_point(aes(color = Trial, fill = Trial), size = 3,shape = 21, stroke = 0.2, color = "black") +
  scale_fill_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Transitivity") +
  theme_minimal() +
  CONSISTENT_THEME +
  scale_x_discrete(labels = c("Queenright\nNetwork", "Queenless\nNetwork")) + # Update x-axis labels to be two lines
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
# nwp_means_efficiency

nwp_means_eff <- nwp_means %>%
  filter(params == "GlobalEfficiency")

plot_eff <- ggplot(nwp_means_eff, aes(x = QR, y = mean_value)) +
  geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
  geom_point(aes(color = Trial, fill = Trial), size = 3,shape = 21, stroke = 0.2, color = "black") +
  scale_fill_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Efficiency") +
  theme_minimal() +
  CONSISTENT_THEME +
  scale_x_discrete(labels = c("Queenright\nNetwork", "Queenless\nNetwork")) + # Update x-axis labels to be two lines
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

nwp_means_assortativity <- nwp_means %>%
  filter(params == "Assortativity")

plot_assort <- ggplot(nwp_means_assortativity, aes(x = QR, y = mean_value)) +
  geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
  geom_point(aes(color = Trial, fill = Trial), size = 3,shape = 21, stroke = 0.2, color = "black") +
  scale_fill_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Degree Assortativity") +
  theme_minimal() +
  CONSISTENT_THEME +
  scale_x_discrete(labels = c("Queenright\nNetwork", "Queenless\nNetwork")) + # Update x-axis labels to be two lines
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

# Create the bottom row with plot_trans, plot_eff, and plot_assort
bottom_row <- plot_grid(plot_trans, plot_eff, plot_assort, ncol = 3, align = "hv")

# Save the final layout with adjusted margins
save_plot("figures/manuscript/figure_5_bottom.jpeg", bottom_row + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")), base_width = 8.5, base_height = 2.8333, dpi = 600)
