# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(patchwork)
library(factoextra)
library(ggplot2)
library(ggbeeswarm)

source("scripts/manuscript/constants.R")

# Read the data
nwp <- read_csv("data/TotalNWP.csv")

nwp_means <- nwp %>%
    group_by(Trial, QR, params) %>%
    summarise(mean_value = mean(values, na.rm = TRUE), .groups = "drop")


# nwp_means_transitivity
nwp_means_transitivity <- nwp_means %>%
    filter(params == "Transitivity")

plot_swarm <- ggplot(nwp_means_transitivity, aes(x = QR, y = mean_value)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 1) +
    geom_point(aes(color = Trial), size = 5) +
    scale_color_manual(values = COLONY_COLORS) +
    xlab("") +
    labs(color = "Source Colony") +
    ylab("Transitivity") +
    theme_minimal() +
    CONSISTENT_THEME +
    scale_x_discrete(labels = c("Queenright\nWorker", "Queenless\nWorker")) + # Update x-axis labels to be two lines
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

ggsave("figures/manuscript/figure_5_supp_nwp_transitivity.png", plot_swarm, width = 6, height = 4, dpi = 300)

# nwp_means_efficiency

nwp_means_eff <- nwp_means %>%
    filter(params == "GlobalEfficiency")

plot_swarm <- ggplot(nwp_means_eff, aes(x = QR, y = mean_value)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 1) +
    geom_point(aes(color = Trial), size = 5) +
    scale_color_manual(values = COLONY_COLORS) +
    xlab("") +
    labs(color = "Source Colony") +
    ylab("Efficiency") +
    theme_minimal() +
    CONSISTENT_THEME +
    scale_x_discrete(labels = c("Queenright\nWorker", "Queenless\nWorker")) + # Update x-axis labels to be two lines
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

ggsave("figures/manuscript/figure_5_supp_nwp_eff.png", plot_swarm, width = 6, height = 4, dpi = 300)

nwp_means_assortativity <- nwp_means %>%
    filter(params == "Assortativity")

plot_swarm <- ggplot(nwp_means_assortativity, aes(x = QR, y = mean_value)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 1) +
    geom_point(aes(color = Trial), size = 5) +
    scale_color_manual(values = COLONY_COLORS) +
    xlab("") +
    labs(color = "Source Colony") +
    ylab("Assortativity") +
    theme_minimal() +
    CONSISTENT_THEME +
    scale_x_discrete(labels = c("Queenright\nWorker", "Queenless\nWorker")) + # Update x-axis labels to be two lines
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

ggsave("figures/manuscript/figure_5_supp_nwp_assortativity.png", plot_swarm, width = 6, height = 4, dpi = 300)
