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

col5_with_network_info <- read_csv("data/bds_pooled.csv")

# Define colors
gray_color <- "#4d4d4d"
blue_color <- "#0072B2"

# Degree by Interactions with Queen
deg_by_queen_ints <- ggplot(col5_with_network_info, aes(x = interactions_with_queen, y = Degree)) +
  geom_point(size = 2, alpha = 0.5, color = blue_color, stroke = 0) +
  geom_smooth(method = "lm", se = TRUE, color = gray_color) +
  scale_color_manual(values = c(gray_color, blue_color)) +
  xlab("Total Int. with Queen") +
  ylab("Std. Interactions per Hour") +
  CONSISTENT_THEME +
  REMOVE_HASH_MARKS +
  theme(aspect.ratio = 1)

ov_idx_by_queen_ints <- ggplot(col5_with_network_info, aes(x = interactions_with_queen, y = ovary_wing_ratio)) +
  geom_point(size = 2, alpha = 0.5, color = blue_color, stroke = 0) +
  geom_smooth(method = "lm", se = TRUE, color = gray_color) +
  scale_color_manual(values = c(gray_color, blue_color)) +
  xlab("Total Int. with Queen") +
  ylab("Ovary Index") +
  CONSISTENT_THEME +
  REMOVE_HASH_MARKS +
  theme(aspect.ratio = 1)

move_perc_by_queen_ints <- ggplot(col5_with_network_info, aes(x = interactions_with_queen, y = 100*move_perc)) +
  geom_point(size = 2, alpha = 0.5, color = blue_color, stroke = 0) +
  geom_smooth(method = "lm", se = TRUE, color = gray_color) +
  scale_color_manual(values = c(gray_color, blue_color)) +
  xlab("Total Int. with Queen") +
  ylab("Percent of Time Moving") +
  CONSISTENT_THEME +
  REMOVE_HASH_MARKS +
  theme(aspect.ratio = 1)

size_by_queen_ints <- ggplot(col5_with_network_info, aes(x = interactions_with_queen, y = AverageWingLength)) +
  geom_point(size = 2, alpha = 0.5, color = blue_color, stroke = 0) +
  geom_smooth(method = "lm", se = TRUE, color = gray_color) +
  scale_color_manual(values = c(gray_color, blue_color)) +
  xlab("Total Int. with Queen") +
  ylab("Average Marginal Cell Length") +
  CONSISTENT_THEME +
  REMOVE_HASH_MARKS +
  theme(aspect.ratio = 1)

plot_grid(deg_by_queen_ints, ov_idx_by_queen_ints, move_perc_by_queen_ints, size_by_queen_ints, ncol = 2,align = "hv")

ggsave("figures/manuscript/si/figure_s14.jpeg", width = 4.25, height = 4.25, units = "in", dpi = 1200)
