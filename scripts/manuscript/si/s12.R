# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(reshape2)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

total_nwp_key_assort_per <- nwp[nwp$params == "KeystoneAssPercentile", ]

degree_over_time <- ggplot(total_nwp_key_assort_per, aes(x = as.integer(Hour), y = values, color = Trial, group = Trial)) +
  geom_jitter() +
  geom_smooth() +
  scale_size(range = c(.001, .5)) +
  scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA), expand = c(0, 0)) + # Expand limits to include 0
  labs(color = "") +
  xlab("Hour") +
  ylab("Degree Assortativity Percentile") +
  scale_color_manual(values = COLONY_COLORS) +
  theme_minimal() +
  theme(
    text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top", # Move legend to top right
    legend.justification = c(1, 1), # Align legend to top right
    legend.background = element_rect(fill = alpha("white", 1), color = NA), # Add white background to legend
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"), # Keep vertical grid lines
    panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"), # Keep vertical grid lines
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(), # Remove horizontal grid lines
    axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
    axis.line.y = element_line(color = "black", size = 0.5), # Add y-axis line
    strip.text = element_text(size = 9, face = "bold"), # Make facet plot titles larger and bold
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    plot.margin = unit(c(1, 1, 1, 1), "mm") # Add margin for alignment
  ) +
  guides(alpha = "none", size = "none")

ggsave("figures/manuscript/si/figure_s12.jpeg", width = 8.5, height = 4.5, units = "in", dpi = 600)
