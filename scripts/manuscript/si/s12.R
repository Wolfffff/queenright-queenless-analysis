# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(reshape2)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

nwp <- read_csv("data/TotalNWP.csv")

total_nwp_key_assort_per <- nwp[nwp$params == 'KeystoneAssPercentile',]

ggplot(total_nwp_key_assort_per, aes(x = as.integer(Hour), y = values, group = ID)) +
  geom_jitter(aes(color = QR, alpha = 0.005, size=0.01)) +
  geom_smooth(aes(group = QR, color = QR)) +
  scale_size(range = c(.001, .2)) +
  scale_color_manual(
    labels = c("Queenless Worker"),
    values = c("#161414")) +
  scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA), expand = c(0, 0)) + # Expand limits to include 0
  labs(color = "") +
  xlab("Hour") +
  ylab("Assortativity Percentile") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(1, 1.02), # Move legend to top right
    legend.justification = c(1, 1), # Align legend to top right
    legend.background = element_rect(fill = alpha("white", 1)),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"), # Keep vertical grid lines
    panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"), # Keep vertical grid lines
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(), # Remove horizontal grid lines
    axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
    axis.line.y = element_line(color = "black", size = 0.5), # Add y-axis line
    strip.text = element_text(size = 10, face = "bold"), # Make facet plot titles larger and bold
    panel.grid = element_blank(),
    panel.border = element_blank()
  ) +
  # scale_y_log10() +
  guides(alpha = "none", size = "none")

ggsave("figures/manuscript/si/figure_s12.jpeg", width = 4.25, height = 4.25, units = "in", dpi = 1200)