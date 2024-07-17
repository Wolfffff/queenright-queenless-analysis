# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(cowplot)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

bds_means_of_means <- bds_means_of_means %>%
  mutate(QR_Queen_Condition = case_when(
    QR_Queen_Condition == "Queenless" ~ "Queenless\nWorkers",
    QR_Queen_Condition == "Queenright" ~ "Queenright\nWorkers",
    TRUE ~ QR_Queen_Condition
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright\nWorkers", "Queenless\nWorkers")))


# Plot Degree
plot_move_perc <- ggplot(bds_means_of_means, aes(x = QR_Queen_Condition, y = move_perc)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
    geom_point(aes(color = Trial), size = 3) +
    scale_color_manual(values = COLONY_COLORS) +
    xlab("") +
    ylab("Percent of Time Moving") +
    CONSISTENT_THEME


plot_bouts <- ggplot(bds_means_of_means, aes(x = QR_Queen_Condition, y = boutDegree)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
    geom_point(aes(color = Trial), size = 3) +
    scale_color_manual(values = COLONY_COLORS) +
    xlab("") +
    ylab("Std. Number of Int. Bouts per Hour") +
    CONSISTENT_THEME


final_plot <- plot_grid(plot_move_perc, plot_bouts)

ggsave("figures/manuscript/figure_2_supp.jpeg", plot = final_plot, width = 8.5, height = 4.25,dpi =1200)
