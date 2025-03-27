# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(patchwork)
library(factoextra)
library(ggplot2)
library(wesanderson)
setwd("~/SuppFiguresNew_121224")
source("constants.R")
source("load_data.R")
bds_means_of_means <- bds_means_of_means %>%
  mutate(QR_Queen_Condition = case_when(
    QR_Queen_Condition == "Queenless" ~ "Queenless\nWorker",
    QR_Queen_Condition == "Queenright" ~ "Queenright\nWorker",
    TRUE ~ QR_Queen_Condition # This retains the names for "Queen" and "Keystone"
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright\nWorker", "Queenless\nWorker")))

plot_ovaries <- ggplot(bds_means_of_means, aes(x = QR_Queen_Condition, y = ovary_idx)) +
  geom_line(aes(group = Trial), color = "darkgray") +
  geom_point(aes(color = Trial), size = 5) +
  scale_color_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Ovary Index") +
  CONSISTENT_THEME
plot_ovaries

ggsave("ovary_index_by_status.jpeg", plot = plot_ovaries, width = 4.5, height = 4.5, units = "in", dpi = 600)
