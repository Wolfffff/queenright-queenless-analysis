# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(patchwork)
library(factoextra)
library(ggplot2)
library(wesanderson)


source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")
bds_means_of_means <- bds_means_of_means %>%
  mutate(QR_Queen_Condition = case_when(
    QR_Queen_Condition == "Queenless" ~ "Queenless\nWorkers",
    QR_Queen_Condition == "Queenright" ~ "Queenright\nWorkers",
    TRUE ~ QR_Queen_Condition # This retains the names for "Queen" and "Keystone"
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright\nWorkes", "Queenless\nWorkers")))

plot_ovaries <- ggplot(bds_means_of_means, aes(x = QR_Queen_Condition, y = ovary_idx)) +
  geom_line(aes(group = Trial), color = "darkgray") +
  geom_point(aes(color = Trial), size = 5) +
  scale_color_manual(values =COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Ovary Index") + 
  CONSISTENT_THEME
plot_ovaries

ggsave("figures/manuscript/ovary_index.png", plot = plot_ovaries, width = 4.25, height = 4.25, units = "in", dpi = 1200)


# Filter and plot
ggplot(bds_means %>% filter(Trial == "RooibosTea" & QR_Queen_Condition != "Queen"), aes(x = ovary_idx, color = QR_Queen_Condition, fill = QR_Queen_Condition)) +
  geom_density(alpha = 0.6, color = "black") +
  scale_color_manual(values = wes_palette("Zissou1", n = length(unique(bds_means$QR_Queen_Condition)), type = "continuous")) +
  scale_fill_manual(values = wes_palette("Zissou1", n = length(unique(bds_means$QR_Queen_Condition)), type = "continuous")) +
  xlab("Ovary Index") +
  ylab("Density") +
  CONSISTENT_THEME +
  REMOVE_HASH_MARKS +
  theme(
    legend.position = "top", # Position the legend at the top
    legend.direction = "horizontal", # Make the legend horizontal
    legend.title = element_blank() # Remove the legend title
  )

ggsave("figures/manuscript/rooibos_ovary_index_distribution.png", width = 4.25, height = 4.25, units = "in", dpi = 1200) 