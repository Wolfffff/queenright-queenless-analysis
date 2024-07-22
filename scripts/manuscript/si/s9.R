library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggpubr)

source("scripts/manuscript/constants.R")

# Read the dataset
wing_morphometrics <- read_csv("data/wing_morphometrics.csv")

# Calculate the average marginal cell length
wing_morphometrics <- wing_morphometrics %>%
  mutate(avg_MargCell_mm = (left_MargCell_mm + right_MargCell_mm) / 2)

# Create the plots with correlation coefficient and linear model
plot1 <- ggplot(wing_morphometrics, aes(x = head_width_mm, y = IT_span_mm)) +
  geom_point(color = "#24281A") +
  CONSISTENT_THEME +
  REMOVE_HASH_MARKS +
  labs(x = "Head Width (mm)", y = "Interthorax Span (mm)") +
  stat_cor(method = "pearson", cor.coef.name = "r") +
  geom_smooth(method = "lm", se = TRUE, color = "#899DA4")

plot2 <- ggplot(wing_morphometrics, aes(x = head_width_mm, y = avg_MargCell_mm)) +
  geom_point(color = "#24281A") +
  CONSISTENT_THEME +
  REMOVE_HASH_MARKS +
  labs(x = "Head Width (mm)", y = "Avg. Marginal Cell Length (mm)") +
  stat_cor(method = "pearson", cor.coef.name = "r") +
  geom_smooth(method = "lm", se = TRUE, color = "#899DA4")

plot3 <- ggplot(wing_morphometrics, aes(x = IT_span_mm, y = avg_MargCell_mm)) +
  geom_point(color = "#24281A") +
  CONSISTENT_THEME +
  REMOVE_HASH_MARKS +
  labs(x = "Interthorax Span (mm)", y = "Avg. Marginal Cell Length (mm)") +
  stat_cor(method = "pearson", cor.coef.name = "r") +
  geom_smooth(method = "lm", se = TRUE, color = "#899DA4")

# Combine the plots using cowplot
combined_plot <- plot_grid(plot1, plot2, plot3, ncol = 3, align = "vh")

# Save the combined plot
ggsave("figures/manuscript/si/figure_s9.jpeg", combined_plot, width = 8.5, height = 4.5, dpi = 600)
