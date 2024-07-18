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
source("scripts/manuscript/load_data.R")

# Assuming bds_means is your dataframe
# Step 1: Create a new grouping variable
bds_means$Group <- with(bds_means, ifelse(Q_QRW_QLW_Keystone %in% c("Queen", "Queenright"), "Q + QRw", "Key + QLw"))
bds_means$Group <- factor(bds_means$Group, levels = c("Q + QRw", "Key + QLw"))
# Step 2: Adjust the factor levels for plotting
bds_means <- bds_means %>%
  mutate(Q_QRW_QLW_Keystone = fct_recode(Q_QRW_QLW_Keystone,
    "Queen" = "Queen",
    "Queenright Worker" = "Queenright",
    "Keystone Individual" = "Keystone",
    "Non-Keystone\nQueenless Worker" = "Queenless"
  ))


bds_means <- bds_means %>% mutate(
  PointSize = case_when(
    Q_QRW_QLW_Keystone %in% c("Queen", "Keystone Individual") ~ 2.5,
    TRUE ~ 1.5
  )
)

bds_means$Alpha <- ifelse(bds_means$Q_QRW_QLW_Keystone %in% c("Queen", "Keystone Individual"), 1, 0.4)
ggplot(bds_means, aes(x = Degree, y = bodyDegree, group = Q_QRW_QLW_Keystone)) +
  geom_point(aes(color = Q_QRW_QLW_Keystone), alpha = bds_means$Alpha, size = bds_means$PointSize, stroke = 0) +
  scale_color_manual(
    values = c("Queen" = Q_QRW_KEY_QLW$Q, "Queenright Worker" = Q_QRW_KEY_QLW$QRW, "Keystone Individual" = Q_QRW_KEY_QLW$KEY, "Non-Keystone\nQueenless Worker" = Q_QRW_KEY_QLW$QLW)
  ) +
  xlab("Head Degree Centrality") +
  ylab("Body Degree Centrality") +
  CONSISTENT_THEME +
  REMOVE_HASH_MARKS +
theme(
  legend.position = "top", # Position the legend at the top
  legend.direction = "horizontal", # Make the legend horizontal
  legend.title = element_blank(), # Remove the legend title
  legend.spacing.x = unit(0.0, "cm"), # Reduces the space between legend labels
  legend.key.width = unit(0.2, "cm") # Adjust the width of the legend keys (symbols)
)

ggsave("figures/manuscript/si/figure_s13.jpeg", width = 4.25, height = 4.25, units = "in", dpi = 1200)
