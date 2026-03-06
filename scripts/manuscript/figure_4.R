# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(patchwork)
library(factoextra)
library(ggplot2)
library(cowplot)
library(ggbeeswarm)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

SHARED_THEME <- theme(
  panel.spacing = unit(1, "lines"),
  strip.background = element_blank(),
  strip.placement = "outside",
  strip.text.x = element_blank(), # Remove sub-labels (facet labels)
  axis.line.x = element_line(color = "black"), # Add bottom axis line
  axis.line.y = element_line(color = "black"),
  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
)


# Assuming bds_means_of_means_Q_QRW_QLW_Keystone is your dataframe
# Step 1: Create a new grouping variable
bds_means_of_means_Q_QRW_QLW_Keystone$Group <- with(bds_means_of_means_Q_QRW_QLW_Keystone, ifelse(Q_QRW_QLW_Keystone %in% c("Queen", "Queenright"), "Q + QRw", "Key + QLw"))
bds_means_of_means_Q_QRW_QLW_Keystone$Group <- factor(bds_means_of_means_Q_QRW_QLW_Keystone$Group, levels = c("Q + QRw", "Key + QLw"))


bds_means$Group <- with(bds_means, ifelse(Q_QRW_QLW_Keystone %in% c("Queen", "Queenright"), "Q + QRw", "Key + QLw"))
bds_means$Group <- factor(bds_means$Group, levels = c("Q + QRw", "Key + QLw"))

# Step 2: Adjust the factor levels for plotting
bds_means_of_means_Q_QRW_QLW_Keystone <- bds_means_of_means_Q_QRW_QLW_Keystone %>%
  mutate(Q_QRW_QLW_Keystone = fct_recode(Q_QRW_QLW_Keystone,
    "Queen" = "Queen",
    "Queenright Worker" = "Queenright",
    "Queenless Influencer" = "Keystone",
    "Queenless Non-Influencer Worker" = "Queenless"
  ))

bds_means <- bds_means %>%
  mutate(Q_QRW_QLW_Keystone = fct_recode(Q_QRW_QLW_Keystone,
    "Queen" = "Queen",
    "Queenright Worker" = "Queenright",
    "Queenless Influencer" = "Keystone",
    "Queenless Non-Influencer Worker" = "Queenless"
  ))

bds_means_of_means_Q_QRW_QLW_Keystone <- bds_means_of_means_Q_QRW_QLW_Keystone %>%
  mutate(Q_QRW_QLW_Keystone = fct_recode(Q_QRW_QLW_Keystone,
    "Queen" = "Queen",
    "Queenright Worker" = "Queenright",
    "Queenless Influencer" = "Keystone",
    "Queenless Non-Influencer Worker" = "Queenless"
  ))

# Set the factor levels with line breaks for plotting for both dataframes
bds_means$Q_QRW_QLW_Keystone <- factor(
  bds_means$Q_QRW_QLW_Keystone,
  levels = c(
    "Queen",
    "Queenright Worker",
    "Queenless Influencer",
    "Queenless Non-Influencer Worker"
  ),
  labels = c(
    "Queen",
    "Queenright Worker",
    "Queenless Influencer",
    "Queenless\nNon-Influencer Worker"
  )
)

bds_means_of_means_Q_QRW_QLW_Keystone$Q_QRW_QLW_Keystone <- factor(
  bds_means_of_means_Q_QRW_QLW_Keystone$Q_QRW_QLW_Keystone,
  levels = c(
    "Queen",
    "Queenright Worker",
    "Queenless Influencer",
    "Queenless Non-Influencer Worker"
  ),
  labels = c(
    "Queen",
    "Queenright Worker",
    "Queenless Influencer",
    "Queenless\nNon-Influencer Worker"
  )
)

# Step 3: Plot with a cut in the x-axis using facets
plot_degree <- ggplot(bds_means %>% filter(QR_Queen_Condition != "Queen"), aes(x = Q_QRW_QLW_Keystone, y = Degree)) +
  geom_line(data = bds_means_of_means_Q_QRW_QLW_Keystone, aes(group = Trial), color = "darkgray", linewidth = 0.2) +
  geom_point(data = bds_means_of_means_Q_QRW_QLW_Keystone %>% filter(Queen == 1), aes(color = Trial, fill = Trial), size = 3, shape = 21, stroke = 0.2, color = "black") +
  geom_beeswarm(aes(color = Trial), stroke = 0, size = 1, alpha = .2, method = "hex") +
  geom_point(data = bds_means_of_means_Q_QRW_QLW_Keystone %>% filter(Queen == 0), aes(color = Trial, fill = Trial), size = 3, shape = 21, stroke = 0.2, color = "black") +
  scale_color_manual(
    values = COLONY_COLORS
  ) +
  scale_fill_manual(values = COLONY_COLORS) +
  xlab("") +
  ylab("Std. Interactions per Hour") +
  theme_minimal() +
  CONSISTENT_THEME_NO_ASPECT +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_grid(~Group, scales = "free_x", space = "free_x", switch = "x", labeller = labeller(.rows = label_both, .cols = label_both)) +
  SHARED_THEME


# Step 3: Plot with a cut in the x-axis using facets
plot_disp <- ggplot(bds_means %>% filter(QR_Queen_Condition != "Queen"), aes(x = Q_QRW_QLW_Keystone, y = N90.Day4)) +
  geom_line(data = bds_means_of_means_Q_QRW_QLW_Keystone, aes(group = Trial), color = "darkgray", linewidth = 0.2) +
  geom_point(data = bds_means_of_means_Q_QRW_QLW_Keystone %>% filter(Queen == 1), aes(color = Trial, fill = Trial), size = 3, shape = 21, stroke = 0.2, color = "black") +
  geom_beeswarm(aes(color = Trial), stroke = 0, size = 1, alpha = .2, method = "hex") +
  geom_point(data = bds_means_of_means_Q_QRW_QLW_Keystone %>% filter(Queen == 0), aes(color = Trial, fill = Trial), size = 3, shape = 21, stroke = 0.2, color = "black") +
  scale_color_manual(
    values = COLONY_COLORS
  ) +
  scale_fill_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("N90 (Dispersion)") +
  theme_minimal() +
  CONSISTENT_THEME_NO_ASPECT +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_grid(~Group, scales = "free_x", space = "free_x", switch = "x", labeller = labeller(.rows = label_both, .cols = label_both)) +
  SHARED_THEME

plot_between <- ggplot(bds_means %>% filter(QR_Queen_Condition != "Queen"), aes(x = Q_QRW_QLW_Keystone, y = Between)) +
  geom_line(data = bds_means_of_means_Q_QRW_QLW_Keystone, aes(group = Trial), color = "darkgray", linewidth = 0.2) +
  geom_point(data = bds_means_of_means_Q_QRW_QLW_Keystone %>% filter(Queen == 1), aes(color = Trial, fill = Trial), size = 3, shape = 21, stroke = 0.2, color = "black") +
  geom_beeswarm(aes(color = Trial), stroke = 0, size = 1, alpha = .2, method = "hex") +
  geom_point(data = bds_means_of_means_Q_QRW_QLW_Keystone %>% filter(Queen == 0), aes(color = Trial, fill = Trial), size = 3, shape = 21, stroke = 0.2, color = "black") +
  scale_color_manual(
    values = COLONY_COLORS
  ) +
  scale_fill_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Betweenness") +
  theme_minimal() +
  CONSISTENT_THEME_NO_ASPECT +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_grid(~Group, scales = "free_x", space = "free_x", switch = "x", labeller = labeller(.rows = label_both, .cols = label_both)) +
  SHARED_THEME

# Plot the same but OvaryIndex
plot_oi <- ggplot(bds_means %>% filter(QR_Queen_Condition != "Queen"), aes(x = Q_QRW_QLW_Keystone, y = ovary_idx)) +
  geom_line(data = bds_means_of_means_Q_QRW_QLW_Keystone, aes(group = Trial), color = "darkgray", linewidth = 0.2) +
  geom_point(data = bds_means_of_means_Q_QRW_QLW_Keystone %>% filter(Queen == 1), aes(color = Trial, fill = Trial), size = 3, shape = 21, stroke = 0.2, color = "black") +
  geom_beeswarm(aes(color = Trial), stroke = 0, size = 1, alpha = .2, method = "hex") +
  geom_point(data = bds_means_of_means_Q_QRW_QLW_Keystone %>% filter(Queen == 0), aes(color = Trial, fill = Trial), size = 3, shape = 21, stroke = 0.2, color = "black") +
  scale_color_manual(
    values = COLONY_COLORS
  ) +
  scale_fill_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Ovary Index") +
  scale_x_discrete(labels = c(
    "Queen" = "Queen",
    "Queenright\nWorker" = "Queenright\nWorker",
    "Queenless\nInfluencer" = "Queenless\nInfluencer",
    "Queenless\nNon-Influencer\nWorker" = "Queenless\nNon-\nInfluencer\nWorker"
  )) +
  theme_minimal() +
  CONSISTENT_THEME_NO_ASPECT +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_grid(~Group, scales = "free_x", space = "free_x", switch = "x", labeller = labeller(.rows = label_both, .cols = label_both)) +
  SHARED_THEME +
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1))

# Create SI figure with first three plots (remove ovary index only)
si_plot <- plot_grid(plot_degree, plot_disp, plot_between, ncol = 3, align = "hv", axis = "tb")
ggsave("figures/manuscript/outlier_statistical_differences.jpg", si_plot, width = 8.5, height = 4, dpi = 600)
