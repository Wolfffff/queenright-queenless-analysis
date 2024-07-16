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

SHARED_THEME <- theme(panel.spacing = unit(1, "lines"), 
        strip.background = element_blank(), 
        strip.placement = "outside",
        strip.text.x = element_blank(),  # Remove sub-labels (facet labels)
        axis.line.x = element_line(color="black"),  # Add bottom axis line
        axis.line.y = element_line(color="black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
)


# Assuming bds_means_of_means_Q_QRW_QLW_Keystone is your dataframe
# Step 1: Create a new grouping variable
bds_means_of_means_Q_QRW_QLW_Keystone$Group <- with(bds_means_of_means_Q_QRW_QLW_Keystone, ifelse(Q_QRW_QLW_Keystone %in% c("Queen", "Queenright"), "Q + QRw", "Key + QLw"))
bds_means_of_means_Q_QRW_QLW_Keystone$Group <- factor(bds_means_of_means_Q_QRW_QLW_Keystone$Group, levels = c("Q + QRw", "Key + QLw"))
# Step 2: Adjust the factor levels for plotting
bds_means_of_means_Q_QRW_QLW_Keystone <- bds_means_of_means_Q_QRW_QLW_Keystone %>%
  mutate(Q_QRW_QLW_Keystone = fct_recode(Q_QRW_QLW_Keystone,
                           "Queen" = "Queen",
                           "Queenright\nWorker" = "Queenright",
                           "Keystone\nIndividual" = "Keystone",
                           "Queenless\nWorker" = "Queenless"))

# Step 3: Plot with a cut in the x-axis using facets
plot_degree <- ggplot(bds_means_of_means_Q_QRW_QLW_Keystone, aes(x = Q_QRW_QLW_Keystone, y = Degree)) +
  geom_line(aes(group = interaction(Trial, Group)), color = "darkgray",size=0.2) +
  geom_point(aes(color = Trial), size = 3) +
  scale_color_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Std. Interactions per Hour") +
  theme_minimal() +
  CONSISTENT_THEME_NO_ASPECT +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_grid(~Group, scales = "free_x", space = "free_x", switch = "x", labeller = labeller(.rows = label_both, .cols = label_both)) +
  SHARED_THEME

# Step 3: Plot with a cut in the x-axis using facets
plot_disp<- ggplot(bds_means_of_means_Q_QRW_QLW_Keystone, aes(x = Q_QRW_QLW_Keystone, y = N90.Day4)) +
  geom_line(aes(group = interaction(Trial, Group)), color = "darkgray",size=0.2) +
  geom_point(aes(color = Trial), size = 3) +
  scale_color_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("N90 (Dispersion)") +
  theme_minimal() +
  CONSISTENT_THEME_NO_ASPECT +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_grid(~Group, scales = "free_x", space = "free_x", switch = "x", labeller = labeller(.rows = label_both, .cols = label_both)) +
  SHARED_THEME

plot_between <- ggplot(bds_means_of_means_Q_QRW_QLW_Keystone, aes(x = Q_QRW_QLW_Keystone, y = Between)) +
  geom_line(aes(group = interaction(Trial, Group)), color = "darkgray",size=0.2) +
  geom_point(aes(color = Trial), size = 3) +
  scale_color_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Betweenness") +
  theme_minimal() +
  CONSISTENT_THEME_NO_ASPECT +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_grid(~Group, scales = "free_x", space = "free_x", switch = "x", labeller = labeller(.rows = label_both, .cols = label_both)) +
  SHARED_THEME

# Plot the same but OvaryIndex
plot_oi <- ggplot(bds_means_of_means_Q_QRW_QLW_Keystone, aes(x = Q_QRW_QLW_Keystone, y = ovary_idx)) +
  geom_line(aes(group = interaction(Trial, Group)), color = "darkgray",size=0.2) +
  geom_point(aes(color = Trial), size = 3) +
  scale_color_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Ovary Index") +
  theme_minimal() +
  CONSISTENT_THEME_NO_ASPECT +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_grid(~Group, scales = "free_x", space = "free_x", switch = "x", labeller = labeller(.rows = label_both, .cols = label_both)) +
  SHARED_THEME

plot_grid(plot_degree, plot_disp, plot_between,plot_oi, ncol = 2)
ggsave("figures/manuscript/figure_4.jpeg", width = 8.5, height = 5, dpi = 1200)
