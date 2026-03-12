library(tidyverse)
library(cowplot)
library(wesanderson)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

bds_means <- bds_means %>%
  mutate(QR = factor(QR, levels = c(0, 1), labels = c("QL", "QR")))

bds_means <- bds_means %>%
  mutate(Trial = factor(Trial,
    levels = c("20221123", "20221209", "20230213", "MexHotChoc", "RooibosTea"),
    labels = c("Colony 1", "Colony 2", "Colony 3", "Colony 4", "Colony 5")
  ))

bds_means <- bds_means %>%
  mutate(QR = fct_recode(factor(QR), "Queenright Worker" = "QR", "Queenless Worker" = "QL"))

# Create "All Colonies" aggregated data
bds_means_all <- bds_means %>%
  mutate(Trial = "All Colonies")

# Combine original data with aggregated data
bds_means_combined <- bind_rows(bds_means, bds_means_all)

bds_means_q <- bds_means_combined %>% filter(Queen == 1)
bds_means_no_q <- bds_means_combined %>% filter(!Queen)

# Step 1: Create a base density plot object
ggplot_obj <- ggplot(bds_means_no_q, aes(AntPresence, group = as.factor(QR))) +
  geom_density(alpha = 0.4, aes(fill = as.factor(QR)), color = "black") +
  scale_fill_manual(labels = c("Queenright Worker", "Queenless Worker"), values = c(Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$QLW), guide = guide_legend(direction = "horizontal", title = "")) +
  CONSISTENT_THEME +
  REMOVE_HASH_MARKS +
  theme(legend.position = "none")

# Step 2: Modify plot_list to adjust labels for each plot
plot_list <- bds_means_no_q %>%
  split(.$Trial) %>%
  map(~ ggplot_obj %+% . + facet_wrap(~Trial, scales = "free_y") +
    xlim(0.2, 1) +
    ylim(0, 10))

# Add y label "Density" and rug plot to the first plot
plot_list$`Colony 1` <- plot_list$`Colony 1` +
  ylab("Density") +
  theme(legend.position.inside = c(0.84, 1.35), axis.title.x = element_blank()) +
  geom_rug(data = bds_means_q %>% filter(Trial == "Colony 1"), aes(x = AntPresence), sides = "b")

# Ensure Colony 3 has no x or y labels and add rug plot
plot_list$`Colony 3` <- plot_list$`Colony 3` +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  geom_rug(data = bds_means_q %>% filter(Trial == "Colony 3"), aes(x = AntPresence), sides = "b")

# Ensure other plots do not have x or y labels and add rug plots
plot_list$`Colony 2` <- plot_list$`Colony 2` +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  geom_rug(data = bds_means_q %>% filter(Trial == "Colony 2"), aes(x = AntPresence), sides = "b")

# Add "All Colonies" plot between Colony 3 and 4 with x-axis label
plot_list$`All Colonies` <- ggplot_obj %+% (bds_means_no_q %>% filter(Trial == "All Colonies")) +
  facet_wrap(~Trial, scales = "free_y") +
  xlim(0.2, 1) +
  ylim(0, 10) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank()) +
  geom_rug(data = bds_means_q %>% filter(Trial == "All Colonies"), aes(x = AntPresence), sides = "b")

plot_list$`Colony 4` <- plot_list$`Colony 4` +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  geom_rug(data = bds_means_q %>% filter(Trial == "Colony 4"), aes(x = AntPresence), sides = "b")

plot_list$`Colony 5` <- plot_list$`Colony 5` +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  geom_rug(data = bds_means_q %>% filter(Trial == "Colony 5"), aes(x = AntPresence), sides = "b")

# Step 3: Use cowplot to arrange the plots horizontally with 6 columns
# Reorder plots to put "All Colonies" between Colony 3 and 4
plot_order <- c("Colony 1", "Colony 2", "Colony 3", "Colony 4", "Colony 5", "All Colonies")
combined_plot <- plot_grid(plotlist = plot_list[plot_order], ncol = 6, align = "hv")

# Add x-axis label just below the combined plot, close to the plot
combined_plot <- plot_grid(
  combined_plot,
  ggdraw() + draw_label("Antennal Presence", fontface = "plain", size = 12, x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5),
  ncol = 1,
  rel_heights = c(1, 0.12)
)
ggsave("figures/manuscript/si/figure_s4.jpeg", plot = combined_plot, width = 10, height = 2.5, dpi = 600)
