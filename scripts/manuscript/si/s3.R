library(tidyverse)
library(cowplot)

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

bds_means_no_q <- bds_means %>% filter(!Queen)

ggplot_obj <- ggplot(bds_means_no_q, aes(AntPresence, group = as.factor(QR))) +
  geom_density(alpha = 0.4, aes(fill = as.factor(QR)), color = "black") +
  scale_fill_manual(labels = c("Queenright Worker", "Queenless Worker"), values = c(Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$QLW), guide = guide_legend(direction = "horizontal", title = "")) +
  CONSISTENT_THEME +
  theme(legend.position = "none") +
  xlab("Ant. Presence") +
  ylab("Density")


plot_list <- bds_means_no_q %>%
  split(.$Trial) %>%
  map(~ ggplot_obj %+% . + facet_wrap(~Trial, scales = "free_y"))
plot_list$`Colony 1` <- plot_list$`Colony 1` + theme(legend.position = c(.84, 1.35))

# Use cowplot to arrange the plots horizontally with 5 columns
plot_grid(plotlist = plot_list, ncol = 5, align = "hv")

# Save the combined plot
ggsave("figures/manuscript/si/figure_s3.jpeg", plot = last_plot(), width = 8.5, height = 2.6, dpi = 1200)
