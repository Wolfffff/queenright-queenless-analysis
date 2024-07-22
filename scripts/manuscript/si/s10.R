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

bds_means <- bds_means %>%
  mutate(QR_Queen_Condition = fct_recode(QR_Queen_Condition,
    "Queen" = "Queen",
    "Colony 5 - Queenright Worker" = "Queenright",
    "Colony 5 - Queenless Worker" = "Queenless"
  )) %>%
  filter(Trial == "RooibosTea") %>%
  filter(QR_Queen_Condition != "Queen")

# Filter and plot
ggplot(bds_means, aes(x = ovary_idx, color = QR_Queen_Condition, fill = QR_Queen_Condition)) +
  geom_density(alpha = 0.6, color = "black") +
  scale_fill_manual(values = c(Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$QLW)) +
  xlab("Ovary Index") +
  ylab("Density") +
  CONSISTENT_THEME +
  REMOVE_HASH_MARKS +
  theme(
    legend.position = "top", # Position the legend at the top
    legend.direction = "horizontal", # Make the legend horizontal
    legend.title = element_blank() # Remove the legend title
  )

ggsave("figures/manuscript/si/figure_s10.jpeg", width = 4.5, height = 4.5, units = "in", dpi = 600)
