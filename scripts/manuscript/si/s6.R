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
    QR_Queen_Condition == "Queenless" ~ "Queenless\nWorker",
    QR_Queen_Condition == "Queenright" ~ "Queenright\nWorker",
    TRUE ~ QR_Queen_Condition
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright\nWorker", "Queenless\nWorker")))


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


bds$UnStandardizedDegree <- bds$Degree * bds$AntPresence

bds_means_unstandardized <- aggregate(cbind(
  UnStandardizedDegree, Close, Eigen, Between,
  QR, Queen, boutDegree, boutBetween, boutClose, boutEigen, bodyDegree, bodyBetween,
  bodyClose, bodyEigen, AverageBoutLength, Presence, AntPresence, mean_vel,
  move_perc, N90.Day4, MRSD.Day4, Initiation.Freq, clust, Disparity
) ~ Bee, bds, mean)
bds_means_unstandardized$Trial <- str_extract(bds_means_unstandardized$Bee, ".+?(?=_)")

bds_means_unstandardized <- bds_means_unstandardized %>%
  mutate(QR_Queen_Condition = case_when(
    QR == 0 & Queen == 0 ~ "Queenless",
    QR == 1 & Queen == 0 ~ "Queenright",
    Queen == 1 ~ "Queen",
    TRUE ~ NA_character_ # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queenright", "Queenless", "Queen")))

bds_means_unstandardized$ID <- paste(bds_means_unstandardized$Trial, bds_means_unstandardized$QR_Queen_Condition)
bds_means_of_means_unstandardized <- aggregate(cbind(UnStandardizedDegree, Close, Eigen, Between, QR, Queen, boutDegree, boutBetween, boutClose, boutEigen, bodyDegree, bodyBetween, bodyClose, bodyEigen, AverageBoutLength, Presence, AntPresence, mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq, clust, Disparity) ~ ID, bds_means_unstandardized, mean)
bds_means_of_means_unstandardized$Trial <- str_extract(bds_means_of_means_unstandardized$ID, ".+?(?= )")

bds_means_of_means_unstandardized <- bds_means_of_means_unstandardized %>%
  mutate(QR_Queen_Condition = case_when(
    QR == 0 & Queen == 0 ~ "Queenless\nWorker",
    QR == 1 & Queen == 0 ~ "Queenright\nWorker",
    Queen == 1 ~ "Queen",
    TRUE ~ NA_character_ # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright\nWorker", "Queenless\nWorker")))

bds_means_of_means_unstandardized <- bds_means_of_means_unstandardized %>%
  mutate(QR_Queen_Condition = fct_relevel(QR_Queen_Condition, "Queen", "Queenright\nWorker", "Queenless\nWorker"))

plot_unstd <- ggplot(bds_means_of_means_unstandardized, aes(x = as.factor(QR_Queen_Condition), y = UnStandardizedDegree)) +
  geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
  geom_point(aes(color = Trial), size = 3) +
  scale_color_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Number of Int. per Hour (Unstandardized)") + # Adjust axis labels
  CONSISTENT_THEME

final_plot <- plot_grid(plot_move_perc, plot_bouts, plot_unstd, ncol = 3, align="hv")

ggsave("figures/manuscript/si/figure_s6.jpeg", plot = final_plot, width = 8.5, height = 3, dpi = 600)
