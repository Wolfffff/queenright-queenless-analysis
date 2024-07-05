# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(patchwork)
library(factoextra)
library(ggplot2)

source("scripts/manuscript/constants.R")

# Read the data
bds <- read_csv("data/BigDataSheet.csv")
bds <- bds %>%
    mutate(Trial = str_extract(Bee, ".+?(?=_)"))

bds <- bds %>%
    mutate(QR_Queen_Condition = case_when(
        QR == 0 & Queen == 0 ~ "Queenless",
        QR == 1 & Queen == 0 ~ "Queenright",
        Queen == 1 ~ "Queen"
    )) %>%
    mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queenright", "Queenless", "Queen")))


bds$OvaryIndex <- bds$AverageOvaryWidth / bds$AverageWingLength
# Calculate mean values of QR, Queen, and Degree for each Bee (QR and Queen are binary so mean is the value itself)
bds_means <- bds %>%
    group_by(Bee) %>%
    summarise(across(c(OvaryIndex, AverageOvaryWidth,AverageWingLength, Infl, Degree, Close, Eigen, Between, QR, Queen, boutDegree, boutBetween, boutClose, boutEigen, bodyDegree, bodyBetween, bodyClose, bodyEigen, AverageBoutLength, Presence, AntPresence, mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq, clust), mean, na.rm = TRUE))

# Extract Trial information from Bee column
bds_means <- bds_means %>%
    mutate(Trial = str_extract(Bee, ".+?(?=_)"))

# Define QR_Queen_Condition based on QR and Queen values
bds_means <- bds_means %>%
    mutate(QR_Queen_Condition = case_when(
        QR == 0 & Queen == 0 ~ "Queenless",
        QR == 1 & Queen == 0 ~ "Queenright",
        Queen == 1 ~ "Queen"
    )) %>%
    mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queenright", "Queenless", "Queen")))

# Create ID column for aggregation
bds_means <- bds_means %>%
    mutate(ID = paste(Trial, QR_Queen_Condition))

# Calculate mean of means for each ID
bds_mean_of_means <- bds_means %>%
    group_by(ID) %>%
    summarise(across(c(OvaryIndex,AverageOvaryWidth,AverageWingLength,Infl, Degree, Close, Eigen, Between, QR, Queen, boutDegree, boutBetween, boutClose, boutEigen, bodyDegree, bodyBetween, bodyClose, bodyEigen, AverageBoutLength, Presence, AntPresence, mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq, clust), mean, na.rm = TRUE))

# Extract Trial information from ID column
bds_mean_of_means <- bds_mean_of_means %>%
    mutate(Trial = str_extract(ID, ".+?(?= )"))

# Define QR_Queen_Condition based on QR and Queen values for mean of means
bds_mean_of_means <- bds_mean_of_means %>%
    mutate(QR_Queen_Condition = case_when(
        QR == 0 & Queen == 0 ~ "Queenless",
        QR == 1 & Queen == 0 ~ "Queenright",
        Queen == 1 ~ "Queen"
    )) %>%
    mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queenright", "Queenless", "Queen")))

plot_ovaries <- ggplot(bds_mean_of_means, aes(x = QR_Queen_Condition, y = OvaryIndex)) +
  geom_line(aes(group = Trial), color = "darkgray") +
  geom_point(aes(color = Trial), size = 5) +
  scale_color_manual(values =COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Ovary Index") + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    #legend.position = "none",
    text = element_text(size = 16),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"), # Keep vertical grid lines
    panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"), # Keep vertical grid lines
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(), # Remove horizontal grid lines
    axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
    axis.line.y = element_line(color = "black", size = 0.5), # Add y-axis line
    strip.text = element_text(size = 14, face = "bold"), # Make facet plot titles larger and bold
    axis.text.y.right = element_blank(), # Remove right y-axis text
    axis.ticks.y.right = element_blank(), # Remove right y-axis ticks
    aspect.ratio = 1,
    legend.position = "none"
  ) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
plot_ovaries

ggsave("figures/manuscript/ovary_index.png", plot = plot_ovaries, width = 4.5, height = 4.5, units = "in", dpi = 1200)


# Plot distribution of ovary index for Rooibos trial
ggplot(bds_means %>% filter(Trial == "RooibosTea"), aes(x = OvaryIndex)) +
  geom_density(fill = COLONY_COLORS[5], alpha = 0.5) +
  xlab("Ovary Index") +
  ylab("Density") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 16),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"), # Keep vertical grid lines
    panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"), # Keep vertical grid lines
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(), # Remove horizontal grid lines
    axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
    axis.line.y = element_line(color = "black", size = 0.5), # Add y-axis line
    strip.text = element_text(size = 14, face = "bold"), # Make facet plot titles larger and bold
    axis.text.y.right = element_blank(), # Remove right y-axis text
    axis.ticks.y.right = element_blank(), # Remove right y-axis ticks
    aspect.ratio = 1
  ) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

ggsave("figures/manuscript/rooibos_ovary_index_distribution.png", width = 4.5, height = 4.5, units = "in", dpi = 1200)
