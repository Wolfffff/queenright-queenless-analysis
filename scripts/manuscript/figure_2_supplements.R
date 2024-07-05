# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(cowplot)

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

# Calculate mean values of QR, Queen, and Degree for each Bee (QR and Queen are binary so mean is the value itself)
bds_means <- bds %>%
    group_by(Bee) %>%
    summarise(across(c(QR, Queen, Degree, Initiation.Freq, N90.Day4, move_perc,boutDegree), mean, na.rm = TRUE))

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
    summarise(across(c(QR, Queen, Degree, Initiation.Freq, N90.Day4,move_perc,boutDegree), mean, na.rm = TRUE))

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

# Plot Degree
plot_move_perc <- ggplot(bds_mean_of_means, aes(x = QR_Queen_Condition, y = move_perc)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
    geom_point(aes(color = Trial), size = 3) +
    scale_color_manual(values = COLONY_COLORS) +
    xlab("") +
    ylab("Percent of Time Moving") +
    CONSISTENT_THEME

# Save the combined plot
# Adjust the plot saving command to include margins
ggsave("figures/manuscript/figure_2_supp.jpeg", plot = plot_move_perc + theme(plot.margin = margin(1, 1, 1, 1, "cm")), width = 4.5, height = 4.5,dpi =1200)

plot_bouts <- ggplot(bds_mean_of_means, aes(x = QR_Queen_Condition, y = boutDegree)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
    geom_point(aes(color = Trial), size = 3) +
    scale_color_manual(values = COLONY_COLORS) +
    xlab("") +
    ylab("Std. Number of Int. Bouts per Hour") +
    CONSISTENT_THEME

# Save the combined plot
# Adjust the plot saving command to include margins
ggsave("figures/manuscript/figure_2_supp_bouts.jpeg", plot = plot_bouts + theme(plot.margin = margin(1, 1, 1, 1, "cm")), width = 4.5, height = 4.5,dpi =1200)

