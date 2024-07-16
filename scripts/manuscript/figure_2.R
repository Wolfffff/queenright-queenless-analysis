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
    mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright", "Queenless")))

# Calculate mean values of QR, Queen, and Degree for each Bee (QR and Queen are binary so mean is the value itself)
bds_means <- bds %>%
    group_by(Bee) %>%
    summarise(across(c(QR, Queen, Degree, Initiation.Freq, N90.Day4), mean, na.rm = TRUE))

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
    mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright", "Queenless")))

# Create ID column for aggregation
bds_means <- bds_means %>%
    mutate(ID = paste(Trial, QR_Queen_Condition))

# Calculate mean of means for each ID
bds_mean_of_means <- bds_means %>%
    group_by(ID) %>%
    summarise(across(c(QR, Queen, Degree, Initiation.Freq, N90.Day4), mean, na.rm = TRUE))

# Extract Trial information from ID column
bds_mean_of_means <- bds_mean_of_means %>%
    mutate(Trial = str_extract(ID, ".+?(?= )"))

# Define QR_Queen_Condition based on QR and Queen values for mean of means
bds_mean_of_means <- bds_mean_of_means %>%
    rename(
        WorkerQR = QR,
        WorkerQueen = Queen
    ) %>%
    mutate(QR_Queen_Condition = case_when(
        WorkerQR == 0 & WorkerQueen == 0 ~ "Queenless\nWorker",
        WorkerQR == 1 & WorkerQueen == 0 ~ "Queenright\nWorker",
        WorkerQueen == 1 ~ "Queen"
    )) %>%
    mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright\nWorker", "Queenless\nWorker")))

# Plot Degree
plot_degree <- ggplot(bds_mean_of_means, aes(x = QR_Queen_Condition, y = Degree)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
    geom_point(aes(color = Trial), size = 3) +
    scale_color_manual(values = COLONY_COLORS) +
    xlab("") +
    ylab("Std. Interactions per Hour") +
    CONSISTENT_THEME

# Plot Initiation.Freq
plot_init_freq <- ggplot(bds_mean_of_means, aes(x = QR_Queen_Condition, y = Initiation.Freq)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
    geom_point(aes(color = Trial), size = 3) +
    scale_color_manual(values = COLONY_COLORS) +
    xlab("") +
    ylab("Initiation Frequency") +
    CONSISTENT_THEME

# Plot N90.Day4
plot_n90_day4 <- ggplot(bds_mean_of_means, aes(x = QR_Queen_Condition, y = N90.Day4)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
    geom_point(aes(color = Trial), size = 3) +
    scale_color_manual(values = COLONY_COLORS) +
    xlab("") +
    ylab("N90 (Dispersion)") +
    CONSISTENT_THEME

# Rug
bds$Alpha <- ifelse(bds$Queen, .5, 0.1)
bds$PointSize <- ifelse(bds$Queen, .05, .005)

degree_over_time <- ggplot(bds, aes(x = as.integer(Hour), y = Degree / 20, group = ID)) +
    geom_jitter(aes(color = QR_Queen_Condition, alpha = Alpha, size = PointSize)) +
    geom_smooth(aes(group = QR_Queen_Condition, color = QR_Queen_Condition), se = FALSE) +
    scale_size(range = c(.001, .5)) +
    scale_color_manual(
        labels = c("Queen", "Queenright Worker", "Queenless Worker"),
        values = setNames(c(Q_QRW_INT_QLW$Q, Q_QRW_INT_QLW$QRW, Q_QRW_INT_QLW$QLW), levels(bds$QR_Queen_Condition)),
        guide = guide_legend(direction = "horizontal")
    ) +
    scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA), expand = c(0, 0)) + # Expand limits to include 0
    labs(color = "") +
    xlab("Hour") +
    ylab("Std. Time Interacting per Hour (s)") +
    theme_minimal() +
    theme(
        text = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(1, 1.02), # Move legend to top right
        legend.justification = c(1, 1), # Align legend to top right
        legend.background = element_rect(fill = alpha("white", 1), color = NA), # Add white background to legend
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 9),
        panel.grid.major.x = element_line(color = "grey", linetype = "dashed"), # Keep vertical grid lines
        panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"), # Keep vertical grid lines
        panel.grid.major.y = element_blank(), # Remove horizontal grid lines
        panel.grid.minor.y = element_blank(), # Remove horizontal grid lines
        axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
        axis.line.y = element_line(color = "black", size = 0.5), # Add y-axis line
        strip.text = element_text(size = 9, face = "bold"), # Make facet plot titles larger and bold
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm") # Add margin for alignment
    ) +
    guides(alpha = "none", size = "none")

# Align the top left plot and the bottom plot
plots <- align_plots(plot_degree, degree_over_time, align = "v", axis = "l")

# Create the top row with the aligned top left plot and the other two top row plots
top_row <- plot_grid(plots[[1]], plot_init_freq, plot_n90_day4, nrow = 1, align = "hv", axis = "tblr")

# Create the final plot with the top row and the bottom plot
final_plot <- plot_grid(plots[[2]],top_row, ncol = 1, rel_heights = c(1, 1))

# Save the combined plot
# Adjust the plot saving command to include margins
ggsave("figures/manuscript/figure_2_combined.jpeg", plot = final_plot + theme(plot.margin = margin(1, 1, 1, 1, "cm")), width = 8.5, height = 6,dpi =1200)
