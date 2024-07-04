# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(patchwork)

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
    mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queenright", "Queenless", "Queen")))

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
    mutate(QR_Queen_Condition = case_when(
        QR == 0 & Queen == 0 ~ "Queenless",
        QR == 1 & Queen == 0 ~ "Queenright",
        Queen == 1 ~ "Queen"
    )) %>%
    mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queenright", "Queenless", "Queen")))

# Plot Degree

consistent_theme <- theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "None",
        text = element_text(size = 7),
        panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", size = 0.5),
        axis.line.y = element_line(color = "black", size = 0.5),
        strip.text = element_text(size = 7, face = "bold"),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        aspect.ratio = 1,
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")
    )
# Plot Degree
plot_degree <- ggplot(bds_mean_of_means, aes(x = QR_Queen_Condition, y = Degree)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
    geom_point(aes(color = Trial), size = 3) +
    scale_color_manual(values = wes_palette("Cavalcanti1")) +
    xlab("") +
    ylab("Standardized Number of Interactions per Hour") +
    consistent_theme

# Plot Initiation.Freq
plot_init_freq <- ggplot(bds_mean_of_means, aes(x = QR_Queen_Condition, y = Initiation.Freq)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
    geom_point(aes(color = Trial), size = 3) +
    scale_color_manual(values = wes_palette("Cavalcanti1")) +
    xlab("") +
    ylab("Initiation Frequency") +
    consistent_theme

# Plot N90.Day4
plot_n90_day4 <- ggplot(bds_mean_of_means, aes(x = QR_Queen_Condition, y = N90.Day4)) +
    geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2) +
    geom_point(aes(color = Trial), size = 3) +
    scale_color_manual(values = wes_palette("Cavalcanti1")) +
    xlab("") +
    ylab("N90 (Dispersion)") +
    consistent_theme

# Rug
bds$Alpha <- ifelse(bds$Queen, .5, 0.005)
bds$PointSize <- ifelse(bds$Queen, .003, .001)

degree_over_time <- ggplot(bds, aes(x = as.integer(Hour), y = Degree / 20, group = ID)) +
    geom_jitter(aes(color = QR_Queen_Condition, alpha = Alpha, size = PointSize)) +
    geom_smooth(aes(group = QR_Queen_Condition, color = QR_Queen_Condition), se = FALSE) +
    scale_size(range = c(.001, .2)) +
    scale_color_manual(
        labels = c("Queenright Worker", "Queenless Worker", "Queen"),
        values = c("#629CC0", "#CEB175", "#E54E21"),
        guide = guide_legend(direction = "horizontal")
    ) +
    scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA), expand = c(0, 0)) + # Expand limits to include 0
    labs(color = "") +
    xlab("Hour") +
    ylab("Time Spent Interacting per Hour (s)") +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = c(1, 1.02), # Move legend to top right
        legend.justification = c(1, 1), # Align legend to top right
        legend.background = element_rect(fill = alpha("white", 1)),
        theme(legend.key = element_rect(fill = "white")),
        legend.title = element_text(size = 7, face = "bold"),
        legend.text = element_text(size = 7),
        panel.grid.major.x = element_line(color = "grey", linetype = "dashed"), # Keep vertical grid lines
        panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"), # Keep vertical grid lines
        panel.grid.major.y = element_blank(), # Remove horizontal grid lines
        panel.grid.minor.y = element_blank(), # Remove horizontal grid lines
        axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
        axis.line.y = element_line(color = "black", size = 0.5), # Add y-axis line
        strip.text = element_text(size = 7, face = "bold"), # Make facet plot titles larger and bold
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm") # Add margin for alignment
    ) +
    guides(alpha = "none", size = "none")

# Align the top left plot and the bottom plot
plots <- align_plots(plot_degree, degree_over_time, align = 'v', axis = 'l')

# Create the top row with the aligned top left plot and the other two top row plots
top_row <- plot_grid(plots[[1]], plot_init_freq, plot_n90_day4, nrow = 1, align = 'hv', axis = 'tblr')

# Create the final plot with the top row and the bottom plot
final_plot <- plot_grid(top_row, plots[[2]], ncol = 1, rel_heights = c(1, 1))

# Save the combined plot
ggsave("figures/manuscript/figure_1_combined.png", plot = final_plot, width = 8.5, height = 6)
