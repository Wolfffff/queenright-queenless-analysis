# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(patchwork)
library(factoextra)
library(ggplot2)

source("scripts/manuscript/constants.R")

SHARED_THEME <- theme(panel.spacing = unit(1, "lines"), 
        strip.background = element_blank(), 
        strip.placement = "outside",
        strip.text.x = element_blank(),  # Remove sub-labels (facet labels)
        axis.line.x = element_line(color="black"),  # Add bottom axis line
        axis.line.y = element_line(color="black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
)

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
    summarise(across(c(Degree, Close, Eigen, Between, QR, Queen, boutDegree, boutBetween, boutClose, boutEigen, bodyDegree, bodyBetween, bodyClose, bodyEigen, AverageBoutLength, Presence, AntPresence, mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq, clust, Infl), mean, na.rm = TRUE))

# Extract Trial information from Bee column
bds_means <- bds_means %>%
    mutate(Trial = str_extract(Bee, ".+?(?=_)"))

# Define QR_Queen_Condition based on QR and Queen values
bds_means <- bds_means %>%
    mutate(QR_Queen_Inf = case_when(
    QR==0 & Infl==0 ~ "Queenless Worker",
    QR==1 & Queen==0 ~ "Queenright Worker",
    Queen==1 ~ "Queen",
    QR==0 & Infl==1 ~ "Influencer",
    TRUE ~ NA_character_  # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Inf = factor(QR_Queen_Inf, levels = c( "Queenless Worker","Queenright Worker", "Queen", "Influencer")))

# Create ID column for aggregation
bds_means <- bds_means %>%
    mutate(ID = paste(Trial, QR_Queen_Inf))

# Calculate mean of means for each ID
bds_mean_of_means <- bds_means %>%
    group_by(ID) %>%
    summarise(across(c(Infl, Degree, Close, Eigen, Between, QR, Queen, boutDegree, boutBetween, boutClose, boutEigen, bodyDegree, bodyBetween, bodyClose, bodyEigen, AverageBoutLength, Presence, AntPresence, mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq, clust), mean, na.rm = TRUE))

# Extract Trial information from ID column
bds_mean_of_means <- bds_mean_of_means %>%
    mutate(Trial = str_extract(ID, ".+?(?= )"))

# Define QR_Queen_Condition based on QR and Queen values for mean of means
bds_mean_of_means <- bds_mean_of_means %>%
    mutate(QR_Queen_Inf = case_when(
    QR==0 & Infl==0 ~ "Queenless Worker",
    QR==1 & Queen==0 ~ "Queenright Worker",
    Queen==1 ~ "Queen",
    QR==0 & Infl==1 ~ "Influencer",
    TRUE ~ NA_character_  # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Inf = factor(QR_Queen_Inf, levels = c( "Queenless Worker","Queenright Worker", "Queen", "Influencer")))


# Plot the data

ggplot(bds_mean_of_means, aes(x = factor(QR_Queen_Inf, levels = c("Queen", "Influencer", "Queenright Worker", "Queenless Worker")), y = Degree)) +
  geom_line(aes(group = Trial), color = "darkgray") +
  geom_point(aes(color = Trial), size = 5) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Mean Number of Interactions per Hour (Unstandardized)") + # Adjust axis labels
  theme_minimal() +
  CONSISTENT_THEME +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))


# Assuming bds_mean_of_means is your dataframe
# Step 1: Create a new grouping variable
bds_mean_of_means$Group <- with(bds_mean_of_means, ifelse(QR_Queen_Inf %in% c("Queen", "Queenright Worker"), "Q + QRw", "Inf + QLw"))
bds_mean_of_means$Group <- factor(bds_mean_of_means$Group, levels = c("Q + QRw", "Inf + QLw"))
# Step 2: Adjust the factor levels for plotting
bds_mean_of_means$QR_Queen_Inf <- factor(bds_mean_of_means$QR_Queen_Inf, levels = c("Queen", "Queenright Worker", "Influencer", "Queenless Worker"))

# Step 3: Plot with a cut in the x-axis using facets
plot_degree <- ggplot(bds_mean_of_means, aes(x = QR_Queen_Inf, y = Degree)) +
  geom_line(aes(group = interaction(Trial, Group)), color = "darkgray") +
  geom_point(aes(color = Trial), size = 2) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Std. Number of Int. per Hour") +
  theme_minimal() +
  CONSISTENT_THEME_NO_ASPECT +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_grid(~Group, scales = "free_x", space = "free_x", switch = "x", labeller = labeller(.rows = label_both, .cols = label_both)) +
  SHARED_THEME

# Step 3: Plot with a cut in the x-axis using facets
plot_disp<- ggplot(bds_mean_of_means, aes(x = QR_Queen_Inf, y = N90.Day4)) +
  geom_line(aes(group = interaction(Trial, Group)), color = "darkgray") +
  geom_point(aes(color = Trial), size = 2) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("N90 (Dispersion)") +
  theme_minimal() +
  CONSISTENT_THEME_NO_ASPECT +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_grid(~Group, scales = "free_x", space = "free_x", switch = "x", labeller = labeller(.rows = label_both, .cols = label_both)) +
  SHARED_THEME

plot_between <- ggplot(bds_mean_of_means, aes(x = QR_Queen_Inf, y = Between)) +
  geom_line(aes(group = interaction(Trial, Group)), color = "darkgray") +
  geom_point(aes(color = Trial), size = 2) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Betweenness") +
  theme_minimal() +
  CONSISTENT_THEME_NO_ASPECT +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_grid(~Group, scales = "free_x", space = "free_x", switch = "x", labeller = labeller(.rows = label_both, .cols = label_both)) +
  SHARED_THEME


plot_grid(plot_degree, plot_disp, plot_between, ncol = 3)
ggsave("figures/manuscript/figure_4.jpeg", width = 8.5, height = 3, dpi = 1200)
