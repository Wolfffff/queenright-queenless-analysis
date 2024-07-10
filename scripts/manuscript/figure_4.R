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
  scale_color_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Std. Interactions per Hour") + # Adjust axis labels
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
plot_disp<- ggplot(bds_mean_of_means, aes(x = QR_Queen_Inf, y = N90.Day4)) +
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

plot_between <- ggplot(bds_mean_of_means, aes(x = QR_Queen_Inf, y = Between)) +
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


bds_oi <- bds %>% mutate(QR_Queen_Inf = case_when(
    QR==0 & Infl==0 ~ "Queenless Worker",
    QR==1 & Queen==0 ~ "Queenright Worker",
    Queen==1 ~ "Queen",
    QR==0 & Infl==1 ~ "Influencer",
    TRUE ~ NA_character_  # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Inf = factor(QR_Queen_Inf, levels = c( "Queenless Worker","Queenright Worker", "Queen", "Influencer")))

bds_oi$OvaryIndex <- bds$AverageOvaryWidth / bds$AverageWingLength
# Get OI per bee
bds_oi <- bds_oi %>%
  group_by(Bee) %>%
  summarise(Trial = first(Trial),QR_Queen_Inf = first(QR_Queen_Inf), OvaryIndex = mean(OvaryIndex, na.rm = TRUE))

# Filter NAs
bds_oi$Group <- with(bds_oi, ifelse(QR_Queen_Inf %in% c("Queen", "Queenright Worker"), "Q + QRw", "Inf + QLw"))
bds_oi$Group <- factor(bds_oi$Group, levels = c("Q + QRw", "Inf + QLw"))

# Mean by group
bds_oi <- bds_oi %>%
  group_by(QR_Queen_Inf, Trial) %>%
  summarise(Group = first(Group), Trial = first(Trial), OvaryIndex = mean(OvaryIndex, na.rm = TRUE))

# Plot the same but OvaryIndex
plot_oi <- ggplot(bds_oi, aes(x = QR_Queen_Inf, y = OvaryIndex)) +
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
plot_oi
plot_grid(plot_degree, plot_disp, plot_between,plot_oi, ncol = 4)
ggsave("figures/manuscript/figure_4.jpeg", width = 8.5, height = 3, dpi = 1200)
