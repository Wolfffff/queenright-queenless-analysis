# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(reshape2)

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

# Degree — Head-Head vs Head-Body -----------------------------------------------------------
TotalDeg <- bds
# Add a new column to TotalDeg to indicate the group
TotalDeg$Group <- ifelse(TotalDeg$QR & !TotalDeg$Queen, "Queenright Workers",
  ifelse(!TotalDeg$QR, "Queenless Workers",
    ifelse(TotalDeg$Queen, "Queens", NA)
  )
)

# Order the factor levels of the 'Group' column
TotalDeg$Group <- factor(TotalDeg$Group, levels = c("Queenless Workers", "Queenright Workers", "Queens"))


# Only queen
TotalDeg <- TotalDeg %>%
  filter(Queen == 0)

# Create the plot
# Add a new column for alpha levels
# Create the plot
BigDataSheetQRFilt = data.frame(bds$Hour,bds$Degree,bds$bodyDegree, bds$ID,bds$Trial)
colnames(BigDataSheetQRFilt)<- c("Hour","Degree","bodyDegree","ID","Trial") 

d <- melt(BigDataSheetQRFilt, id=c("Hour","ID","Trial"))

bds$Alpha <- ifelse(bds$Queen, .5, 0.005)
bds <- bds[sample(nrow(bds)), ]
bds$PointSize <- ifelse(bds$Queen, .003, .001)

plot_int_type <- ggplot(d, aes(x = as.integer(Hour), y = value/20, group = variable)) +
  geom_jitter(aes(group = variable, color = variable), size = 0.3, width = 0.2,alpha=0.05) +
  geom_smooth(aes(group = variable, color = variable), size = 1, se = FALSE, linetype = 1, method = "loess") +
  scale_color_manual(
    labels = c( "Head-to-Head", "Head-to-Body"),
    values = c( "#DDBC58", "#023020"),
    guide = guide_legend(direction = "horizontal")
  ) +
  scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA), expand = c(0, 0)) + # Expand limits to include 0
  labs(color = "") +
  xlab("Hour") +
  ylab("Std. Time Interacting per Hour (s)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(1, 1.02), # Move legend to top right
    legend.justification = c(1, 1), # Align legend to top right
    legend.background = element_rect(fill = alpha("white", 1),linewidth=0),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"), # Keep vertical grid lines
    panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"), # Keep vertical grid lines
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(), # Remove horizontal grid lines
    axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
    axis.line.y = element_line(color = "black", size = 0.5), # Add y-axis line
    strip.text = element_text(size = 10, face = "bold"), # Make facet plot titles larger and bold
    panel.grid = element_blank(),
    panel.border = element_blank()
  ) +
  guides(alpha = "none", size = "none")

ggsave("figures/manuscript/fig_s1_interaction_type.png", plot_int_type, width = 8.5, height = 4, dpi = 1200)

fm <- lmer(formula = value ~ 1 + Hour + variable + (1|Trial), data = d) #to run the model
fm.null <- lmer(formula = value ~ 1 + Hour + (1|Trial), data = d) #to run the model
anova(fm, fm.null)
print(fm)