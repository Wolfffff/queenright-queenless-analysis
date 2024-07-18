# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(reshape2)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

# Degree — Head-Head vs Head-Body ----------
TotalDeg <- bds
# Add a new column to TotalDeg to indicate the group
TotalDeg$Group <- TotalDeg$QR_Queen_Condition

# Only queen
TotalDeg <- TotalDeg %>%
  filter(Queen == 0)

# Create the plot
BigDataSheetQRFilt <- data.frame(bds$Hour, bds$Degree, bds$bodyDegree, bds$ID, bds$Trial)
colnames(BigDataSheetQRFilt) <- c("Hour", "Degree", "bodyDegree", "ID", "Trial")

d <- melt(BigDataSheetQRFilt, id = c("Hour", "ID", "Trial"))

bds$Alpha <- ifelse(bds$Queen, .5, 0.005)
bds <- bds[sample(nrow(bds)), ]
bds$PointSize <- ifelse(bds$Queen, .003, .001)

plot_int_type <- ggplot(d, aes(x = as.integer(Hour), y = value / 20, group = variable)) +
  geom_jitter(aes(group = variable, color = variable), size = 0.3, width = 0.2, alpha = 0.05) +
  geom_smooth(aes(group = variable, color = variable), size = 1, se = FALSE, linetype = 1, method = "loess") +
  scale_color_manual(
    labels = c("Head-to-Head", "Head-to-Body"),
    values = c("#DDBC58", "#023020"),
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
    legend.background = element_rect(fill = alpha("white", 1), linewidth = 0),
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

ggsave("figures/manuscript/si/figure_s1.jpeg", plot_int_type, width = 8.5, height = 4, dpi = 1200)

fm <- lmer(formula = value ~ 1 + Hour + variable + (1|Trial), data = d) #to run the model
fm.null <- lmer(formula = value ~ 1 + Hour + (1|Trial), data = d) #to run the model
anova(fm, fm.null)
print(fm)