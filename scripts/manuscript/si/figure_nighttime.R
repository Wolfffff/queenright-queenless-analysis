# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(cowplot)
library(reshape2)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

# Filter data to nighttime hours only (Zeit > 17 | Zeit < 8)
bds_night <- bds_all %>%
  filter(Zeit > 17 | Zeit < 8)

# Create detailed nighttime plots by group (similar to S5)
create_nighttime_plot <- function(data, group_label) {
  BigDataSheetQRFilt <- data.frame(data$Hour, data$Degree, data$bodyDegree, data$ID, data$Trial)
  colnames(BigDataSheetQRFilt) <- c("Hour", "Degree", "bodyDegree", "ID", "Trial")
  
  d <- melt(BigDataSheetQRFilt, id = c("Hour", "ID", "Trial"))
  alpha <- ifelse(group_label == "Queen", 1, 0.05)
  
  plot <- ggplot(d, aes(x = as.integer(Hour), y = value / 20, group = variable)) +
    geom_jitter(aes(group = variable, color = variable), size = 1, width = 0.2, alpha = alpha, stroke = 0) +
    geom_smooth(aes(group = variable, color = variable), size = 1, se = FALSE, linetype = 1, method = "loess") +
    scale_color_manual(
      labels = c("Head-to-Head", "Head-to-Body"),
      values = c("#DDBC58", "#023020"),
      guide = guide_legend(direction = "horizontal")
    ) +
    scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA), expand = c(0, 0)) +
    labs(color = "", title = group_label) +
    xlab("Hour") +
    ylab("Std. Time Interacting per Hour (s)") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = c(1, 1.02),
      legend.justification = c(1, 1),
      legend.background = element_rect(fill = alpha("white", 1), linewidth = 0),
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 7),
      panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
      panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.5),
      axis.line.y = element_line(color = "black", size = 0.5),
      strip.text = element_text(size = 10, face = "bold"),
      panel.grid = element_blank(),
      panel.border = element_blank()
    ) +
    ylim(0, 1000)
  
  return(plot)
}

# Filter nighttime data for each group
TotalDeg_night <- bds_night
TotalDeg_night$Group <- TotalDeg_night$QR_Queen_Condition

queen_data_night <- TotalDeg_night %>% filter(Queen == 1)
queenright_data_night <- TotalDeg_night %>% filter(QR_Queen_Condition == "Queenright")
queenless_data_night <- TotalDeg_night %>% filter(QR_Queen_Condition == "Queenless")

# Create detailed nighttime plots for each group
plot_queen_night <- create_nighttime_plot(queen_data_night, "Queen") + ylab("") + xlab("")
plot_queenright_night <- create_nighttime_plot(queenright_data_night, "Queenright Worker") + xlab("") + theme(legend.position = "none")
plot_queenless_night <- create_nighttime_plot(queenless_data_night, "Queenless Worker") + ylab("") + theme(legend.position = "none")

# Combine detailed nighttime plots (S5-style)
combined_nighttime_detail <- plot_grid(plot_queen_night, plot_queenright_night, plot_queenless_night, ncol = 1, align = "v")

# Save the detailed nighttime plot only
ggsave("figures/manuscript/si/nighttime_interactions.jpeg", plot = combined_nighttime_detail, width = 8.5, height = 9, dpi = 600)