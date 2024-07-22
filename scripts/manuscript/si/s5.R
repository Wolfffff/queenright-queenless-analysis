# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(reshape2)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

# Define a function to create the plots
create_plot <- function(data, group_label) {
  BigDataSheetQRFilt <- data.frame(data$Hour, data$Degree, data$bodyDegree, data$ID, data$Trial)
  colnames(BigDataSheetQRFilt) <- c("Hour", "Degree", "bodyDegree", "ID", "Trial")

  d <- melt(BigDataSheetQRFilt, id = c("Hour", "ID", "Trial"))
  if (group_label == "Queen") {
    alpha <- 1
  } else {
    alpha <- 0.05
  }


  plot <- ggplot(d, aes(x = as.integer(Hour), y = value / 20, group = variable)) +
    geom_jitter(aes(group = variable, color = variable), size = 1, width = 0.2, alpha = alpha, stroke = 0) +
    geom_smooth(aes(group = variable, color = variable), size = 1, se = FALSE, linetype = 1, method = "loess") +
    scale_color_manual(
      labels = c("Head-to-Head", "Head-to-Body"),
      values = c("#DDBC58", "#023020"),
      guide = guide_legend(direction = "horizontal")
    ) +
    scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA), expand = c(0, 0)) + # Expand limits to include 0
    labs(color = "", title = group_label) +
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
    ylim(0, 1000)
  guides(alpha = "none", size = "none")

  return(plot)
}

# Filter the data for each group
TotalDeg <- bds
TotalDeg$Group <- TotalDeg$QR_Queen_Condition

queen_data <- TotalDeg %>% filter(Queen == 1)
queenright_data <- TotalDeg %>% filter(QR_Queen_Condition == "Queenright")
queenless_data <- TotalDeg %>% filter(QR_Queen_Condition == "Queenless")

# Create plots for each group
plot_queen <- create_plot(queen_data, "Queen") + ylab("") + xlab("")
plot_queenright <- create_plot(queenright_data, "Queenright Worker") + xlab("") + theme(legend.position = "none")
plot_queenless <- create_plot(queenless_data, "Queenless Worker") + ylab("") + theme(legend.position = "none")

library(cowplot)

# Stack the plots on top of each other and align them vertically
combined_plot <- plot_grid(plot_queen, plot_queenright, plot_queenless, ncol = 1, align = "v")

# Save the combined plot
ggsave("figures/manuscript/si/figure_s5.jpeg", combined_plot, width = 8.5, height = 9, dpi = 600)
