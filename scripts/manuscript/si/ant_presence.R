# Load required libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(gridExtra)
library(reshape2)

# Load data
source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

# Use bds_all (unfiltered data) as equivalent to BigDataSheet
BDSRanked <- aggregate(cbind(Degree, Close, Eigen, Between, QR, Queen, boutDegree, boutBetween, boutClose, boutEigen, bodyDegree, bodyBetween, bodyClose, bodyEigen, AverageBoutLength, Presence, AntPresence, mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq) ~ Bee, bds_all, mean)
BDSRanked$Trial <- str_extract(BDSRanked$Bee, ".+?(?=_)")
BDSRanked$Col <- str_extract(BDSRanked$Bee, ".+?(?=#)")
BDSRanked <- BDSRanked %>%
  arrange(Col, AntPresence) %>%
  group_by(Col) %>%
  mutate(Rank = rank(AntPresence))

ggplot(BDSRanked, aes(x = Rank, y = AntPresence)) +
  geom_line(data = subset(BDSRanked, QR == 1), aes(group = Col, color = Col)) +
  geom_point(data = subset(BDSRanked, Queen == 1), color = "purple", size = 3)

BDSRankedTotal <- BDSRanked %>%
  arrange(Col, Presence) %>%
  group_by(Col) %>%
  mutate(Rank = rank(Presence))

ggplot(BDSRankedTotal, aes(x = Rank, y = Presence)) +
  geom_line(data = subset(BDSRankedTotal, QR == 1), aes(group = Col, color = Col)) +
  geom_point(data = subset(BDSRankedTotal, Queen == 1), color = "purple", size = 3)

# Comprehensive multi-panel plot using all available data

# Create a long format dataset for network metrics
network_metrics <- BDSRanked %>%
  select(Bee, Col, QR, Queen, Degree, Close, Eigen, Between,
         boutDegree, boutBetween, boutClose, boutEigen,
         bodyDegree, bodyBetween, bodyClose, bodyEigen) %>%
  pivot_longer(cols = c(Degree, Close, Eigen, Between,
                        boutDegree, boutBetween, boutClose, boutEigen,
                        bodyDegree, bodyBetween, bodyClose, bodyEigen),
               names_to = "Metric", values_to = "Value")

# Create behavioral metrics dataset
behavioral_metrics <- BDSRanked %>%
  select(Bee, Col, QR, Queen, AverageBoutLength, Presence, AntPresence,
         mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq) %>%
  pivot_longer(cols = c(AverageBoutLength, Presence, AntPresence,
                        mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq),
               names_to = "Metric", values_to = "Value")

# Network metrics comparison plot
p1 <- ggplot(network_metrics, aes(x = QR, y = Value, fill = factor(QR))) +
  geom_boxplot(alpha = 0.7) +
  geom_point(data = subset(network_metrics, Queen == 1),
             aes(color = "Queen"), size = 2, position = position_jitter(width = 0.2)) +
  facet_wrap(~Metric, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c("0" = "lightcoral", "1" = "lightblue")) +
  scale_color_manual(values = c("Queen" = "purple")) +
  labs(title = "Network Metrics: Queenright vs Queenless",
       x = "Queenright Status", y = "Metric Value",
       fill = "Queenright", color = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Behavioral metrics comparison plot
p2 <- ggplot(behavioral_metrics, aes(x = QR, y = Value, fill = factor(QR))) +
  geom_boxplot(alpha = 0.7) +
  geom_point(data = subset(behavioral_metrics, Queen == 1),
             aes(color = "Queen"), size = 2, position = position_jitter(width = 0.2)) +
  facet_wrap(~Metric, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c("0" = "lightcoral", "1" = "lightblue")) +
  scale_color_manual(values = c("Queen" = "purple")) +
  labs(title = "Behavioral Metrics: Queenright vs Queenless",
       x = "Queenright Status", y = "Metric Value",
       fill = "Queenright", color = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Correlation heatmap of all metrics
cor_data <- BDSRanked %>%
  select(Degree, Close, Eigen, Between, boutDegree, boutBetween, boutClose, boutEigen,
         bodyDegree, bodyBetween, bodyClose, bodyEigen, AverageBoutLength,
         Presence, AntPresence, mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq) %>%
  cor(use = "complete.obs")

cor_melted <- melt(cor_data)
p3 <- ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  labs(title = "Correlation Matrix of All Metrics") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Colony-level summary plot
colony_summary <- BDSRanked %>%
  group_by(Col, QR) %>%
  summarise(
    avg_degree = mean(Degree, na.rm = TRUE),
    avg_presence = mean(Presence, na.rm = TRUE),
    avg_ant_presence = mean(AntPresence, na.rm = TRUE),
    avg_velocity = mean(mean_vel, na.rm = TRUE),
    .groups = "drop"
  )

p4 <- ggplot(colony_summary, aes(x = avg_degree, y = avg_presence,
                                 color = factor(QR), size = avg_ant_presence)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("0" = "red", "1" = "blue")) +
  labs(title = "Colony-Level Summary: Degree vs Presence",
       x = "Average Degree", y = "Average Presence",
       color = "Queenright", size = "Avg Ant Presence") +
  theme_minimal()

# Combine all plots
comprehensive_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Save the comprehensive plot
ggsave("comprehensive_analysis_plot.png", comprehensive_plot,
       width = 16, height = 12, dpi = 300)
