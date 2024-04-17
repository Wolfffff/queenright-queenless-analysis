# Load Degree Data — Head And Body ----------------------------------------
list_of_packages <- c("lme4", "ggplot2", "sp", "tidyverse", "car", "ggthemes", "dplyr", "dineq", "plyr")
# pak::pkg_install(list_of_packages)
for (p in list_of_packages) {
  library(p, character.only = TRUE)
}
source("../scripts/utils/base_utils.R")
agg_df <- process_files(suffix = "Cent.csv")
filtered_df <- filter_data("/Users/wolf/git/queenright-queenless-analysis/meta/experimental_tag_list.csv", agg_df)

TotalCent <- filtered_df
TotalCentMean <- aggregate(cbind(Degree, Closeness, Betweenness, QR, Queen) ~ ID, TotalCent, mean)

TotalCentMean$Col <- sub("_[^_]+$", "", TotalCentMean$ID)

TotalCent$Alpha <- ifelse(TotalCent$Queen, .5, 0.005)
TotalCent <- TotalCent[sample(nrow(TotalCent)), ]
TotalCent$PointSize <- ifelse(TotalCent$Queen, .003, .001)

library(dplyr)

TotalCent <- TotalCent %>%
  mutate(QR_Queen_Condition = case_when(
    !QR & !Queen ~ "Queenless",
    QR & !Queen ~ "Queenright",
    Queen ~ "Queen",
    TRUE ~ NA_character_  # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queenless", "Queenright", "Queen")))


ggplot(TotalCent, aes(x = as.integer(Hour), y = Degree, group = ID)) +
  geom_jitter(aes(color = QR_Queen_Condition, alpha = Alpha, size = PointSize)) +
  geom_smooth(aes(group = QR_Queen_Condition, color = QR_Queen_Condition)) +
  scale_size(range = c(.001, .2)) +
  scale_color_manual(
    labels = c( "Queenright Worker","Queenless Worker", "Queen"),
    values = c("#629CC0","#CEB175",  "#E54E21"),
    guide = guide_legend(direction = "horizontal")
  ) +
  scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA), expand = c(0, 0)) + # Expand limits to include 0
  labs(color = "") +
  xlab("Hour") +
  ylab("Mean Number of Interactions per Hour") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(1, 1.02), # Move legend to top right
    legend.justification = c(1, 1), # Align legend to top right
    legend.background = element_rect(fill = alpha("white", 1)),
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
  # scale_y_log10() +
  guides(alpha = "none", size = "none")

ggsave("../figures/DegreeCent.png", width = 8.5, height = 3, dpi = 600, bg = "white")

ggplot(TotalCent, aes(x = Hour, y = Closeness, group = ID)) +
  geom_jitter(aes(color = interaction(QR, Queen), alpha = Queen)) +
  geom_smooth(aes(group = interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() +
  labs(title = "Closeness Cent in Queens, Queenless Workers, Queenright Workers") +
  scale_alpha_discrete(range = c(0.1, 0.4)) +
  scale_color_manual(
    name = "Bee",
    labels = c("QL Worker", "QR Worker", "Queen"),
    values = c("#161414", "#629CC0", "#7851A9")
  )

ggsave("../figures/ClosenessCent.png", width = 4.25, height = 3, dpi = 600)

ggplot(TotalCent, aes(x = Hour, y = Betweenness, group = ID)) +
  # geom_jitter(aes(color = interaction(QR, Queen), alpha = Queen)) +
  geom_smooth(aes(group = interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() +
  labs(title = "Betweenness Cent in Queens, Queenless Workers, Queenright Workers") +
  scale_alpha_discrete(range = c(0.1, 0.4)) +
  scale_color_manual(
    name = "Bee",
    labels = c("QL Worker", "QR Worker", "Queen"),
    values = c("#ff0000", "#629CC0", "#7851A9")
  )

ggsave("../figures/BetweennessCent.png", width = 4.25, height = 3, dpi = 600)


ggplot(TotalCentMean, aes(x = Degree, y = Closeness)) +
  geom_point(aes(color = interaction(factor(QR), factor(Queen)), alpha = factor(Queen))) +
  theme_classic() +
  labs(title = "Degree Cent vs Closeness Cent") +
  scale_alpha_discrete(range = c(0.3, 0.8)) +
  scale_color_manual(
    name = "Bee",
    labels = c("QL Worker", "QR Worker", "Queen"),
    values = c("#161414", "#629CC0", "#7851A9")
  )

ggplot(TotalCentMean, aes(x = Degree, y = Betweenness)) +
  geom_point(aes(color = interaction(factor(QR), factor(Queen)), alpha = factor(Queen))) +
  theme_classic() +
  labs(title = "Degree Cent vs Betweenness Cent") +
  scale_alpha_discrete(range = c(0.3, 0.8)) +
  scale_color_manual(
    name = "Bee",
    labels = c("QL Worker", "QR Worker", "Queen"),
    values = c("#161414", "#629CC0", "#7851A9")
  )

ggplot(TotalCentMean, aes(x = Betweenness, y = Closeness)) +
  geom_point(aes(color = interaction(factor(QR), factor(Queen)), alpha = factor(Queen))) +
  theme_classic() +
  labs(title = "Betweenness Cent vs Closeness Cent") +
  scale_alpha_discrete(range = c(0.3, 0.8)) +
  scale_color_manual(
    name = "Bee",
    labels = c("QL Worker", "QR Worker", "Queen"),
    values = c("#161414", "#629CC0", "#7851A9")
  )

ggplot(TotalCentMean, aes(x = Degree, y = Closeness)) +
  geom_point(aes(color = Col)) +
  theme_classic() +
  labs(title = "Degree Cent vs Closeness Cent, Colored by ID") +
  scale_alpha_discrete(range = c(0.3, 0.8))


fm <- lmer(formula = values ~ 1 + QR + Day + Day:QR + (1 | Col), data = TotalAss) # to run the model
fm.null <- lmer(formula = values ~ 1 + Day + (1 | Col), data = TotalAss) # to run the model
anova(fm, fm.null)


# Assuming 'degree' is the column you want to plot
list_of_cols <- unique(TotalCentMeanRanked$Col)

for (col in list_of_cols) {
  subset_data <- subset(TotalCentMeanRanked, Col == col)

  # # Sum 'Degree' by 'ID'
  # sum_data <- subset_data %>%
  #   group_by(ID) %>%
  #   summarize(Degree = sum(Degree, na.rm = TRUE),
  #             Queen = first(Queen))

  p <- ggplot(subset_data, aes(x = Degree, y = ID, color = as.factor(Queen))) +
    geom_point() +
    labs(
      title = paste("Cleveland Dot Plot for", col),
      x = "Degree",
      y = "ID"
    ) +
    theme_minimal() +
    xlim(0, max(subset_data$Degree))

  ggsave(paste0("../figures/cleveland_dot_plot_", col, ".jpg"), plot = p, width = 8.5, height = 5, dpi = 600)

  print(p)
}
