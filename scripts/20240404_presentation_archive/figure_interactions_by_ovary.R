list_of_packages <- c("lme4", "ggplot2", "sp", "tidyverse", "car", "raster", "pak", "wesanderson", "forcats", "ggside")
# pak::pkg_install(list_of_packages)
for (p in list_of_packages) {
  library(p, character.only = TRUE)
}
source("../scripts/utils/base_utils.R")
agg_df <- process_files(suffix = "Cent.csv")
filtered_df <- filter_data("/Users/wolf/git/queenright-queenless-analysis/meta/experimental_tag_list.csv", agg_df)

TotalCent <- filtered_df

# Get unique ids by group in TotalCent
TotalCent %>%
  group_by(Col) %>%
  summarise(Unique = length(unique(ID)))

TotalCentMean <- TotalCent %>%
  group_by(ID) %>%
  summarise(
    Degree = mean(Degree),
    Closeness = mean(Closeness),
    Betweenness = mean(Betweenness),
    QR = first(QR),
    Col = first(Col),
    Queen = first(Queen)
  )

Ovaries <- process_ovaries("ovary_measurements.csv")

TotalCentSum <- TotalCent %>%
  group_by(ID) %>%
  summarise(Degree = mean(Degree), QR = first(QR),Col = first(Col), Queen = first(Queen))

TotalCentSum <- merge(TotalCentSum, Ovaries, by = "ID", all.x = TRUE)

# Swap levels of QR to match the first plot
TotalCentSum$Treatment <- ifelse(TotalCentSum$QR, "Queenright Worker", "Queenless Worker")

# Drop rows with NA in Degree or AverageWidth
df <- TotalCentSum[complete.cases(TotalCentSum$Degree, TotalCentSum$AverageWidth), ]
df$Alpha <- ifelse(df$Queen, 0.5, 0.75)
df$PointSize <- ifelse(df$Queen, 2, 1)


ggplot(df, aes(x = AverageWidth, y = Degree/20, colour = interaction(QR, Queen))) +
  geom_xsidedensity(data = subset(df, Queen == FALSE),size=.5,aes(fill = factor(Treatment)),alpha=0.1) +
  geom_ysidedensity(data = subset(df, Queen == FALSE),size=.5,aes(fill = factor(Treatment)),alpha=0.1) +
 geom_smooth(
    data = subset(df, Queen == FALSE), # Subset the data to exclude queens
    method = lm, # Change method to lm for linear model
    se = T, # Standard error
    size = .75, # Adjust size to match first plot
    linetype = 1, # Line type
    aes(color = interaction(QR,Queen))
  ) + # Color by treatment
  geom_point(aes(color = interaction(QR, Queen)), alpha = 1,size= df$PointSize) +
  scale_color_manual(values = c("#CEB175", "#629CC0", "#E54E21"), labels = c("Queenless Worker", "Queenright Worker", "Queen"), guide = guide_legend(direction = "vertical")) + # Adjust colors and labels
  scale_x_continuous() + # Use a continuous scale for x
  scale_y_continuous() + # Use a continuous scale for y
  # labs(title="Total Interaction Count vs Max Oocyte Width", color = "Treatment") +  # Adjust title and legend title
  xlab("Mean Width of Two Largest Oocytes (mm)") +
  ylab("Mean Time Spent Interacting per Hour (s)") + # Adjust axis labels
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"), # Keep vertical grid lines
    panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"), # Keep vertical grid lines
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(), # Remove horizontal grid lines
    axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
    axis.line.y = element_line(color = "black", size = 0.5), # Add y-axis line
    strip.text = element_text(size = 14, face = "bold"), # Make facet plot titles larger and bold
    axis.text.y.right = element_blank(), # Remove right y-axis text
    axis.ticks.y.right = element_blank(), # Remove right y-axis ticks
    aspect.ratio = 1
  ) +
  theme_ggside_void()

ggsave("../figures/interactions_by_ovary.jpg", width = 6.25, height = 6.25, dpi = 600)


df_rooibos <- df[df$Colony == "RooibosTea", ]
ggplot(df_rooibos, aes(x = AverageWidth, y = Degree/20, colour = QR)) +
  geom_xsidedensity(data = subset(df_rooibos, Queen == FALSE),size=.5,aes(fill = factor(Treatment)),alpha=0.1) +
  geom_ysidedensity(data = subset(df_rooibos, Queen == FALSE),size=.5,aes(fill = factor(Treatment)),alpha=0.1) +
 geom_smooth(
    # data = subset(df, Queen == FALSE), # Subset the data to exclude queens
    method = lm, # Change method to lm for linear model
    se = T, # Standard error
    size = .75, # Adjust size to match first plot
    linetype = 1, # Line type
    aes(color = QR)
  ) + # Color by treatment
  geom_point(aes(color = QR), alpha = 1) +
  scale_color_manual(values = c("#CEB175", "#629CC0"), labels = c("Queenless Worker", "Queenright Worker"), guide = guide_legend(direction = "vertical")) + # Adjust colors and labels
  scale_x_continuous() + # Use a continuous scale for x
  scale_y_continuous() + # Use a continuous scale for y
  # labs(title="Total Interaction Count vs Max Oocyte Width", color = "Treatment") +  # Adjust title and legend title
  xlab("Mean Width of Two Largest Oocytes (mm)") +
  ylab("Mean Time Spent Interacting per Hour (s)") + # Adjust axis labels
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"), # Keep vertical grid lines
    panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"), # Keep vertical grid lines
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(), # Remove horizontal grid lines
    axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
    axis.line.y = element_line(color = "black", size = 0.5), # Add y-axis line
    strip.text = element_text(size = 14, face = "bold"), # Make facet plot titles larger and bold
    axis.text.y.right = element_blank(), # Remove right y-axis text
    axis.ticks.y.right = element_blank(), # Remove right y-axis ticks
    aspect.ratio = 1
  ) +
  theme_ggside_void()
ggsave("../figures/interactions_by_ovary.jpg", width = 6.25, height = 6.25, dpi = 600)
