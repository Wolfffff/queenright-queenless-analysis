# Load Degree Data — Head And Body ----------------------------------------
list_of_packages <- c("lme4", "ggplot2", "sp", "tidyverse", "car", "raster", "pak")
# pak::pkg_install(list_of_packages)
for (p in list_of_packages) {
  library(p, character.only = TRUE)
}

source("../scripts/utils/base_utils.R")
agg_df <- process_files(suffix="Cent.csv")
filtered_df <- filter_data("/Users/wolf/git/queenright-queenless-analysis/meta/experimental_tag_list.csv",agg_df)


# Get unique ids by group in TotalCent
filtered_df %>%
  group_by(Col) %>%
  summarise(Unique = length(unique(ID)))

TotalCent <- filtered_df
TotalCentMean <- aggregate(cbind(Degree, Closeness, Betweenness, QR, Queen) ~ ID, TotalCent, mean)
TotalCentMean$Col <- sub("_[^_]+$", "", TotalCentMean$ID)


TotalCentMeanRanked <- TotalCentMean %>%
  group_by(Col) %>%
  mutate(Rank = order(order(Degree, decreasing = TRUE)))

TotalCentMeanRanked <- TotalCentMeanRanked[order(TotalCentMeanRanked$Degree), ]

Ovaries <- process_ovaries("ovary_measurements.csv")

TotalCentSum <- TotalCent %>%
  group_by(ID) %>%
  summarise(Degree = sum(Degree), Closeness = sum(Closeness),Betweenness=sum(Betweenness), QR = first(QR),Col = first(Col), Queen = first(Queen))

TotalCentSum <- merge(TotalCentSum, Ovaries, by = "ID")

# Swap levels of QR to match the first plot
TotalCentSum$Treatment <- ifelse(TotalCentSum$QR, "Queenright Worker", "Queenless Worker")

# If Queen is TRUE, set Treatment to Queen
TotalCentSum$Treatment <- ifelse(TotalCentSum$Queen, "Queen", TotalCentSum$Treatment)

queen <- TotalCentSum[TotalCentSum$Queen,] 
queen$Queen = TRUE

# Drop queens 
TotalCentSum <- TotalCentSum[!TotalCentSum$Queen,] 

assign_groups <- function(df) {
  df <- df %>%
    group_by(Colony) %>%
    mutate(Group = "Middle",  # Default group is Middle
           Group = if_else(row_number(-Degree) <= 10, "Top 10", Group),  # Assign Top 10
           Group = if_else(row_number(Degree) <= 10, "Bottom 10", Group)) %>%  # Assign Bottom 10
    ungroup()  # Remove grouping
  return(df)
}

# Assuming 'queen' is a dataframe that includes a 'Colony' and 'Degree' column
queen <- assign_groups(queen)


# Apply the function to each Colony
TotalCentSum <- TotalCentSum %>%
  group_by(Col) %>%
  do(assign_groups(.)) %>%
  ungroup()

# Check dist

TotalCentSum <- rbind(TotalCentSum, queen)
dim(TotalCentSum[TotalCentSum$Col == "20221123_1543_AmericanoLatte_QR",])
TotalCentSum$source_colony <- sapply(strsplit(TotalCentSum$Col, "_"), `[`, 1) 

# Count by group
TotalCentSum %>% group_by(Group) %>% summarise(n = n())
#### stats ####

  df_for_stats <- TotalCentSum
  # drop Middle group
  df_for_stats <- df_for_stats[df_for_stats$Group != "Middle", ]
  # remove queens
  # df_for_stats$zeit <- as.numeric(df_for_stats$Hour) %% 24
  # df_for_stats$day <- factor(as.numeric(df_for_stats$Hour) %/% 24 + 1)  # Convert day to a factor
  df_for_stats$source_colony <- sapply(strsplit(df_for_stats$Col, "_"), `[`, 1)
  df_for_stats$source_colony <- factor(df_for_stats$source_colony, levels = c("RooibosTea", "MexHotChoc", "20230213", "20221209", "20221123"))
  # reverse levels of QR to match the first plot
  # df_for_stats$QR <- factor(df_for_stats$QR, levels = c(1, 0))

  df_for_stats_all <- df_for_stats %>% filter(Group %in% c("Top 10", "Bottom 10"))
  fm <- lmer(AverageWidth ~ 1 + QR*Group + (Group | source_colony), data = df_for_stats_all)

  # Just Top 10 from df_for_stats
  df_for_stats_t10 <- df_for_stats[df_for_stats$Group == "Top 10",]
  fm.null <- lmer(AverageWidth ~ 1 + (1 | source_colony), data = df_for_stats_t10)
  fm <- lmer(AverageWidth ~ 1 + QR + (1 | source_colony), data = df_for_stats_t10)

  summary(fm)

  # Just Bottom 10 from df_for_stats
  df_for_stats_b10 <- df_for_stats[df_for_stats$Group == "Bottom 10",]
  save(df_for_stats_b10, file = "../figures/df_for_stats_b10.RData")
  fm.null <- lmer(AverageWidth ~ 1 + (1 | source_colony), data = df_for_stats_b10)
  fm <- lmer(AverageWidth ~ 1 + QR + (1 | source_colony), data = df_for_stats_b10)
  ranef(fm)
  summary(fm)
#### end ####

df_for_stats_all %>% group_by(source_colony,Group) %>% summarise(mean(AverageWidth), sd(AverageWidth))

# group by combo of Treatment, Colony, and Queen
grouped_sum <- TotalCentSum %>%
  group_by(Treatment, Col, Queen, Group) %>%
  summarise(AverageWidth_var=var(AverageWidth,na.rm=TRUE),AverageLength_var=var(AverageLength,na.rm=TRUE), Degree_var=var(Degree), Degree = mean(Degree), AverageLength = mean(AverageLength,na.rm=TRUE), AverageWidth = mean(AverageWidth,na.rm=TRUE),Closeness=mean(Closeness), Betweenness = mean(Betweenness), .groups = 'drop')
# Only workers for this plot
grouped_sum <- grouped_sum[!grouped_sum$Queen, ]

# get value before first _ in source_colony
grouped_sum$source_colony <- sapply(strsplit(grouped_sum$Col, "_"), `[`, 1)                  

library(wesanderson)
library(forcats)

grouped_sum$source_colony <- factor(grouped_sum$source_colony, levels = c("RooibosTea", "MexHotChoc", "20230213", "20221209", "20221123"))
# Drop middle group
grouped_sum <- grouped_sum[grouped_sum$Group != "Middle",]

ggplot(grouped_sum, aes(x = fct_rev(Treatment), y = AverageWidth)) + 
  geom_line(aes(group = source_colony),color="darkgray") +
  geom_point(aes(color = source_colony), size = 5) +
  # geom_boxplot(aes(color = source_colony), alpha = 0.5) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color="Source Colony") +
  ylab("Mean Width of Two Largest Oocytes (mm)") + # Adjust axis labels
  theme_minimal() +
  theme(
    # text = element_text(size = 16),
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
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_wrap(~Group)

ggsave("../figures/top_bottom_ovary_deg.jpg", width = 8.5, height = 4, dpi = 600)



grouped_sum$source_colony <- factor(grouped_sum$source_colony, levels = c("RooibosTea", "MexHotChoc", "20230213", "20221209", "20221123"))
# Drop middle group
grouped_sum <- grouped_sum[grouped_sum$Group != "Middle",]

ggplot(grouped_sum, aes(x = Group, y = Degree)) + 
  geom_line(aes(group = source_colony),color="darkgray") +
  geom_point(aes(color = source_colony), size = 5) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  # labs(color="Source Colony") +
  ylab("Degree") + # Adjust axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    # legend.position = "none",
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
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_wrap(~Treatment)

ggsave("../figures/fct_treatment_top_bottom_ovary_deg.jpg", width = 8.5, height = 4, dpi = 600)


library(ggbeeswarm)
df_for_plot <- TotalCentSum
df_for_plot <- df_for_plot[!df_for_plot$Queen, ]

# get value before first _ in source_colony
df_for_plot$source_colony <- sapply(strsplit(df_for_plot$Col, "_"), `[`, 1)                  

df_for_plot$source_colony <- factor(df_for_plot$source_colony, levels = c("RooibosTea", "MexHotChoc", "20230213", "20221209", "20221123"))

df_for_plot <- df_for_plot[df_for_plot$Group != "Middle",]

ggplot(df_for_plot, aes(x = fct_rev(Treatment), y = Degree,Group)) + 
  # geom_line(aes(group = source_colony),color="darkgray") +
  geom_beeswarm(aes(color = source_colony), size = 1,alpha=.5) +
  # geom_point(aes(color = source_colony), size = 5) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color="Source Colony") +
  ylab("Degree") + # Adjust axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    # legend.position = "none",
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
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_wrap(~Group)

ggsave("../figures/top_bottom_ovary_deg_swarm.jpg", width = 8.5, height = 4, dpi = 600)




# 
ggplot(grouped_sum, aes(x = Group, y = Betweenness)) + 
  geom_line(aes(group = source_colony),color="darkgray") +
  geom_point(aes(color = source_colony), size = 5) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color="Source Colony") +
  ylab("Betweenness") + # Adjust axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    # legend.position = "none",
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
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_wrap(~Treatment)

ggsave("../figures/top_bottom_ovary_betweenness.jpg", width = 8.5, height = 4, dpi = 600)

ggplot(grouped_sum, aes(x = Group, y = Closeness)) + 
  geom_line(aes(group = source_colony),color="darkgray") +
  geom_point(aes(color = source_colony), size = 5) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color="Source Colony") +
  ylab("Closeness") + # Adjust axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    # legend.position = "none",
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
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_wrap(~Treatment)

ggsave("../figures/top_bottom_ovary_closeness.jpg", width = 8.5, height = 4, dpi = 600)
