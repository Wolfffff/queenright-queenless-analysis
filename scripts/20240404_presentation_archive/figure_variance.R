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
dim(TotalCentMeanRanked)


Ovaries <- process_ovaries("OvaryMeasurements.csv")

TotalCentSum <- TotalCent %>%
  group_by(ID) %>%
  summarise(Degree = sum(Degree), QR = first(QR),Col = first(Col), Queen = first(Queen))

TotalCentSum <- merge(TotalCentSum, Ovaries, by = "ID", all.x = TRUE)

# Swap levels of QR to match the first plot
TotalCentSum$Treatment <- ifelse(TotalCentSum$QR, "Queenright Worker", "Queenless Worker")

# If Queen is TRUE, set Treatment to Queen
TotalCentSum$Treatment <- ifelse(TotalCentSum$Queen, "Queen", TotalCentSum$Treatment)

# Plot distributions and group by Colony and Treatment

# TotalCentSum[which(TotalCentSum$Degree == max(TotalCentSum$Degree)), ]

# group by combo of Treatment, Colony, and Queen
grouped_sum <- TotalCentSum %>%
  group_by(Treatment, Col, Queen) %>%
  summarise(AverageWidth_var=var(AverageWidth,na.rm=TRUE),AverageLength_var=var(AverageLength,na.rm=TRUE), Degree_var=var(Degree), Degree = mean(Degree), AverageLength = mean(AverageLength,na.rm=TRUE), AverageWidth = mean(AverageWidth,na.rm=TRUE), .groups = 'drop')
# Only workers for this plot
grouped_sum <- grouped_sum[!grouped_sum$Queen, ]

# get value before first _ in source_colony
grouped_sum$source_colony <- sapply(strsplit(grouped_sum$Col, "_"), `[`, 1)                  

# if is QR make treatment "Queen" otherwise don't change it. Keeping the current otherwise
grouped_sum$Treatment <- ifelse(grouped_sum$Queen, "Queen", grouped_sum$Treatment)


library(wesanderson)
library(forcats)

grouped_sum$source_colony <- factor(grouped_sum$source_colony, levels = c("RooibosTea", "MexHotChoc", "20230213", "20221209", "20221123"))

ggplot(grouped_sum, aes(x = fct_rev(Treatment), y = Degree_var)) + 
  geom_line(aes(group = source_colony),color="darkgray") +
  geom_point(aes(color = source_colony), size = 5) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color="Source Colony") +
  ylab("Degree Variance") + # Adjust axis labels
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
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

ggsave("../figures/ovary_var.jpg", width = 6.25, height = 6.25, dpi = 600)


ggplot(grouped_sum, aes(x = fct_rev(Treatment), y = Degree)) + 
  geom_line(aes(group = source_colony),color="darkgray") +
  geom_point(aes(color = source_colony), size = 5) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color="Source Colony") +
  ylab("Degree Variance") + # Adjust axis labels
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
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))


grouped_sum_no_queens <- grouped_sum[!grouped_sum$Queen, ]
ggplot(grouped_sum_no_queens, aes(x = fct_rev(Treatment), y = AverageWidth_var)) + 
  geom_line(aes(group = source_colony),color="darkgray") +
  geom_point(aes(color = source_colony), size = 5) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color="Source Colony") +
  ylab("Oovary Index Variance") + # Adjust axis labels
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
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))

ggsave("../figures/ovary_index_var.jpg", width = 6.25, height = 6.25, dpi = 600)
