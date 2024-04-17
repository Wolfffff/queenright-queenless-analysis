# Load Degree Data — Head And Body ----------------------------------------
list_of_packages <- c("lme4", "ggplot2", "sp", "tidyverse", "car", "raster", "pak", "wesanderson", "forcats")
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

TotalCentMean$Col <- sub("_[^_]+$", "", TotalCentMean$ID)

# Swap levels of QR to match the plot
TotalCentMean$Treatment <- ifelse(TotalCentMean$QR == 0, "Queenless Worker", "Queenright Worker")

# group by combo of Treatment, Colony, and Queen
grouped_sum <- TotalCentMean %>%
  group_by(Treatment, Col, Queen) %>%
  summarise(Degree = mean(Degree))

# get value before first _ in source_colony
grouped_sum$source_colony <- sapply(strsplit(grouped_sum$Col, "_"), `[`, 1)

# if is QR make treatment "Queen" otherwise don't change it. Keeping the current otherwise
grouped_sum$Treatment <- ifelse(grouped_sum$Queen, "Queen", grouped_sum$Treatment)

grouped_sum$source_colony <- factor(grouped_sum$source_colony, levels = c("RooibosTea", "MexHotChoc", "20230213", "20221209", "20221123"))

ggplot(grouped_sum, aes(x = fct_rev(Treatment), y = Degree)) +
  geom_line(aes(group = source_colony), color = "darkgray") +
  geom_point(aes(color = source_colony), size = 5) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Mean Number of Interactions Per Hour") + # Adjust axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    text = element_text(size = 16),
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

ggsave("../figures/interactions_by_grouping.jpg", width = 6.25, height = 6.25, dpi = 600)
