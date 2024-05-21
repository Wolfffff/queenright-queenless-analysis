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
TotalCentMean$source_colony <- sapply(strsplit(TotalCentMean$Col, "_"), `[`, 1)

# if is QR make treatment "Queen" otherwise don't change it. Keeping the current otherwise
TotalCentMean$Treatment <- ifelse(TotalCentMean$Queen, "Queen", TotalCentMean$Treatment)
TotalCentMean$source_colony <- factor(TotalCentMean$source_colony, levels = c("RooibosTea", "MexHotChoc", "20230213", "20221209", "20221123"))

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

grouped_sum <- grouped_sum %>%
  mutate(linetype = case_when(
    Treatment %in% c("Queenleft Worker", "Queenright Worker") ~ "solid",
    TRUE ~ "dashed"
  ))


ggplot(grouped_sum, aes(x = fct_rev(Treatment), y = Degree)) +
  geom_beeswarm(data= TotalCentMean,aes(x = fct_rev(Treatment), y = Degree, color = source_colony),stroke=0,size=1.7,,alpha=.3,method = "center") +
  geom_line(aes(group = source_colony), linetype = "dashed",color="#535353") +
  geom_line(data = grouped_sum[grouped_sum$Treatment %in% c("Queenless Worker", "Queenright Worker"),],aes(group = source_colony), linetype = "solid",color="#535353") +
  geom_point(aes(color = source_colony), size = 5) +
  geom_point(aes(group = source_colony), size = 5.5,stroke=.5,color="black",shape=21) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Mean Number of Interactions per Hour") + # Adjust axis labels
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


#### stats ####

library(lmerTest)
df_for_stats <- TotalCentMean
# Only workers
df_for_stats <- df_for_stats[df_for_stats$Queen == 0,]
df_for_stats$source_colony <- sapply(strsplit(df_for_stats$Col, "_"), `[`, 1)
df_for_stats$source_colony <- factor(df_for_stats$source_colony, levels = c("RooibosTea", "MexHotChoc", "20230213", "20221209", "20221123"))

fm.null <- lmer(Degree ~ 1 + (1 | source_colony), data = df_for_stats)
fm <- lmer(Degree ~ 1 + fct_rev(Treatment) + (1 | source_colony), data = df_for_stats)

anova(fm, fm.null)
summary(fm)



#### end ####