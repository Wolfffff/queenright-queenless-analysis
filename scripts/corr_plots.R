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

# Corr matrix incl Betweenness, Degree, and Closeness
correlation_matrix <- cor(TotalCentMean[, c("Degree", "Closeness", "Betweenness")])
jpeg("../figures/correlation_matrix_centrality.jpg", width = 8.5, height = 8.5, units = "in", res = 600)
corrplot(correlation_matrix, method = "circle", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
dev.off()
