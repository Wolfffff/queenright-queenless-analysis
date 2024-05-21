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


# focal_df <- TotalCentMean[TotalCentMean$QR == 0,]
# focal_df <- TotalCentMean[TotalCentMean$QR == 1,]
focal_df <- TotalCentMean
library(corrplot)
# Corr matrix incl Betweenness, Degree, and Closeness
correlation_matrix <- cor(focal_df[, c("Degree", "Closeness", "Betweenness")])
jpeg("../figures/correlation_matrix_indiv_all.jpg", width = 8.25, height = 8.25, units = "in", res = 300)
corrplot(correlation_matrix, type = "upper", tl.col = "black", tl.srt = 45,diag = FALSE,mar=c(0,0,0,0))
dev.off()


