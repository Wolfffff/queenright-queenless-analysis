
# Load Degree Data — Head And Body
list_of_packages <- c("lme4", "ggplot2", "sp", "tidyverse", "car", "raster", "pak", "wesanderson", "forcats")
# pak::pkg_install(list_of_packages)
for (p in list_of_packages) {
  library(p, character.only = TRUE)
}

source("../scripts/utils/base_utils.R")
agg_df <- process_files(suffix = "NWP.csv")

# Get unique ids by group in TotalCent
agg_df %>%
  group_by(Col) %>%
  summarise(Unique = length(unique(ID)))

unique_network_metrics <- unique(agg_df$params)
for (metric in unique_network_metrics) {

  paste("Plotting", metric)
  single_param_df <- agg_df[agg_df$params == metric, ]

  single_param_df_agg <- aggregate(cbind(values, QR) ~ ID, single_param_df, mean)
  single_param_df_agg$Col <- sub("_[^_]+$", "", single_param_df_agg$ID)

  # Swap levels of QR to match the first plot
  single_param_df_agg$Treatment <- ifelse(single_param_df_agg$QR, "Queenright", "Queenless")


  # get value before first _ in source_colony
  single_param_df_agg$source_colony <- sapply(strsplit(single_param_df_agg$Col, "_"), `[`, 1)
  single_param_df_agg$source_colony <- factor(single_param_df_agg$source_colony, levels = c("RooibosTea", "MexHotChoc", "20230213", "20221209", "20221123"))


  ggplot(single_param_df_agg, aes(x = fct_rev(Treatment), y = values)) + 
    geom_line(aes(group = source_colony),color="darkgray") +
    geom_point(aes(color = source_colony), size = 5) +
    scale_color_manual(values = wes_palette("Cavalcanti1")) +
    xlab("") +
    labs(color="Source Colony") +
    ylab(paste0(metric)) +
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

  ggsave(paste0("../figures/network_metric_",metric,"_contrast.jpg"), width = 8.5, height = 4, dpi = 600)
}

# Group by QR, select the columns and pivot
wide_df <- agg_df_qr %>% 
  select(params, values) %>% 
  pivot_wider(names_from = params, values_from = values)
# Unlist each column and convert back to a dataframe
wide_df <- lapply(wide_df, unlist)
wide_df <- as.data.frame(wide_df)

# Ensure all columns are numeric
wide_df <- mutate_if(wide_df, is.character, as.numeric)

# Remove rows with NA values
wide_df <- na.omit(wide_df)

# Calculate the correlation matrix
corr_df <- cor(wide_df)

library(corrplot)
jpeg("../figures/correlation_matrix.jpg", width = 8.5, height = 8.5, units = "in", res = 600)
corrplot(corr_df, tl.cex = 0.5, tl.col = "black", tl.srt = 45, mar = c(5, 5, 5, 5))
dev.off()
# save plot

