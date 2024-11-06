library(lme4)
library(tidyverse)
library(stringr)
library(ggplot2)
library(broom.mixed)
library(wesanderson)
library(lmerTest)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

# Data preparation
bds_means <- bds_means %>%
  mutate(QR_Queen_Condition = case_when(
    QR_Queen_Condition == "Queenless" ~ "Queenless\nWorker",
    QR_Queen_Condition == "Queenright" ~ "Queenright\nWorker",
    TRUE ~ QR_Queen_Condition
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright\nWorker", "Queenless\nWorker"))) %>%
  filter(QR_Queen_Condition != "Queen")

# Function to perform variance analysis
perform_variance_analysis <- function(value_column) {
  bds_means <- bds_means %>%
    mutate(
      condition = as.integer(factor(QR)) - 1,
      group = as.integer(factor(Trial)),
      value = !!sym(value_column)
    )
  
  n_groups <- length(unique(bds_means$group))
  n_conditions <- length(unique(bds_means$condition))
  
  getVarDiff <- function(x) {
    var(x[x$condition == 1, ]$value) - var(x[x$condition == 0, ]$value)
  }
  
  var_diffs <- sapply(1:n_groups, function(i) getVarDiff(bds_means[bds_means$group == i, ]))
  mean_var_diff <- mean(var_diffs)
  
  n_perm <- 10000
  perm_mean_var_diffs <- replicate(n_perm, {
    p_var_diffs <- sapply(1:n_groups, function(j) {
      perm_data <- bds_means
      perm_data$condition[perm_data$group == j] <- sample(perm_data$condition[perm_data$group == j])
      getVarDiff(perm_data[perm_data$group == j, ])
    })
    mean(p_var_diffs)
  })
  
  data.frame(perm_mean_var_diffs) %>%
    ggplot(aes(x = perm_mean_var_diffs)) +
    geom_histogram(bins = 20, fill = "darkgrey") +
    geom_vline(xintercept = mean_var_diff, color = "red", size = 1) +
    CONSISTENT_THEME +
    labs(
      title = paste("Permutation Distribution of Paired Mean Variance Difference (Queenless Workers - Queenright Workers) for", value_column),
      x = "Mean Difference of Variances"
    )
  
  pval <- sum(perm_mean_var_diffs < mean_var_diff) / n_perm
  print(pval)
  
  ggsave(paste0("figures/manuscript/si/variance_analysis_", value_column, ".png"), width = 6, height = 6, units = "in", dpi = 300)
}

# Run analysis for clust
perform_variance_analysis("clust")

# Run analysis for Degree
perform_variance_analysis("Degree")

