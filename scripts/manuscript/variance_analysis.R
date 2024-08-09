library(lme4)
library(tidyverse)
library(stringr)
library(ggplot2)
library(broom.mixed)
library(wesanderson)
library(lmerTest)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")
bds_means <- bds_means %>%
  mutate(QR_Queen_Condition = case_when(
    QR_Queen_Condition == "Queenless" ~ "Queenless\nWorker",
    QR_Queen_Condition == "Queenright" ~ "Queenright\nWorker",
    TRUE ~ QR_Queen_Condition
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright\nWorker", "Queenless\nWorker")))

# Remove queen
bds_means <- bds_means %>%
  filter(QR_Queen_Condition != "Queen")

bds_means$condition <- as.integer(factor(bds_means$QR)) - 1
bds_means$group <- as.integer(factor(bds_means$Trial))
bds_means$value <- bds_means$Degree

n_groups <- length(unique(bds_means$group))
n_conditions <- length(unique(bds_means$condition))

data <- bds_means
getVarDiff <- function(x) {
  return(var(x[x$condition == 1, ]$value) - var(x[x$condition == 0, ]$value))
}
var_diffs <- rep(NA, n_groups)
for (i in 1:n_groups) {
  var_diffs[i] <- (getVarDiff(data[data$group == i, ]))
}
mean_var_diff <- mean(var_diffs)

# Generate Null distribution of paired variance difference mean
perm_data <- data
n_perm <- 100000
perm_mean_var_diffs <- rep(NA, n_perm)
for (i in 1:n_perm) {
  p_var_diffs <- rep(NA, n_groups)
  for (j in 1:n_groups) {
    perm_data$condition[data$group == j] <- sample(perm_data$condition[data$group == j])
    p_var_diffs[j] <- (getVarDiff(perm_data[perm_data$group == j, ]))
    perm_mean_var_diffs[i] <- mean(p_var_diffs)
  }
}

data.frame(perm_mean_var_diffs) %>%
  ggplot(aes(x = perm_mean_var_diffs)) +
  geom_histogram(bins = 20, fill = "darkgrey") +
  geom_vline(xintercept = mean_var_diff, color = "red", size = 1) +
  CONSISTENT_THEME +
  labs(
    title = "Permutation Distribution of Paired Mean Variance Difference (Queenless Workers - Queenright Workers)",
    x = "Mean Difference of Variances"
  )
pval <- (p_value <- sum(perm_mean_var_diffs < mean_var_diff) / n_perm)
pval

ggsave("figures/manuscript/si/variance_analysis.png", width = 6, height = 6, units = "in", dpi = 300)
