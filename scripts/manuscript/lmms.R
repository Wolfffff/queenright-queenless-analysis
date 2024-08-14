# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(ggplot2)
library(broom.mixed)
library(lmerTest)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

# Remove queens
bds_no_queens <- bds %>%
  filter(Queen == FALSE)

bds_no_queens$OvaryIndex = bds_no_queens$AverageOvaryWidth/bds_no_queens$AverageWingLength
bds_no_queens <- bds_no_queens %>%
  mutate(QR_Queen_Inf = case_when(
    QR == 0 & Infl == 0 ~ "Queenless Worker",
    QR == 1 & Queen == 0 ~ "Queenright Worker",
    Queen == 1 ~ "Queen",
    QR == 0 & Infl == 1 ~ "Influencer",
    TRUE ~ NA_character_ # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Inf = factor(QR_Queen_Inf, levels = c("Queenless Worker", "Queenright Worker", "Queen", "Influencer")))

bds_pooled <- bds_no_queens %>%
  filter(TimeOfDay == "Day") %>%
  mutate(Day_Zeit = paste(Day, Zeit, sep = "_"))

# List of features to test
features <- c("Degree", "move_perc", "mean_vel", "N90.Day4", "Initiation.Freq", "Close", "Between","clust", "OvaryIndex")

# Function to fit and summarize the model for each feature
fit_and_summarize <- function(feature) {
  formula <- as.formula(paste(feature, "~ 1 + QR_Queen_Inf + (1 | Trial) + (1 | QR_Queen_Inf:Trial) + (1 | Day_Zeit) + (1 | Day_Zeit:Trial)"))
  model <- lmer(formula, data = bds_pooled)
  tidy_model <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(feature = feature, model_spec = paste(deparse(formula), collapse = " "))
  return(tidy_model)
}

# Fit and summarize models for each feature
tidy_results_list <- lapply(features, fit_and_summarize)

# Combine tidy results into a single data frame
tidy_results <- bind_rows(tidy_results_list)

# Ensure no truncation in the output display
options(width = 200)

# Display tidy results
print(tidy_results)

# Create a comprehensive summary table
comprehensive_summary_table_inf <- tidy_results %>%
  select(feature, model_spec, term, estimate, std.error, statistic, p.value) %>%
  arrange(feature, term)

# Display comprehensive summary table
print(comprehensive_summary_table_inf)

# Get significant features
significant_features <- comprehensive_summary_table_inf %>%
  filter(p.value < 0.05) %>%
  select(feature,term, p.value)

print(significant_features)