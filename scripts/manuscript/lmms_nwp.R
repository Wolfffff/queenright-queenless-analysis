library(lme4)
library(tidyverse)
library(dplyr)
library(stringr)
library(wesanderson)
library(ggnewscale)
library(ggplot2)
library(broom.mixed)
library(lmerTest)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")


TotalNWP_Wider <- nwp %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = params, values_from = values, values_fill = NA)

names(TotalNWP_Wider)[names(TotalNWP_Wider) == "Average Clustering"] <- "Average.Clustering"
TotalNWP_Merge <- TotalNWP_Wider %>%
  group_by(ID) %>%
  summarise_all(na.omit)

TotalNWP_One <- TotalNWP_Merge[TotalNWP_Merge$X == 1, ]

TotalNWP_Pooled <- TotalNWP_One %>%
  filter(TimeOfDay == "Day") %>%
  mutate(Day_Zeit = paste(Day, Zeit, sep = "_"))


# List of features to test
features <- c("Modularity", "GlobalEfficiency", "Transitivity", "Average.Clustering", "Assortativity", "Sum")

# Function to fit and summarize the model for each feature
fit_and_summarize <- function(feature) {
  formula <- as.formula(paste(feature, "~ 1 + QR + (1 | Trial) + (1 | QR:Trial) + (1 | Day_Zeit) + (1 | Day_Zeit:Trial)"))
  model <- lmer(formula, data = TotalNWP_Pooled)
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
comprehensive_summary_table <- tidy_results %>%
  select(feature, model_spec, term, estimate, std.error, statistic, p.value) %>%
  arrange(feature, term)

# Display comprehensive summary table
print(comprehensive_summary_table)

# Get significant features
significant_features <- comprehensive_summary_table %>%
  filter(p.value < 0.05) %>%
  select(feature, term, p.value)
print(significant_features)
