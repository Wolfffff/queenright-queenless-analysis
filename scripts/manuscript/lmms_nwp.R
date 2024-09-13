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


nwp$ID_Time <- paste(nwp$ID, nwp$Hour)

nwpModularity <- nwp[nwp$params == "Modularity", ]
nwpEfficiency <- nwp[nwp$params == "GlobalEfficiency", ]
nwpTransitivity <- nwp[nwp$params == "Transitivity", ]
nwpClustering <- nwp[nwp$params == "Average Clustering", ]
nwpAssortativity <- nwp[nwp$params == "Assortativity", ]
nwp_filtered <- nwp[nwp$params != "KeystoneAssortativity", ]
nwp_filtered <- nwp_filtered[nwp_filtered$params != "KeystoneAssPercentile", ]

nwp_wide <- nwp_filtered %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = params, values_from = values, values_fill = NA)
names(nwp_wide)[names(nwp_wide) == "Average Clustering"] <- "Average.Clustering"



nwp_merge <- nwp_wide %>%
  group_by(ID_Time) %>%
  summarise_all(na.omit)


nwp_Pooled <- nwp_merge %>%
  filter(TimeOfDay == "Day") %>%
  mutate(Day_Zeit = paste(Day, Zeit, sep = "_"))


# List of features to test
features <- c("Modularity", "GlobalEfficiency", "Transitivity", "Average.Clustering", "Assortativity", "Sum")

# Function to fit and summarize the model for each feature
fit_and_summarize <- function(feature) {
  formula <- as.formula(paste(feature, "~ 1 + QR + (1 | Trial) + (1 | QR:Trial) + (1 | Day_Zeit) + (1 | Day_Zeit:Trial)"))
  model <- lmer(formula, data = nwp_Pooled)
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
