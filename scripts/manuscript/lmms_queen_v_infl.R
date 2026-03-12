# Load necessary libraries
library(glmmTMB)
library(tidyverse)
library(stringr)
library(ggplot2)
library(broom.mixed)
library(emmeans)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

bds$OvaryIndex <- bds$AverageOvaryWidth / bds$AverageWingLength

# Filter for queens and hub bees only, during day
bds_queen_infl <- bds %>%
  filter(TimeOfDay == "Day") %>%
  filter(Queen == TRUE | (QR == 0 & Infl == 1)) %>%
  mutate(Type = ifelse(Queen == TRUE, "Queen", "Hub Bee"),
         Type = factor(Type, levels = c("Queen", "Hub Bee")),
         Day_Zeit = paste(Day, Zeit, sep = "_"))

# List of features to test
features <- c("Degree", "move_perc", "mean_vel", "N90.Day4", "Initiation.Freq", "Close", "Between", "clust", "OvaryIndex")

# Function to fit and summarize the model for each feature
fit_and_summarize <- function(feature) {
  formula <- as.formula(paste(feature, "~ 1 + Type + (1 | Trial) + (1 | Day_Zeit)"))
  model <- glmmTMB(formula, data = bds_queen_infl, family = gaussian(), dispformula = ~ Type)
  tidy_model <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(feature = feature,
           model_spec = paste(deparse(formula), collapse = " "))

  # Compare dispersion estimates between Type groups using emmeans
  disp_emm <- emmeans(model, ~ Type, component = "disp")
  disp_contrast <- pairs(disp_emm)
  cat("\n--- Dispersion comparison for", feature, "---\n")
  print(summary(disp_emm))
  print(summary(disp_contrast))

  return(tidy_model)
}

# Fit and summarize models for each feature
tidy_results_list <- lapply(features, fit_and_summarize)
tidy_results <- bind_rows(tidy_results_list)

# Create comprehensive summary table
comprehensive_summary_table <- tidy_results %>%
  select(feature, model_spec, term, estimate, std.error, statistic, p.value) %>%
  arrange(feature, term)

options(width = 200)
# Print results
print(comprehensive_summary_table)

# Get significant comparisons
significant_features <- comprehensive_summary_table %>%
  filter(p.value < 0.05) %>%
  select(feature, term, p.value)

print(significant_features)

# Save results
write_csv(comprehensive_summary_table, "results/queen_vs_hub_bee_lmm.csv")
