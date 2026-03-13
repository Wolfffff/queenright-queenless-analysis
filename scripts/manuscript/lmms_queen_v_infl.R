# Load necessary libraries
library(lme4)
library(lmerTest)
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

# Function to fit lme4 model for fixed effects (Wald chi-squared)
fit_lme4 <- function(feature) {
  formula <- as.formula(paste(feature, "~ 1 + Type + (1 | Trial) + (1 | Day_Zeit)"))
  model <- lmer(formula, data = bds_queen_infl)
  tidy_model <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(feature = feature,
           model_spec = paste(deparse(formula), collapse = " "))
  return(tidy_model)
}

# Function to fit glmmTMB model for dispersion comparison
fit_dispersion <- function(feature) {
  formula <- as.formula(paste(feature, "~ 1 + Type + (1 | Trial) + (1 | Day_Zeit)"))
  model <- glmmTMB(formula, data = bds_queen_infl, family = gaussian(), dispformula = ~ Type)

  disp_emm <- emmeans(model, ~ Type, component = "disp")
  disp_contrast <- pairs(disp_emm)
  cat("\n--- Dispersion comparison for", feature, "---\n")
  print(summary(disp_emm))
  print(summary(disp_contrast))
}

# Fit lme4 models for fixed effects
tidy_results_list <- lapply(features, fit_lme4)
tidy_results <- bind_rows(tidy_results_list)

# Fit glmmTMB models for dispersion comparisons
lapply(features, fit_dispersion)

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
