# Load necessary libraries
library(glmmTMB)
library(tidyverse)
library(stringr)
library(ggplot2)
library(broom.mixed)
library(emmeans)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

# Remove queens
bds_ql <- bds %>%
  filter(QR == FALSE)

bds_ql$OvaryIndex <- bds_ql$AverageOvaryWidth / bds_ql$AverageWingLength
bds_ql <- bds_ql %>%
  mutate(worker_v_infl = case_when(
    QR == 0 & Infl == 0 ~ "Queenless Worker",
    QR == 1 & Queen == 0 ~ "Queenright Worker",
    Queen == 1 ~ "Queen",
    QR == 0 & Infl == 1 ~ "Hub Bee"
  )) %>%
  mutate(worker_v_infl = factor(worker_v_infl, levels = c("Queenless Worker", "Queenright Worker", "Queen", "Hub Bee")))

bds_ql <- bds_ql %>%
  filter(TimeOfDay == "Day") %>%
  mutate(Day_Zeit = paste(Day, Zeit, sep = "_"))

# Get number of hub bees
num_hub_bees <- bds_ql %>%
  filter(Infl == 1) %>%
  summarise(num_hub_bees = n_distinct(Bee))

# List of features to test
features <- c("Degree", "move_perc", "mean_vel", "N90.Day4", "Initiation.Freq", "Close", "Between", "clust", "OvaryIndex")

# Function to fit and summarize the model for each feature
fit_and_summarize <- function(feature) {
  formula <- as.formula(paste(feature, "~ 1 + worker_v_infl + (1 | Trial) + (1 | Day_Zeit)"))
  model <- glmmTMB(formula, data = bds_ql, family = gaussian(), dispformula = ~ worker_v_infl)
  tidy_model <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(feature = feature, model_spec = paste(deparse(formula), collapse = " "))

  # Compare dispersion estimates between worker_v_infl groups using emmeans
  disp_emm <- emmeans(model, ~ worker_v_infl, component = "disp")
  disp_contrast <- pairs(disp_emm)
  cat("\n--- Dispersion comparison for", feature, "---\n")
  print(summary(disp_emm))
  print(summary(disp_contrast))

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
  select(feature, term, p.value)

print(significant_features)

write_csv(comprehensive_summary_table_inf, "results/comprehensive_summary_table_inf.csv")
