# Load necessary libraries
library(lme4)
library(lmerTest)
library(glmmTMB)
library(tidyverse)
library(stringr)
library(ggplot2)
library(broom.mixed)
library(emmeans)

source("scripts/manuscript/load_data.R")
source("scripts/manuscript/constants.R")

# Filter out queenless data
queenright_data <- bds %>%
  filter(QR == TRUE)

# Calculate Ovary Index
queenright_data$OvaryIndex <- queenright_data$AverageOvaryWidth / queenright_data$AverageWingLength

# Filter and prepare data for modeling
daytime_filtered_data <- queenright_data %>%
  filter(TimeOfDay == "Day") %>%
  mutate(DayTimePeriod = paste(Day, Zeit, sep = "_"))

# Separate list of metrics to test for the Queen model
metrics_for_queen_model <- c("Degree", "move_perc", "mean_vel", "N90.Day4", "Initiation.Freq", "Close", "Between", "clust", "OvaryIndex")

# Function to fit lme4 model for fixed effects (Wald chi-squared)
fit_lme4_queen <- function(metric) {
  formula <- as.formula(paste(metric, "~ 1 + Queen + (1 | Trial) + (1 | DayTimePeriod)"))
  model <- lmer(formula, data = daytime_filtered_data)
  tidy_model <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(metric = metric, model_specification = paste(deparse(formula), collapse = " "))
  return(tidy_model)
}

# Function to fit glmmTMB model for dispersion comparison
fit_dispersion_queen <- function(metric) {
  formula <- as.formula(paste(metric, "~ 1 + Queen + (1 | Trial) + (1 | DayTimePeriod)"))
  model <- glmmTMB(formula, data = daytime_filtered_data, family = gaussian(), dispformula = ~ Queen)

  disp_emm <- emmeans(model, ~ Queen, component = "disp")
  disp_contrast <- pairs(disp_emm)
  cat("\n--- Dispersion comparison for", metric, "---\n")
  print(summary(disp_emm))
  print(summary(disp_contrast))
}

# Fit lme4 models for fixed effects
queen_model_summary_list <- lapply(metrics_for_queen_model, fit_lme4_queen)
combined_queen_model_summary <- bind_rows(queen_model_summary_list)

# Fit glmmTMB models for dispersion comparisons
lapply(metrics_for_queen_model, fit_dispersion_queen)

# Display Queen model summaries
print(combined_queen_model_summary)

# Create a comprehensive summary table for the Queen model
queen_comprehensive_summary_table <- combined_queen_model_summary %>%
  select(metric, model_specification, term, estimate, std.error, statistic, p.value) %>%
  arrange(metric, term)

# Display comprehensive Queen model summary table
print(queen_comprehensive_summary_table)

# Identify significant metrics for Queen model
significant_metrics_queen <- queen_comprehensive_summary_table %>%
  filter(p.value < 0.05) %>%
  select(metric, term, p.value)

print(significant_metrics_queen)

# Save Queen model summary to CSV
write_csv(queen_comprehensive_summary_table, "results/queen_model_summary_table.csv")
