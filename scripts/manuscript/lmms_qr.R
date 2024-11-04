# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(ggplot2)
library(broom.mixed)
library(lmerTest)

source("scripts/manuscript/load_data.R")
source("scripts/manuscript/constants.R")

# Filter out queens
workers_data <- bds %>%
  filter(QR == TRUE)

# Calculate Ovary Index
workers_data$OvaryIndex <- workers_data$AverageOvaryWidth / workers_data$AverageWingLength

# Classify individuals by role
workers_data <- workers_data %>%
  mutate(RoleClassification = case_when(
    QR == 0 & Infl == 0 ~ "Queenless Worker",
    QR == 1 & Queen == 0 ~ "Queenright Worker",
    Queen == 1 ~ "Queen",
    QR == 0 & Infl == 1 ~ "Influencer",
    TRUE ~ NA_character_  # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(RoleClassification = factor(RoleClassification, levels = c("Queenless Worker", "Queenright Worker", "Queen", "Influencer")))

# Filter and prepare data for modeling
daytime_data <- workers_data %>%
  filter(TimeOfDay == "Day") %>%
  mutate(DayTimePeriod = paste(Day, Zeit, sep = "_"))

# List of behavioral and physiological metrics to test
metrics_to_test <- c("Degree", "move_perc", "mean_vel", "N90.Day4", "Initiation.Freq", "Close", "Between", "clust", "OvaryIndex")

# Function to fit and summarize the model for each metric
fit_and_summarize_model <- function(metric) {
  formula <- as.formula(paste(metric, "~ 1 + RoleClassification + (1 | Trial) + (1 | RoleClassification:Trial) + (1 | DayTimePeriod) + (1 | DayTimePeriod:Trial)"))
  model <- lmer(formula, data = daytime_data)
  tidy_model <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(metric = metric, model_specification = paste(deparse(formula), collapse = " "))
  return(tidy_model)
}

# Fit and summarize models for each metric
model_summary_list <- lapply(metrics_to_test, fit_and_summarize_model)

# Combine model summaries into a single data frame
model_summary <- bind_rows(model_summary_list)

# Ensure no truncation in the output display
options(width = 200)

# Display model summaries
print(model_summary)

# Create a comprehensive summary table
summary_table <- model_summary %>%
  select(metric, model_specification, term, estimate, std.error, statistic, p.value) %>%
  arrange(metric, term)

# Display comprehensive summary table
print(summary_table)

# Identify significant metrics
significant_metrics <- summary_table %>%
  filter(p.value < 0.05) %>%
  select(metric, term, p.value)

print(significant_metrics)

# Separate list of metrics to test for the Queen model
metrics_for_queen_model <- c("Degree", "move_perc", "mean_vel", "N90.Day4", "Initiation.Freq", "Close", "Between", "clust", "OvaryIndex")

# Function to fit and summarize the Queen model for each metric
fit_and_summarize_queen_model <- function(metric) {
  formula <- as.formula(paste(metric, "~ 1 + Queen + (1 | Trial) + (1 | Queen:Trial) + (1 | DayTimePeriod) + (1 | DayTimePeriod:Trial)"))
  model <- lmer(formula, data = daytime_data)
  tidy_model <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(metric = metric, model_specification = paste(deparse(formula), collapse = " "))
  return(tidy_model)
}

# Fit and summarize Queen models for each metric
queen_model_summary_list <- lapply(metrics_for_queen_model, fit_and_summarize_queen_model)

# Combine Queen model summaries into a single data frame
queen_model_summary <- bind_rows(queen_model_summary_list)

# Display Queen model summaries
print(queen_model_summary)

# Create a comprehensive summary table for the Queen model
queen_summary_table <- queen_model_summary %>%
  select(metric, model_specification, term, estimate, std.error, statistic, p.value) %>%
  arrange(metric, term)

# Display comprehensive Queen model summary table
print(queen_summary_table)

# Identify significant metrics for Queen model
significant_metrics_queen <- queen_summary_table %>%
  filter(p.value < 0.05) %>%
  select(metric, term, p.value)

print(significant_metrics_queen)

# Save Queen model summary to CSV
write_csv(queen_summary_table, "results/queen_model_summary_table.csv")