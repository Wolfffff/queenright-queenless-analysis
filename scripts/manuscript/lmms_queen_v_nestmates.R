# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(ggplot2)
library(broom.mixed)
library(lmerTest)

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

# Function to fit and summarize the Queen model for each metric
fit_and_summarize_queen_model <- function(metric) {

  formula <- as.formula(paste(metric, "~ 1 + Queen + (1 | Trial) + (1 | DayTimePeriod)"))
  model <- lmer(formula, data = daytime_filtered_data)

  tidy_model <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(metric = metric, model_specification = paste(deparse(formula), collapse = " "))
  return(tidy_model)
}

# Fit and summarize Queen models for each metric
queen_model_summary_list <- lapply(metrics_for_queen_model, fit_and_summarize_queen_model)

# Combine Queen model summaries into a single data frame
combined_queen_model_summary <- bind_rows(queen_model_summary_list)

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
