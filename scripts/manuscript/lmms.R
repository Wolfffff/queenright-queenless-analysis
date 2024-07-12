# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(ggplot2)
library(broom.mixed)

# Read the data
bds <- read_csv("data/BigDataSheet.csv")
bds <- bds %>%
  mutate(Trial = str_extract(Bee, ".+?(?=_)"))

bds <- bds %>%
  mutate(QR_Queen_Inf = case_when(
    QR == 0 & Infl == 0 ~ "Queenless Worker",
    QR == 1 & Queen == 0 ~ "Queenright Worker",
    Queen == 1 ~ "Queen",
    QR == 0 & Infl == 1 ~ "Influencer",
    TRUE ~ NA_character_ # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Inf = factor(QR_Queen_Inf, levels = c("Queenless Worker", "Queenright Worker", "Queen", "Influencer")))


# Get average by individual, trial, time of day

# Pool time by Day and DayNight pair and get mean per
bds_pooled <- bds %>%
  mutate(Day_DayNight = paste(Day, TimeOfDay, sep = "_")) %>%
  group_by(Bee, Trial, QR, Day_DayNight) %>%
  summarise(across(c(Infl, Degree, Close, Eigen, Between, Queen, boutDegree, boutBetween, boutClose, boutEigen, bodyDegree, bodyBetween, bodyClose, bodyEigen, AverageBoutLength, Presence, AntPresence, mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq, clust), mean, na.rm = TRUE))


bds_pooled %>%
  select(Bee, Trial, QR, Day_DayNight, Degree) %>%
  head(10)

# List of features to test
features <- c("Degree", "move_perc", "mean_vel", "N90.Day4", "Initiation.Freq", "Close", "Between")

# Function to fit and summarize the model for each feature
fit_and_summarize <- function(feature) {
  formula <- as.formula(paste(feature, "~ 1 + QR + (1 | Trial) + (1 | QR:Trial) + (1 | Day_DayNight) + (1 | Day_DayNight:Trial)"))
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
comprehensive_summary_table <- tidy_results %>%
  select(feature, model_spec, term, estimate, std.error, statistic, p.value) %>%
  arrange(feature, term)

# Display comprehensive summary table
print(comprehensive_summary_table)

# Get significant features
significant_features <- comprehensive_summary_table %>%
  filter(p.value < 0.05) %>%
  select(feature,term, p.value)

write_csv(comprehensive_summary_table, "data/comprehensive_summary_table.csv")

significant_features
