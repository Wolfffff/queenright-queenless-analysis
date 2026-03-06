# Ovary-Interactivity Interaction Model
# Tests whether the slope of interactivity (Degree) vs ovary index
# differs between queenright and queenless workers using an LMM
# with an interaction term.

library(glmmTMB)
library(emmeans)
library(tidyverse)
library(broom.mixed)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

# Filter to workers only (remove queens)
workers <- bds_means %>%
  filter(QR_Queen_Condition != "Queen") %>%
  droplevels()

cat("=== Data Summary ===\n")
cat("N workers:", nrow(workers), "\n")
cat("By condition:\n")
print(table(workers$QR_Queen_Condition))
cat("By Trial:\n")
print(table(workers$Trial))
cat("\n")

# Fit glmmTMB model with interaction term
# Degree ~ QR_Queen_Condition * ovary_idx + (1 | Trial)
model <- glmmTMB(
  Degree ~ QR_Queen_Condition * ovary_idx + (1 | Trial),
  data = workers,
  family = gaussian()
)

cat("=== Model Summary ===\n")
print(summary(model))
cat("\n")

# Tidy model output
tidy_model <- tidy(model, effects = "fixed", conf.int = TRUE)
cat("=== Fixed Effects ===\n")
print(tidy_model)
cat("\n")

# Extract and report the interaction term
interaction_row <- tidy_model %>%
  filter(str_detect(term, ":"))

cat("=== Interaction Term (test of slope difference) ===\n")
cat("Term:", interaction_row$term, "\n")
cat("Estimate:", round(interaction_row$estimate, 4), "\n")
cat("Std. Error:", round(interaction_row$std.error, 4), "\n")
cat("z-value:", round(interaction_row$statistic, 4), "\n")
cat("p-value:", format.pval(interaction_row$p.value, digits = 4), "\n")
cat("\n")

# Estimate separate slopes for each condition using emtrends
slopes <- emtrends(model, ~ QR_Queen_Condition, var = "ovary_idx")
cat("=== Estimated Slopes (emtrends) ===\n")
print(slopes)
cat("\n")

# Pairwise comparison of slopes
slope_contrast <- pairs(slopes)
cat("=== Pairwise Slope Comparison ===\n")
print(slope_contrast)
cat("\n")

# Save results to CSV
results <- tidy_model %>%
  mutate(model_specification = "Degree ~ QR_Queen_Condition * ovary_idx + (1 | Trial)")

write_csv(results, "results/ovary_interactivity_interaction_model.csv")
cat("Results saved to results/ovary_interactivity_interaction_model.csv\n")
