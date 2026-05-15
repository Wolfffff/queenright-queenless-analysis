# Load necessary libraries
library(glmmTMB)
library(tidyverse)
library(stringr)
library(ggplot2)
library(broom.mixed)
library(emmeans)
# Note: car is referenced via car::Anova() rather than attached, because
# car::recode masks dplyr::recode and that breaks load_data.R.

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

# Keep only queenless workers (non-hub + hub)
bds_ql <- bds %>%
  filter(QR == FALSE, Queen == FALSE)

bds_ql$OvaryIndex <- bds_ql$AverageOvaryWidth / bds_ql$AverageWingLength
bds_ql <- bds_ql %>%
  mutate(worker_v_infl = case_when(
    Infl == 0 ~ "Queenless Worker",
    Infl == 1 ~ "Hub Bee"
  )) %>%
  mutate(worker_v_infl = factor(worker_v_infl, levels = c("Queenless Worker", "Hub Bee")))

bds_ql <- bds_ql %>%
  filter(TimeOfDay == "Day") %>%
  mutate(Day_Zeit = paste(Day, Zeit, sep = "_"))

# Get number of hub bees
num_hub_bees <- bds_ql %>%
  filter(Infl == 1) %>%
  summarise(num_hub_bees = n_distinct(Bee))

# List of features to test
features <- c("Degree", "move_perc", "mean_vel", "N90.Day4", "Initiation.Freq", "Close", "Between", "clust", "OvaryIndex")

# Single glmmTMB model per feature: fixed-effect of worker_v_infl for the mean
# comparison and dispformula=~worker_v_infl for treatment-specific variance. The
# Wald chi-squared test on the worker_v_infl fixed effect provides the p-value
# for the mean comparison; emmeans pairs() on the dispersion component provides
# the p-value for the variance comparison. Both come from the same model so the
# standard errors on the means correctly absorb treatment-specific residual
# variance (Reviewer 2, R2 Comment 1).
fit_unified <- function(feature) {
  formula <- as.formula(paste(feature, "~ 1 + worker_v_infl + (1 | Trial) + (1 | Day_Zeit)"))
  model <- glmmTMB(
    formula,
    data = bds_ql,
    family = gaussian(),
    dispformula = ~ worker_v_infl
  )

  cat("\n=================================================\n")
  cat("Feature:", feature, "\n")
  cat("=================================================\n")
  print(summary(model))

  # Mean comparison: Wald chi-squared on the worker_v_infl fixed effect
  wald <- car::Anova(model, type = "II", component = "cond")
  stopifnot("worker_v_infl" %in% rownames(wald))
  cat("\n--- Mean comparison (Wald chi-squared on worker_v_infl) ---\n")
  print(wald)

  # Variance comparison: emmeans contrast on the dispersion component
  disp_emm <- emmeans(model, ~ worker_v_infl, component = "disp")
  disp_pairs <- pairs(disp_emm)
  cat("\n--- Variance comparison (emmeans on dispersion) ---\n")
  print(summary(disp_emm))
  print(summary(disp_pairs))

  # Tidy fixed-effect output and attach Wald + dispersion contrast results
  tidy_fixed <- tidy(model, effects = "fixed", conf.int = TRUE, component = "cond") %>%
    mutate(
      feature = feature,
      model_spec = paste(deparse(formula), collapse = " "),
      wald_chi2 = wald["worker_v_infl", "Chisq"],
      wald_df = wald["worker_v_infl", "Df"],
      wald_p = wald["worker_v_infl", "Pr(>Chisq)"],
      disp_contrast_estimate = summary(disp_pairs)$estimate[1],
      disp_contrast_SE = summary(disp_pairs)$SE[1],
      # emmeans returns z.ratio for asymptotic tests, t.ratio when df is finite.
      disp_contrast_stat = if (!is.null(summary(disp_pairs)$z.ratio))
                              summary(disp_pairs)$z.ratio[1]
                           else summary(disp_pairs)$t.ratio[1],
      disp_contrast_p = summary(disp_pairs)$p.value[1]
    )
  return(tidy_fixed)
}

# Ensure no truncation in the output display
options(width = 200)

tidy_results <- bind_rows(lapply(features, fit_unified))

# Display tidy results
print(tidy_results)

# Create a comprehensive summary table
comprehensive_summary_table_inf <- tidy_results %>%
  select(feature, model_spec, term, estimate, std.error, statistic, p.value,
         wald_chi2, wald_df, wald_p,
         disp_contrast_estimate, disp_contrast_SE, disp_contrast_stat, disp_contrast_p) %>%
  arrange(feature, term)

# Display comprehensive summary table
print(comprehensive_summary_table_inf)

# Get significant features
significant_features <- comprehensive_summary_table_inf %>%
  filter(wald_p < 0.05) %>%
  select(feature, term, wald_chi2, wald_p, disp_contrast_p)

print(significant_features)

write_csv(comprehensive_summary_table_inf, "results/comprehensive_summary_table_inf.csv")
