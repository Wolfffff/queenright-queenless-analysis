library(glmmTMB)
library(tidyverse)
library(dplyr)
library(stringr)
library(wesanderson)
library(ggnewscale)
library(ggplot2)
library(broom.mixed)
library(emmeans)
# Note: car is referenced via car::Anova() rather than attached, because
# car::recode masks dplyr::recode and that breaks load_data.R.

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

# Single glmmTMB model per feature: fixed-effect of QR for the mean
# comparison and dispformula=~QR for treatment-specific variance. The Wald
# chi-squared test on the QR fixed effect provides the p-value for the mean
# comparison; emmeans pairs() on the dispersion component provides the
# p-value for the variance comparison. Both come from the same model so the
# standard errors on the means correctly absorb treatment-specific residual
# variance (Reviewer 2, R2 Comment 1).
fit_unified <- function(feature) {
  formula <- as.formula(paste(feature, "~ 1 + QR + (1 | Trial) + (1 | Day_Zeit)"))
  model <- glmmTMB(
    formula,
    data = nwp_Pooled,
    family = gaussian(),
    dispformula = ~ QR
  )

  cat("\n=================================================\n")
  cat("Feature:", feature, "\n")
  cat("=================================================\n")
  print(summary(model))

  # Mean comparison: Wald chi-squared on the QR fixed effect
  wald <- car::Anova(model, type = "II", component = "cond")
  stopifnot("QR" %in% rownames(wald))
  cat("\n--- Mean comparison (Wald chi-squared on QR) ---\n")
  print(wald)

  # Variance comparison: emmeans contrast on the dispersion component
  disp_emm <- emmeans(model, ~ QR, component = "disp")
  disp_pairs <- pairs(disp_emm)
  cat("\n--- Variance comparison (emmeans on dispersion) ---\n")
  print(summary(disp_emm))
  print(summary(disp_pairs))

  # Tidy fixed-effect output and attach Wald + dispersion contrast results
  tidy_fixed <- tidy(model, effects = "fixed", conf.int = TRUE, component = "cond") %>%
    mutate(
      feature = feature,
      model_spec = paste(deparse(formula), collapse = " "),
      wald_chi2 = wald["QR", "Chisq"],
      wald_df = wald["QR", "Df"],
      wald_p = wald["QR", "Pr(>Chisq)"],
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

options(width = 200)

tidy_results <- bind_rows(lapply(features, fit_unified))

print(tidy_results)

comprehensive_summary_table <- tidy_results %>%
  select(feature, model_spec, term, estimate, std.error, statistic, p.value,
         wald_chi2, wald_df, wald_p,
         disp_contrast_estimate, disp_contrast_SE, disp_contrast_stat, disp_contrast_p) %>%
  arrange(feature, term)

print(comprehensive_summary_table)

significant_features <- comprehensive_summary_table %>%
  filter(wald_p < 0.05) %>%
  select(feature, term, wald_chi2, wald_p, disp_contrast_p)

print(significant_features)

write_csv(comprehensive_summary_table, "results/comprehensive_summary_table_nwp.csv")
