library(glmmTMB)
library(tidyverse)
library(stringr)
library(ggplot2)
library(broom.mixed)
library(wesanderson)
library(emmeans)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

# Data preparation
bds_means <- bds_means %>%
  mutate(QR_Queen_Condition = case_when(
    QR_Queen_Condition == "Queenless" ~ "Queenless\nWorker",
    QR_Queen_Condition == "Queenright" ~ "Queenright\nWorker",
    TRUE ~ QR_Queen_Condition
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright\nWorker", "Queenless\nWorker"))) %>%
  filter(QR_Queen_Condition != "Queen")

# Function to perform dispersion modeling analysis
perform_variance_analysis <- function(value_column) {
  bds_means <- bds_means %>%
    mutate(value = !!sym(value_column))

  # Fit glmmTMB with dispersion model
  model <- glmmTMB(
    value ~ QR_Queen_Condition + (1 | Trial),
    dispformula = ~ QR_Queen_Condition,
    family = gaussian(),
    data = bds_means
  )

  cat("\n===", value_column, "===\n")
  print(summary(model))

  # Extract and compare dispersion estimates via emmeans
  disp_emm <- emmeans(model, ~ QR_Queen_Condition, component = "disp")
  cat("\nDispersion estimates (log scale):\n")
  print(disp_emm)

  disp_contrast <- pairs(disp_emm)
  cat("\nDispersion contrast:\n")
  print(disp_contrast)

  # Plot dispersion estimates
  disp_df <- as.data.frame(disp_emm)
  # Column names vary by emmeans version; use lower.CL/upper.CL or asymp.LCL/asymp.UCL
  if (!"asymp.LCL" %in% names(disp_df)) {
    names(disp_df)[names(disp_df) == "lower.CL"] <- "asymp.LCL"
    names(disp_df)[names(disp_df) == "upper.CL"] <- "asymp.UCL"
  }

  p <- ggplot(disp_df, aes(x = QR_Queen_Condition, y = emmean)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
    CONSISTENT_THEME +
    labs(
      title = paste("Dispersion Estimates by Condition for", value_column),
      x = "Condition",
      y = "Log Dispersion Estimate"
    )

  print(p)

  ggsave(paste0("figures/manuscript/si/variance_analysis_", value_column, ".png"), width = 6, height = 6, units = "in", dpi = 300)
}

# Run analysis for clust
perform_variance_analysis("clust")

# Run analysis for Degree
perform_variance_analysis("Degree")

