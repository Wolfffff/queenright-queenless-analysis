# Load necessary libraries
library(lme4)
library(glmmTMB)
library(tidyverse)
library(stringr)
library(wesanderson)
library(patchwork)
library(factoextra)
library(ggplot2)
library(cowplot)
library(ggbeeswarm)
library(ggrepel)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

# Fit the same glmmTMB interaction model reported in the manuscript
# (see scripts/manuscript/ovary_interactivity_interaction_model.R).
# Workers only (queens dropped), QR_Queen_Condition has 2 levels (QR vs QL).
workers_interaction <- bds_means %>%
  filter(QR_Queen_Condition != "Queen") %>%
  droplevels()

ovary_interaction_model <- glmmTMB(
  Degree ~ QR_Queen_Condition * ovary_idx + (1 | Trial),
  data = workers_interaction,
  family = gaussian()
)

# Build prediction grid: 200 points spanning ovary_idx within each condition.
# Predict at the population level (re.form = NA) so the ribbon reflects the
# fixed-effect mean and its SE, not trial-specific intercepts.
ovary_pred_grid <- workers_interaction %>%
  group_by(QR_Queen_Condition) %>%
  summarise(
    ovary_idx = list(seq(min(ovary_idx, na.rm = TRUE), max(ovary_idx, na.rm = TRUE), length.out = 200)),
    .groups = "drop"
  ) %>%
  unnest(ovary_idx)

ovary_pred_se <- predict(ovary_interaction_model, newdata = ovary_pred_grid, se.fit = TRUE, re.form = NA)
ovary_pred_display <- ovary_pred_grid %>%
  mutate(
    fit = ovary_pred_se$fit,
    se = ovary_pred_se$se.fit,
    lo = fit - 1.96 * se,
    hi = fit + 1.96 * se
  )

SHARED_THEME <- theme(
  panel.spacing = unit(1, "lines"),
  strip.background = element_blank(),
  strip.placement = "outside",
  strip.text.x = element_blank(), # Remove sub-labels (facet labels)
  axis.line.x = element_line(color = "black"), # Add bottom axis line
  axis.line.y = element_line(color = "black"),
  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
)

# ===== PANEL A: PCA LOADINGS CLEVELAND PLOT =====

# Define focal columns and prepare numerical data for PCA
focal_columns <- c("Degree", "Close", "Eigen", "Between", "boutDegree", "boutBetween", "boutClose", "boutEigen", "bodyDegree", "bodyBetween", "bodyClose", "bodyEigen", "mean_vel", "move_perc", "N90.Day4", "Initiation.Freq", "clust")
numerical_data <- bds_means[, focal_columns]
numerical_data <- numerical_data[complete.cases(numerical_data), ]
data_normalized <- scale(numerical_data)

# New human-readable column names
human_readable_names <- c(
  "Degree Centrality", "Closeness Centrality", "Eigenvector Centrality", "Betweenness Centrality",
  "Bout Degree Centrality", "Bout Betweenness Centrality", "Bout Closeness Centrality", "Bout Eigenvector Centrality",
  "Body Degree Centrality", "Body Betweenness Centrality", "Body Closeness Centrality", "Body Eigenvector Centrality",
  "Mean Velocity", "Movement Percentage", "N90 Day 4", "Initiation Frequency", "Clustering Coefficient"
)

# Assign new human-readable names to the columns of numerical_data
colnames(numerical_data) <- human_readable_names
# Perform PCA
data.pca <- prcomp(data_normalized, scale = TRUE)
pca_data <- as.data.frame(data.pca$x)
rownames(data.pca$rotation) <- human_readable_names

# Create a data frame for PCA variable loadings with updated, human-readable variable names
loadings <- -as.data.frame(data.pca$rotation)
loadings$variables <- rownames(loadings)

# Create Cleveland plot of PC1 loadings
loadings_pc1 <- loadings %>%
  arrange(PC1) %>%
  mutate(variables = factor(variables, levels = variables))

plot_cleveland <- ggplot(loadings_pc1, aes(x = PC1, y = variables)) +
  geom_segment(aes(x = 0, xend = PC1, y = variables, yend = variables), color = "gray50") +
  geom_point(size = 3, color = "gray50", stroke = 0.2, fill = "gray25", shape = 21) +
  geom_vline(xintercept = 0, color = "gray50", size = 0.6) +
  labs(
    x = paste0("PC1 Loadings (", round(summary(data.pca)$importance[2, 1] * 100, 1), "%)"),
    y = ""
  ) +
  theme_minimal() +
  CONSISTENT_THEME_NO_ASPECT +
  theme(
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# ===== PANEL B: DEGREE BY MOVEMENT PERCENTAGE (from s15.R) =====

# Ovary Index by Degree with linear models (flipped x and y, removed queens)
plot_lm <- ggplot(bds_means %>% filter(Q_QRW_QLW_Keystone != "Queen"), aes(x = ovary_idx, y = Degree, color = Q_QRW_QLW_Keystone)) +
  geom_point(aes(size = Q_QRW_QLW_Keystone %in% c("Keystone")), stroke = 0.2, alpha = .75) +
  geom_ribbon(
    data = ovary_pred_display %>% filter(QR_Queen_Condition == "Queenright"),
    aes(x = ovary_idx, ymin = lo, ymax = hi), fill = "#642076", alpha = 0.2, inherit.aes = FALSE
  ) +
  geom_ribbon(
    data = ovary_pred_display %>% filter(QR_Queen_Condition == "Queenless"),
    aes(x = ovary_idx, ymin = lo, ymax = hi), fill = "#E68200", alpha = 0.2, inherit.aes = FALSE
  ) +
  geom_line(
    data = ovary_pred_display %>% filter(QR_Queen_Condition == "Queenright"),
    aes(x = ovary_idx, y = fit), color = "#642076", linewidth = 0.8, inherit.aes = FALSE
  ) +
  geom_line(
    data = ovary_pred_display %>% filter(QR_Queen_Condition == "Queenless"),
    aes(x = ovary_idx, y = fit), color = "#E68200", linewidth = 0.8, inherit.aes = FALSE
  ) +
  scale_color_manual(
    labels = c("Queenright Worker", "Queenless\nHub Worker", "Queenless Non-Hub Worker"),
    values = c(Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$KEY, Q_QRW_KEY_QLW$QLW)
  ) +
  scale_size_manual(values = c(`TRUE` = 3, `FALSE` = 1)) +
  guides(size = FALSE) +
  ylab("Std. Interactions per Hour") +
  xlab("Ovary Index") +
  CONSISTENT_THEME_NO_ASPECT +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 3))) +
  theme(
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 9, color = "black"),
    legend.key.width = unit(1.5, "cm")
  )

# ===== PANEL C: OVARY INDEX (from figure_4.R) =====

# Data preparation for ovary index plot
bds_means_of_means_Q_QRW_QLW_Keystone$Group <- with(bds_means_of_means_Q_QRW_QLW_Keystone, ifelse(Q_QRW_QLW_Keystone %in% c("Queen", "Queenright"), "Q + QRw", "Key + QLw"))
bds_means_of_means_Q_QRW_QLW_Keystone$Group <- factor(bds_means_of_means_Q_QRW_QLW_Keystone$Group, levels = c("Q + QRw", "Key + QLw"))

bds_means$Group <- with(bds_means, ifelse(Q_QRW_QLW_Keystone %in% c("Queen", "Queenright"), "Q + QRw", "Key + QLw"))
bds_means$Group <- factor(bds_means$Group, levels = c("Q + QRw", "Key + QLw"))

# Adjust the factor levels for plotting
bds_means_of_means_Q_QRW_QLW_Keystone <- bds_means_of_means_Q_QRW_QLW_Keystone %>%
  mutate(Q_QRW_QLW_Keystone = fct_recode(Q_QRW_QLW_Keystone,
    "Queen" = "Queen",
    "Queenright Worker" = "Queenright",
    "Queenless\nHub Worker" = "Keystone",
    "Queenless Non-Hub Worker" = "Queenless"
  ))

bds_means <- bds_means %>%
  mutate(Q_QRW_QLW_Keystone = fct_recode(Q_QRW_QLW_Keystone,
    "Queen" = "Queen",
    "Queenright Worker" = "Queenright",
    "Queenless\nHub Worker" = "Keystone",
    "Queenless Non-Hub Worker" = "Queenless"
  ))

# Set the factor levels with line breaks for plotting for both dataframes
bds_means$Q_QRW_QLW_Keystone <- factor(
  bds_means$Q_QRW_QLW_Keystone,
  levels = c(
    "Queen",
    "Queenright Worker",
    "Queenless\nHub Worker",
    "Queenless Non-Hub Worker"
  ),
  labels = c(
    "Queen",
    "Queenright Worker",
    "Queenless\nHub Worker",
    "Queenless\nNon-Hub Worker"
  )
)

bds_means_of_means_Q_QRW_QLW_Keystone$Q_QRW_QLW_Keystone <- factor(
  bds_means_of_means_Q_QRW_QLW_Keystone$Q_QRW_QLW_Keystone,
  levels = c(
    "Queen",
    "Queenright Worker",
    "Queenless\nHub Worker",
    "Queenless Non-Hub Worker"
  ),
  labels = c(
    "Queen",
    "Queenright Worker",
    "Queenless\nHub Worker",
    "Queenless\nNon-Hub Worker"
  )
)

# Plot the ovary index
plot_oi <- ggplot(bds_means %>% filter(QR_Queen_Condition != "Queen"), aes(x = Q_QRW_QLW_Keystone, y = ovary_idx)) +
  geom_line(data = bds_means_of_means_Q_QRW_QLW_Keystone, aes(group = Trial), color = "darkgray", linewidth = 0.2) +
  geom_point(data = bds_means_of_means_Q_QRW_QLW_Keystone %>% filter(Queen == 1), aes(color = Trial, fill = Trial), size = 3, shape = 21, stroke = 0.2, color = "black") +
  geom_beeswarm(aes(color = Trial), stroke = 0, size = 1.5, alpha = .4, method = "hex") +
  geom_point(data = bds_means_of_means_Q_QRW_QLW_Keystone %>% filter(Queen == 0), aes(color = Trial, fill = Trial), size = 3, shape = 21, stroke = 0.2, color = "black") +
  scale_color_manual(
    values = COLONY_COLORS
  ) +
  scale_fill_manual(values = COLONY_COLORS) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Ovary Index") +
  scale_x_discrete(labels = c(
    "Queen" = "Queen",
    "Queenright\nWorker" = "Queenright\nWorker",
    "Queenless\nHub" = "Queenless\nHub",
    "Queenless\nNon-Hub\nWorker" = "Queenless\nNon-Hub\nWorker"
  )) +
  theme_minimal() +
  CONSISTENT_THEME_NO_ASPECT +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5)) +
  facet_grid(~Group, scales = "free_x", space = "free_x", switch = "x", labeller = labeller(.rows = label_both, .cols = label_both)) +
  SHARED_THEME +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    strip.text = element_text(size = 10, color = "black")
  )

# ===== COMBINE ALL THREE PLOTS =====
final_plot <- plot_grid(plot_cleveland, plot_oi, plot_lm, ncol = 3, rel_widths = c(1.2, 1.2, 1), align = "hv", axis = "tb")

# Save the combined figure
ggsave("figures/manuscript/fig_4_new.jpeg", final_plot, width = 12, height = 4, dpi = 600)