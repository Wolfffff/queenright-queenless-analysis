# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(cowplot)
library(ggbeeswarm)

source("scripts/manuscript/constants.R")
source("scripts/manuscript/load_data.R")

# Adjust QR_Queen_Condition in bds_means
bds_means <- bds_means %>%
  mutate(QR_Queen_Condition = case_when(
    QR_Queen_Condition == "Queenless" ~ "Queenless\nWorker",
    QR_Queen_Condition == "Queenright" ~ "Queenright\nWorker",
    TRUE ~ QR_Queen_Condition # Retains the names for "Queen" and "Keystone"
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright\nWorker", "Queenless\nWorker")))

# Adjust QR_Queen_Condition in bds_means_of_means
bds_means_of_means <- bds_means_of_means %>%
  mutate(QR_Queen_Condition = case_when(
    QR_Queen_Condition == "Queenless" ~ "Queenless\nWorker",
    QR_Queen_Condition == "Queenright" ~ "Queenright\nWorker",
    TRUE ~ QR_Queen_Condition # Retains the names for "Queen" and "Keystone"
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright\nWorker", "Queenless\nWorker")))


queenright_means <- bds_means %>%
  filter(QR_Queen_Condition == "Queenright\nWorker") %>%
  group_by(Trial) %>%
  summarise(Queenright_Mean_Degree = mean(Degree))

# Step 2: Join the queenright means with the original dataset and compute the difference
bds_means_with_diff <- bds_means %>%
  left_join(queenright_means, by = "Trial") %>%
  mutate(Difference_From_Queenright = Degree - Queenright_Mean_Degree)

# Step 3: Compute the mean degree for the queenright condition for each trial in bds_means_of_means
queenright_means_of_means <- bds_means_of_means %>%
  filter(QR_Queen_Condition == "Queenright\nWorker") %>%
  group_by(Trial) %>%
  summarise(Queenright_Mean_Degree = mean(Degree))

# Step 4: Join the queenright means with the original dataset and compute the difference
bds_means_of_means_with_diff <- bds_means_of_means %>%
  left_join(queenright_means_of_means, by = "Trial") %>%
  mutate(Difference_From_Queenright = Degree - Queenright_Mean_Degree)


# Plot Degree
plot_degree <- ggplot(bds_means_with_diff %>% filter(QR_Queen_Condition != "Queen"), aes(x = QR_Queen_Condition, y = Difference_From_Queenright)) +
  geom_point(data = bds_means_of_means_with_diff %>% filter(Queen == 1), aes(color = Trial, fill = Trial), size = 3, shape = 21, stroke = 0.2, position = position_dodge(width = 0.5), color = "black") +
  geom_beeswarm(aes(color = Trial), stroke = 0, size = 1, alpha = .2, method = "hex") +
  geom_point(data = bds_means_of_means_with_diff %>% filter(Queen == 0), aes(color = Trial, fill = Trial), size = 3, shape = 21, stroke = 0.2, position = position_dodge(width = 0.5), color = "black") +
  scale_color_manual(values = COLONY_COLORS) +
  scale_fill_manual(values = COLONY_COLORS) +
  xlab("") +
  ylab("Δ from Colony Queenright Worker Mean\nStd. Interactions per Hour") +
  CONSISTENT_THEME +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank(), # Removes the legend title if not needed
    legend.spacing.x = unit(0.2, "cm"), # Reduces the space between legend labels
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0), # Reduces the margin around the legend
    legend.key.width = unit(0.25, "cm"), # Reduces the width of the legend keys
    legend.key.height = unit(0.25, "cm"), # Reduces the height of the legend keys
    axis.title.y = element_text(size = 8)
  )


# Plot Initiation.Freq
plot_init_freq <- ggplot(bds_means_of_means, aes(x = QR_Queen_Condition, y = N90.Day4, color = Trial)) +
  # geom_line(aes(group = Trial), color = "darkgray", linewidth = 0.2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = N90.Day4 - N90.Day4_se, ymax = N90.Day4 + N90.Day4_se, group = Trial), 
                position = position_dodge(width = 0.5), width = 0, color = "black") +
  geom_point(aes(fill = Trial), shape= 21, color="black", position = position_dodge(width = 0.5), size = 2.5, stroke = 0.2) +
  scale_color_manual(values = COLONY_COLORS) +
  scale_fill_manual(values = COLONY_COLORS) +
  xlab("") +
  ylab("N90 (Dispersion)") +
  CONSISTENT_THEME

# Plot N90.Day4
plot_n90_day4 <- ggplot(bds_means, aes(x = QR_Queen_Condition, y = N90.Day4)) +
  geom_beeswarm(aes(color = Trial), stroke = 0.5, size = 1, alpha = .3, method = "hex") +
  geom_line(data = bds_means_of_means, aes(group = Trial), color = "darkgray", linewidth = 0.2) +
  geom_point(data = bds_means_of_means, aes(color = Trial, fill = Trial), size = 1, shape = 21, stroke = 0.2, color = "white") +
  scale_color_manual(values = COLONY_COLORS) +
  scale_fill_manual(values = COLONY_COLORS) +
  xlab("") +
  ylab("N90 (Dispersion)") +
  CONSISTENT_THEME

# Combine plots
plots <- plot_grid(plot_degree, plot_init_freq, ncol = 2, align = "vh")

# Save the combined plot
ggsave("figures/scratch.jpeg", plot = plots + theme(plot.margin = margin(1, 1, 1, 1, "cm")), width = 8.5, height = 4, dpi = 600)
 