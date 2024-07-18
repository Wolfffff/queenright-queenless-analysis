library(tidyverse)
library(wesanderson)

source("scripts/manuscript/constants.R")

treatment <- c("QL", "QR")
preinterp <- c(2.4638e7, 2.427e7)
deadbeefilter <- c(2.4635e7, 2.422e7)
speedfilter <- c(2.09e7, 2.09e7)
sizefilter <- c(2.063e7, 2.064e7)
spacefilter <- c(2.061e7, 2.062e7)
interp <- c(2.54e7, 2.48e7)

data <- data.frame(treatment, preinterp, deadbeefilter, speedfilter, sizefilter, spacefilter, interp)

df <- melt(data, id.vars = "treatment", variable.name = "filter")

df <- df %>% mutate(treatment = fct_recode(treatment, "Queenright" = "QR", "Queenless" = "QL"))

df <- df %>%
  mutate(filter = fct_recode(filter,
                             "Pre-Interpolation" = "preinterp",
                             "Tag Filter" = "deadbeefilter",
                             "Speed Filter" = "speedfilter",
                             "Size Filter" = "sizefilter",
                             "Space Filter" = "spacefilter",
                             "Interpolation" = "interp"))


ggplot(data = df, aes(x = filter, y = value)) +
  geom_bar(stat = "identity", aes(fill = treatment)) +
  scale_fill_manual(
    values = c(Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$QLW)
  ) +
  CONSISTENT_THEME_NO_ASPECT +
  REMOVE_HASH_MARKS +
  facet_wrap(~treatment, scales = "free_x") +
  xlab("") +
  ylab("Number of Instances") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/manuscript/si/figure_s2.jpeg", width = 8.5, height = 4.25, dpi = 1200)
