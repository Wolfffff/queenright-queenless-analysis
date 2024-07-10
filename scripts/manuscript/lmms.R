# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(wesanderson)
library(patchwork)
library(factoextra)
library(ggplot2)
library(ggbeeswarm)
library(cowplot)

source("scripts/manuscript/constants.R")

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


# Dont truncate
bds_pooled %>% select(Bee, Trial, QR, Day_DayNight, Degree) %>% head(10)


lmer(data=bds_pooled,Degree ~ 1 + QR +  (1|Trial) + (1|QR:Trial) + (1|Day_DayNight)  + (1|Day_DayNight:Trial))
