# Load necessary libraries
library(lme4)
library(tidyverse)
library(stringr)
library(ggplot2)
library(broom.mixed)
library(lmerTest)

source("constants.R")
source("load_data.R")

# QR Workers
bds_QR_Work <- bds %>%
  filter(Queen == FALSE) %>%
  filter(QR == TRUE)
t.test(bds_QR_Work$Degree, bds_QR_Work$bodyDegree, paired = TRUE, alternative = "two.sided")

bds_QL_Work <- bds %>%
  filter(Queen == FALSE) %>%
  filter(QR == FALSE)
t.test(bds_QL_Work$Degree, bds_QL_Work$bodyDegree, paired = TRUE, alternative = "two.sided")

bds_Queen <- bds %>%
  filter(Queen == TRUE)
t.test(bds_Queen$Degree, bds_Queen$bodyDegree, paired = TRUE, alternative = "two.sided")
