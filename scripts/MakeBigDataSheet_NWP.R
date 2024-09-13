library(lme4)
library(tidyverse)
library(dplyr)
library(stringr)
library("wesanderson")
library(ggnewscale)
library(ggplot2)
library(broom.mixed)
library(lmerTest)

prefixes <- c("RooibosTea_QR_1216_1646", "RooibosTea_QL_1216_1646", "MexHotChoc_QR_1216_1646", "MexHotChoc_QL_1216_1646", "20230213_1745_AlmdudlerGspritzt_C1", "20230213_1745_AlmdudlerGspritzt_C0", "20221209_1613_QR", "20221209_1613_QL", "20221123_1543_AmericanoLatte_QR", "20221123_1543_AmericanoLatte_QL")
setwd("~/Downloads/NWP_080423")
Start <- 0
for (i in 1:10) {
  for (j in 0:95) {
    if (file.exists(paste(prefixes[i], sprintf("%03d", j), "Head_to_Head_True_NWP.csv", sep = "_"))) {
      Deg <- read.csv(paste(prefixes[i], sprintf("%03d", j), "Head_to_Head_True_NWP.csv", sep = "_"))
    }
    Deg$ExpDay <- as.factor(floor((j / 24) + 1))
    Deg$Day <- as.factor(floor(((j + 17) / 24) + 1))
    Deg$Zeit <- as.numeric((j + 7) %% 24)
    Deg$Hour <- j
    Deg$Col <- prefixes[i]
    Deg$QR <- i %% 2 == 1
    Deg$ID <- paste(Deg$Col, Deg$Hour)
    Deg$Mod <- "Head_Head"
    Deg$Trial <- str_extract(Deg$Col, ".+?(?=_)")
    if (Start == 1) {
      TotalNWP <- rbind(TotalNWP, Deg)
    }
    if (Start == 0) {
      TotalNWP <- Deg
      Start <- 1
    }
  }
}

TotalNWP <- TotalNWP %>%
  mutate(TimeOfDay = case_when(
    Zeit <= 17 & Zeit >= 8 ~ "Day",
    Zeit > 17 | Zeit < 8 ~ "Night",
    TRUE ~ NA_character_ # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(TimeOfDay = factor(TimeOfDay, levels = c("Day", "Night")))
TotalNWP$ColID <- paste(TotalNWP$Trial, TotalNWP$QR)


TotalNWPModularity <- TotalNWP[TotalNWP$params == "Modularity", ]
TotalNWPEfficiency <- TotalNWP[TotalNWP$params == "GlobalEfficiency", ]
TotalNWPTransitivity <- TotalNWP[TotalNWP$params == "Transitivity", ]
TotalNWPClustering <- TotalNWP[TotalNWP$params == "Average Clustering", ]
TotalNWPAssortativity <- TotalNWP[TotalNWP$params == "Assortativity", ]
TotalNWP_New = TotalNWP[TotalNWP$params != 'KeystoneAssortativity',]
TotalNWP_Wider = TotalNWP_New %>%
  mutate(n = 1) %>%
  tidyr::pivot_wider(names_from = params, values_from = values, values_fill = NA)
names(TotalNWP_Wider)[names(TotalNWP_Wider) == 'Average Clustering'] <- 'Average.Clustering'

