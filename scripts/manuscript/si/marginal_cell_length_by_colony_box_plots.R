library(tidyverse)
library(cowplot)
library(wesanderson)
library(dplyr)

source("scripts/manuscript/constants.R")

# > bds_means_of_means_Q_QRW_QLW_Keystone$Trial
#  [1] "20221123"   "20221123"   "20221123"   "20221123"   "20221209"  
#  [6] "20221209"   "20221209"   "20221209"   "20230213"   "20230213"  
# [11] "20230213"   "20230213"   "MexHotChoc" "MexHotChoc" "MexHotChoc"
# [16] "MexHotChoc" "RooibosTea" "RooibosTea" "RooibosTea" "RooibosTea"

# [1] "AlmdudlerGspritzt"   "AmericanoLatte"      "ArgentinanMate"     
# [4] "MexicanHotChocolate" "RooibosTea"  


# 20221123 = AmericanoLatte
# 20221209 = ArgentinanMate
# 20230213 = AlmdudlerGspritzt
# MexHotChoc = MexicanHotChocolate
# RooibosTea = RooibosTea

# Read the CSV file
wing_morphometrics <- read_csv("data/2023_macros_qr_v_ql_wings.csv")


# Create the mapping of names to trial codes
name_to_trial <- c("AlmdudlerGspritzt" = "Colony 1",
                   "AmericanoLatte" = "Colony 2",
                   "ArgentinanMate" = "Colony 3",
                   "MexicanHotChocolate" = "Colony 4",
                   "RooibosTea" = "Colony 5")

# Update bds_means dataset with the new mapping
wing_morphometrics <- wing_morphometrics %>%
  mutate(Trial = recode(colony, !!!name_to_trial))


# Filter out if ID is in AlmdG_QR_2x_Tag24, AmerLatte_QR_2x_Tag43, ArgMate_QR_2x_Tag47, MexHotChoc_QR_2x_Tag13
wing_morphometrics <- wing_morphometrics %>%
  filter(!ID %in% c("AlmdG_QR_2x_Tag24", "AmerLatte_QR_2x_Tag43", "ArgMate_QR_2x_Tag47", "MexHotChoc_QR_2x_Tag13"))

wing_morphometrics <- wing_morphometrics %>%
  mutate(QR_QL = factor(treatment, levels = c("QR", "QL"), labels = c("Queenright Worker", "Queenless Worker")))

wing_morphometrics <- wing_morphometrics %>%
  mutate(wing_mean = (wing_1_mm + wing_2_mm) / 2)

ggplot(wing_morphometrics, aes(x = Trial, y = wing_mean, fill = QR_QL)) +
  geom_boxplot() +
  CONSISTENT_THEME +
  REMOVE_HASH_MARKS +
  labs(y = "Mean Marginal Cell Length (mm)", x = "",
       fill = "") +
  scale_fill_manual(values = c(Q_QRW_KEY_QLW$QRW, Q_QRW_KEY_QLW$QLW)) +
  theme(legend.position = "top")

ggsave("figures/manuscript/si/marginal_cell_length_by_colony_box_plots.jpeg", width = 4.5, height = 4.5, dpi = 600)

