library(dplyr)
library(readr)
library(stringr)
library(forcats)

# Read the data
bds <- read_csv("data/data_sheet.csv")
# If we want to filter to day only...
bds <- bds[bds$TimeOfDay == "Day",]

# Add Trial information to bds
bds <- bds %>%
  mutate(Trial = str_extract(Bee, ".+?(?=_)"))

bds <- bds %>% mutate(Trial = factor(Trial,
    levels = c("20221123", "MexicanHotChocolate", "20221209", "RooibosTea", "20230213"),
    labels = c("Colony 1", "Colony 2", "Colony 3", "Colony 4", "Colony 5")
  ))

# Add Q_QRW_QLW_Keystone to bds
bds <- bds %>%
  mutate(Q_QRW_QLW_Keystone = case_when(
    QR == 0 & Infl == 0 ~ "Queenless",
    QR == 1 & Queen == 0 ~ "Queenright",
    Queen == 1 ~ "Queen",
    QR == 0 & Infl == 1 ~ "Keystone",
    TRUE ~ NA_character_ # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(Q_QRW_QLW_Keystone = factor(Q_QRW_QLW_Keystone,
    levels = c("Queenless", "Queenright", "Queen", "Keystone")
  ))

# Calculate mean values of QR, Queen, and Degree for each Bee (QR and Queen are binary so mean is the value itself)
bds_means <- bds %>%
  group_by(Bee) %>%
  summarise(across(c(
    AverageOvaryWidth, AverageWingLength, Infl,
    Degree, Close, Eigen, Between, QR, Queen, boutDegree, boutBetween,
    boutClose, boutEigen, bodyDegree, bodyBetween, bodyClose, bodyEigen,
    AverageBoutLength, Presence, AntPresence, mean_vel, move_perc, N90.Day4,
    MRSD.Day4, Initiation.Freq, clust
  ), mean, na.rm = TRUE))

bds_means$ovary_idx <- bds_means$AverageOvaryWidth / bds_means$AverageWingLength
# Extract Trial information from Bee column in bds_means
bds_means <- bds_means %>%
  mutate(Trial = str_extract(Bee, ".+?(?=_)"))

# Define QR_Queen_Condition based on QR and Queen values
bds_means <- bds_means %>%
  mutate(QR_Queen_Condition = case_when(
    QR == 0 & Queen == 0 ~ "Queenless",
    QR == 1 & Queen == 0 ~ "Queenright",
    Queen == 1 ~ "Queen"
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright", "Queenless")))

# Create ID column for aggregation
bds_means <- bds_means %>%
  mutate(ID = paste(Trial, QR_Queen_Condition))

bds_means$ovary_idx <- bds_means$ovary_idx


se <- function(x) {
  tryCatch({
    sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
  }, error = function(e) {
    NA
  })
}

bds_means_of_means <- bds_means %>%
  group_by(ID, Trial) %>%
  summarise(across(c(ovary_idx, Degree, Close, Eigen, Between, QR, Queen, boutDegree, boutBetween, boutClose, boutEigen, bodyDegree, bodyBetween, bodyClose, bodyEigen, AverageBoutLength, Presence, AntPresence, mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq, clust), 
                   list(mean = ~mean(.x, na.rm = TRUE), se = se))) %>%
  rename_with(~str_replace(., "_mean", ""), ends_with("_mean"))
# Add QR_Queen_Condition and Q_QRW_QLW_Keystone to each level
bds <- bds %>%
  mutate(QR_Queen_Condition = case_when(
    QR == 0 & Queen == 0 ~ "Queenless",
    QR == 1 & Queen == 0 ~ "Queenright",
    Queen == 1 ~ "Queen"
  ), Q_QRW_QLW_Keystone = case_when(
    QR == 0 & Infl == 0 ~ "Queenless",
    QR == 1 & Queen == 0 ~ "Queenright",
    Queen == 1 ~ "Queen",
    QR == 0 & Infl == 1 ~ "Keystone",
    TRUE ~ "NA_character_" # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(
    QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright", "Queenless")),
    Q_QRW_QLW_Keystone = factor(Q_QRW_QLW_Keystone, levels = c("Queen", "Queenright", "Keystone", "Queenless"))
  )

bds_means <- bds_means %>%
  mutate(QR_Queen_Condition = case_when(
    QR == 0 & Queen == 0 ~ "Queenless",
    QR == 1 & Queen == 0 ~ "Queenright",
    Queen == 1 ~ "Queen"
  )) %>%
  mutate(
    QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright", "Queenless")),
    Q_QRW_QLW_Keystone = case_when(
      QR == 0 & Infl == 0 ~ "Queenless",
      QR == 1 & Queen == 0 ~ "Queenright",
      Queen == 1 ~ "Queen",
      QR == 0 & Infl == 1 ~ "Keystone",
      TRUE ~ NA_character_ # This handles any other case, which shouldn't exist in your scenario
    )
  ) %>%
  mutate(
    QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright", "Queenless")),
    Q_QRW_QLW_Keystone = factor(Q_QRW_QLW_Keystone, levels = c("Queen", "Queenright", "Keystone", "Queenless"))
  )

bds_means_of_means <- bds_means_of_means %>%
  mutate(QR_Queen_Condition = case_when(
    str_detect(ID, "Queenless") ~ "Queenless",
    str_detect(ID, "Queenright") ~ "Queenright",
    str_detect(ID, "Queen") ~ "Queen"
  )) %>%
  mutate(
    QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright", "Queenless")),
    Q_QRW_QLW_Keystone = case_when(
      str_detect(ID, "Queenless") ~ "Queenless",
      str_detect(ID, "Queenright") ~ "Queenright",
      str_detect(ID, "Queen") ~ "Queen",
      TRUE ~ NA_character_ # This handles any other case, which shouldn't exist in your scenario
    )
  ) %>%
  mutate(
    QR_Queen_Condition = factor(QR_Queen_Condition, levels = c("Queen", "Queenright", "Queenless")),
    Q_QRW_QLW_Keystone = factor(Q_QRW_QLW_Keystone, levels = c("Queen", "Queenright", "Keystone", "Queenless"))
  )

# Create a separate means_of_means dataset that separates all four categories of Q_QRW_QLW_Keystone
bds_means_of_means_Q_QRW_QLW_Keystone <- bds_means %>%
  group_by(Trial, Q_QRW_QLW_Keystone) %>%
  summarise(across(c(ovary_idx, Degree, Close, Eigen, Between, QR, Queen, boutDegree, boutBetween, boutClose, boutEigen, bodyDegree, bodyBetween, bodyClose, bodyEigen, AverageBoutLength, Presence, AntPresence, mean_vel, move_perc, N90.Day4, MRSD.Day4, Initiation.Freq, clust), mean, na.rm = TRUE))



nwp <- read_csv("data/network_parameters.csv")


# Map the trial names to human-readable colony names that match the ordering in the figure
nwp <- nwp %>%
  mutate(Trial = recode(Col,
    "RooibosTea_QR_1216_1646" = "RooibosTea",
    "RooibosTea_QL_1216_1646" = "RooibosTea",
    "MexHotChoc_QR_1216_1646" = "MexicanHotChocolate",
    "MexHotChoc_QL_1216_1646" = "MexicanHotChocolate",
    "20230213_1745_AlmdudlerGspritzt_C1" = "20230213",
    "20230213_1745_AlmdudlerGspritzt_C0" = "20230213",
    "20221209_1613_QR" = "20221209",
    "20221209_1613_QL" = "20221209",
    "20221123_1543_AmericanoLatte_QR" = "20221123",
    "20221123_1543_AmericanoLatte_QL" = "20221123"
  )) %>%
  mutate(Trial = factor(Trial,
    levels = c("20221123", "20221209", "20230213", "MexicanHotChocolate", "RooibosTea"),
    labels = c("Colony 1", "Colony 2", "Colony 3", "Colony 4", "Colony 5")
  ))

nwp$QR <- factor(nwp$QR, levels = c(TRUE, FALSE), labels = c("Queenright", "Queenless"))