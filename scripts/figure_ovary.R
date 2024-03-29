# Load Degree Data — Head And Body ----------------------------------------
list_of_packages <- c("lme4", "ggplot2", "sp", "tidyverse", "car", "raster", "ggthemes", "ggExtra", "pak", "ggside")
pak::pkg_install(list_of_packages)
for (p in list_of_packages) {
  library(p, character.only = TRUE)
}

prefixes <- c("RooibosTea_QR_1216_1646", "RooibosTea_QL_1216_1646", "MexHotChoc_QR_1216_1646", "MexHotChoc_QL_1216_1646", "20230213_1745_AlmdudlerGspritzt_C1", "20230213_1745_AlmdudlerGspritzt_C0", "20221209_1613_QR", "20221209_1613_QL", "20221123_1543_AmericanoLatte_QR", "20221123_1543_AmericanoLatte_QL")
Day <- 1
Day1 <- c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020", "021", "022", "023", "024")
Day2 <- c("025", "026", "027", "028", "029", "030", "031", "032", "033", "034", "035", "036", "037", "038", "039", "040", "041", "042", "043", "044", "045", "046", "047", "048")
Day3 <- c("049", "050", "051", "052", "053", "054", "055", "056", "057", "058", "059", "060", "061", "062", "063", "064", "065", "066", "067", "068", "069", "070", "071", "072")
Day4 <- c("073", "074", "075", "076", "077", "078", "079", "080", "081", "082", "083", "084", "085", "086", "087", "088", "089", "090", "091", "092", "093", "094", "095", "096")
Days <- c(Day1, Day2, Day3, Day4)
Start <- 0


QUEEN_LIST <- c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")

Start <- 0
for (i in 1:10) {
  for (j in 1:96) {
    if (file.exists(paste(prefixes[i], Days[j], "Cent.csv", sep = "_"))) {
      Cent <- read.csv(paste(prefixes[i], Days[j], "Cent.csv", sep = "_"))
      Cent$Node <- sub("\\.0", "", Cent$X)
      Cent$Day <- floor((j - 1) / 9) + 1
      Cent$Hour <- Days[j]
      Cent$Col <- prefixes[i]
      Cent$QR <- i %% 2 == 1
      Cent$ID <- paste(Cent$Col, Cent$Node, sep = "_")
      Cent$Queen <- Cent$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if (Start == 1) {
        TotalCent <- rbind(TotalCent, Cent)
      }
      if (Start == 0) {
        TotalCent <- Cent
        Start <- 1
      }
    }
  }
}

# TotalCent <- TotalCent[TotalCent$Degree > 100, ]
TotalCentMean <- aggregate(cbind(Degree, Closeness, Betweenness, QR, Queen) ~ ID, TotalCent, mean)
TotalCentMean$Col <- sub("_[^_]+$", "", TotalCentMean$ID)


TotalCentMeanRanked <- TotalCentMean %>%
  group_by(Col) %>%
  mutate(Rank = order(order(Degree, decreasing = TRUE)))

TotalCentMeanRanked <- TotalCentMeanRanked[order(TotalCentMeanRanked$Degree), ]
dim(TotalCentMeanRanked)


Ovaries <- read.csv("OvaryMeasurements.csv")

OvariesRT = Ovaries[Ovaries$Colony=="RooibosTea",]
OvariesRT$ID <- paste("RooibosTea_", OvariesRT$Treatment, "_1216_1646_ArUcoTag#", OvariesRT$Tag, sep = "")

OvariesMHC = Ovaries[Ovaries$Colony=="MexicanHotChocolate",]
OvariesMHC$ID = paste("MexHotChoc_",OvariesMHC$Treatment,"_1216_1646_ArUcoTag#",OvariesMHC$Tag,sep="")

OvariesAM = Ovaries[Ovaries$Colony=="ArgentinanMate",]
OvariesAM$ID = paste("20221209_1613_",OvariesAM$Treatment,"_ArUcoTag#",OvariesAM$Tag,sep="")

OvariesAG = Ovaries[Ovaries$Colony=="AlmdudlerGspritzt",]
OvariesAG$ID = paste("20230213_1745_AlmdudlerGspritzt_",OvariesAG$Treatment,"_ArUcoTag#",OvariesAG$Tag,sep="")

OvariesAL = Ovaries[Ovaries$Colony=="AmericanoLatte",]
OvariesAL$ID = paste("20221123_1543_AmericanoLatte_",OvariesAL$Treatment,"_ArUcoTag#",OvariesAL$Tag,sep="")


TotalCentMeanRanked <- TotalCentMeanRanked[TotalCentMeanRanked$Degree > 200, ]
TotalCentMeanRanked <- TotalCentMeanRanked[!grepl("#82", TotalCentMeanRanked$ID), ]
TotalCentMeanRanked <- TotalCentMeanRanked[!grepl("#88", TotalCentMeanRanked$ID), ]
TotalCentMeanRanked <- TotalCentMeanRanked[!grepl("#84", TotalCentMeanRanked$ID), ]

# Print shape
dim(TotalCentMeanRanked)

# ALQR#45, MHCQR#16

TotalCentMeanRanked <- TotalCentMeanRanked[!grepl("20221123_1543_AmericanoLatte_QR_ArUcoTag#45", TotalCentMeanRanked$ID), ]
TotalCentMeanRanked <- TotalCentMeanRanked[!grepl("MexHotChoc_QR_1216_1646_ArUcoTag#16", TotalCentMeanRanked$ID), ]
dim(TotalCentMeanRanked)

# Merge the data
Ovaries <- rbind(OvariesRT, OvariesMHC, OvariesAM, OvariesAG, OvariesAL)
Ovaries$AverageLength <- (Ovaries$LongestOocyteLength1..mm. + Ovaries$LongestOocyteLength2..mm.) / 2
Ovaries$AverageWidth <- (Ovaries$LongestOocyteWidth1..mm. + Ovaries$LongestOocyteWidth2..mm.) / 2

# Sum by id

TotalCentSum <- TotalCent %>%
  group_by(ID) %>%
  summarise(Degree = sum(Degree), QR = first(QR),Col = first(Col), Queen = first(Queen))

TotalCentSum <- merge(TotalCentSum, Ovaries, by = "ID")

# Swap levels of QR to match the first plot
TotalCentSum$Treatment <- ifelse(TotalCentSum$QR == 0, "Queenless Worker", "Queenright Worker")
# Keep only first of duplicates
TotalCentSum <- TotalCentSum[!duplicated(TotalCentSum$ID), ]

TotalCentSum[which(TotalCentSum$Degree == max(TotalCentSum$Degree)), ]

# Drop queens
TotalCentSum <- TotalCentSum[!(TotalCentSum$ID %in% QUEEN_LIST), ]


ggplot(TotalCentSum, aes(x = AverageWidth, y = Degree, colour = factor(Treatment))) +
  geom_xsidedensity(size=.5,aes(fill = factor(Treatment)),alpha=0.1) +
  geom_ysidedensity(size=.5,aes(fill = factor(Treatment)),alpha=0.1) +
  geom_smooth(
    method = lm, # Change method to lm for linear model
    se = T, # Standard error
    size = .75, # Adjust size to match first plot
    linetype = 1, # Line type
    alpha = .5, # Transparency of the line
    aes(color = factor(Treatment))
  ) + # Color by treatment
  geom_point(aes(color = factor(Treatment)), size = 1, alpha = 1) +
  scale_color_manual(values = c("#CEB175", "#629CC0"), labels = c("Queenless Worker", "Queenright Worker"), guide = guide_legend(direction = "horizontal")) + # Adjust colors and labels
   scale_fill_manual(values = c("#CEB175", "#629CC0"), labels = c("Queenless Worker", "Queenright Worker"), guide = guide_legend(direction = "horizontal")) +
  scale_x_continuous() + # Use a continuous scale for x
  scale_y_continuous() + # Use a continuous scale for y
  # labs(title="Total Interaction Count vs Max Oocyte Width", color = "Treatment") +  # Adjust title and legend title
  xlab("Average Max Oocyte Width (mm)") +
  ylab("Total Interaction Count") + # Adjust axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"), # Keep vertical grid lines
    panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"), # Keep vertical grid lines
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(), # Remove horizontal grid lines
    axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
    axis.line.y = element_line(color = "black", size = 0.5), # Add y-axis line
    strip.text = element_text(size = 14, face = "bold"), # Make facet plot titles larger and bold
    axis.text.y.right = element_blank(), # Remove right y-axis text
    axis.ticks.y.right = element_blank(), # Remove right y-axis ticks
    aspect.ratio = 1
  ) +
  theme_ggside_void()

ggsave("../figures/fig4_ovary_plot.jpg", width = 4.25, height = 4.25, dpi = 600)


# Assuming 'Col' is the column you want to extract prefixes from
prefixes <- c("RooibosTea", "MexHotChoc", "20221209_1613", "20230213_1745_AlmdudlerGspritzt_")

# 
TotalCentSum <- TotalCentSum %>%
  mutate(source_colony = ifelse(str_detect(Col, paste(prefixes, collapse = "|")), 
                                Col, 
                                NA))

# Spearman correlation between degree and average width
cor.test(TotalCentSum$Degree, TotalCentSum$AverageWidth, method = "spearman")

# lme4
library(lme4)
library(lmerTest)

# Get pvals

TotalCentSum$parent_colony <- TotalCentSum$source_colony
TotalCentSum$group_id <- TotalCentSum$Colony

mod <- lmer(Degree ~ 1 + AverageWidth + Treatment + (1 | group_id) + (1 | parent_colony), data = TotalCentSum)
summary(mod)

