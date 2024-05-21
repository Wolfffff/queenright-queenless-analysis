# Load Degree Data — Head And Body ----------------------------------------
list_of_packages <- c("lme4", "ggplot2", "sp", "tidyverse", "car", "raster", "ggthemes", "dplyr")
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

Start <- 0
for (i in 1:10) {
  for (j in 1:96) {
    if (file.exists(paste(prefixes[i], Days[j], "NWP.csv", sep = "_"))) {
      NWP <- read.csv(paste(prefixes[i], Days[j], "NWP.csv", sep = "_"))
      NWP$Day <- floor((j - 1) / 9) + 1
      NWP$Hour <- Days[j]
      NWP$Col <- prefixes[i]
      NWP$QR <- i %% 2 == 1
      NWP$Mod <- "Head_Head"
      if (Start == 1) {
        TotalNWP <- rbind(TotalNWP, NWP)
      }
      if (Start == 0) {
        TotalNWP <- NWP
        Start <- 1
      }
    }
  }
}

Eff <- TotalNWP[TotalNWP$params == "GlobalEfficiency", ]
EffMean <- aggregate(values ~ Hour + QR + Col, Eff, mean)

# Assortativity Plotting ------------------------------------------------
# get value before first _ in source_colony
EffMean$source_colony <- sapply(strsplit(EffMean$Col, "_"), `[`, 1)

# Get mean per colony
EffMean <- EffMean %>%
  group_by(Col, QR) %>%
  summarise(values = mean(values), source_colony = first(source_colony))

library(wesanderson)
library(forcats)

# Convert QR to factor

# Order source_colony
EffMean$source_colony <- factor(EffMean$source_colony, levels = c("RooibosTea", "MexHotChoc", "20230213", "20221209", "20221123"))
EffMean$Treatment <- ifelse(EffMean$QR, "Queenright Worker", "Queenless Worker")

ggplot(EffMean, aes(x = fct_rev(Treatment), y = values)) +
  geom_line(aes(group = source_colony), color = "darkgray") +
  geom_point(aes(color = source_colony), size = 5) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Global Efficiency") + # Adjust axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    # legend.position = "none",
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
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))


ggsave("../figures/efficiency_paired.jpg", width = 6.25, height = 6.25, dpi = 600)
