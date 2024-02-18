# Load Degree Data — Head And Body ----------------------------------------
list_of_packages = c("lme4","ggplot2","sp","tidyverse","car", "raster","ggthemes")
pak::pkg_install(list_of_packages)
for(p in list_of_packages){
  library(p, character.only = TRUE)
}


prefixes = c("RooibosTea_QR_1216_1646", "RooibosTea_QL_1216_1646", "MexHotChoc_QR_1216_1646", "MexHotChoc_QL_1216_1646", "20230213_1745_AlmdudlerGspritzt_C1", "20230213_1745_AlmdudlerGspritzt_C0", "20221209_1613_QR", "20221209_1613_QL", "20221123_1543_AmericanoLatte_QR", "20221123_1543_AmericanoLatte_QL")
Day = 1
Day1 = c("016", "017", "018", "019", "020", "021", "022", "023", "024")
Day2 = c("040", "041", "042", "043", "044", "045", "046", "047", "048")
Day3 = c("064", "065", "066", "067", "068", "069", "070", "071", "072")
Day4 = c("088", "089", "090", "091", "092", "093", "094", "095", "096")
Days = c(Day1, Day2, Day3, Day4)

Start = 0
for(i in 1:10){
  for(j in 1:96){
    if(file.exists(paste(prefixes[i],Days[j],"Cent.csv",sep = "_"))) {
      Cent = read.csv(paste(prefixes[i],Days[j],"Cent.csv",sep = "_"))
      Cent$Node = sub('\\.0','',Cent$X)
      Cent$Day = floor((j-1)/9)+1
      Cent$Hour = Days[j]
      Cent$Col = prefixes[i]
      Cent$QR = i %% 2 == 1
      Cent$ID = paste(Cent$Col, Cent$Node, sep='_')
      Cent$Queen = Cent$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalCent = rbind(TotalCent, Cent)
      }
      if(Start == 0){
        TotalCent = Cent
        Start = 1
      }
    }
  }
}

TotalCent = TotalCent[TotalCent$Degree > 100,]
TotalCentMean <- aggregate(cbind(Degree,Closeness,Betweenness, QR, Queen) ~ ID, TotalCent, mean)

TotalCentMean$Col <- sub("_[^_]+$", "", TotalCentMean$ID)


TotalCentMeanRanked = TotalCentMean %>%
  group_by(Col) %>%
  mutate(Rank = order(order(Degree, decreasing=TRUE)))

library(tidyr)
TotalCentMeanRanked = DegVelMean %>%
  group_by(Col) %>%
  top_n(10)


TotalCentMeanRanked = TotalCentMeanRanked[TotalCentMeanRanked$Degree > 200,]
TotalCentMeanRanked = TotalCentMeanRanked[!grepl("#82", TotalCentMeanRanked$ID),]
TotalCentMeanRanked = TotalCentMeanRanked[!grepl("#88", TotalCentMeanRanked$ID),]
top11 <- top_n(TotalCentMeanRanked,11,Degree)
bottom11 <- top_n(TotalCentMeanRanked,-11,Degree)
top10RT <- top_n(DegVelMeanDayRT[DegVelMeanDayRT$Day==1,],10,Degree)
Ovaries = read.csv("OvaryMeasurements.csv")
Ovaries$AverageLength = (Ovaries$LongestOocyteLength1..mm. + Ovaries$LongestOocyteLength2..mm.) / 2
Ovaries$AverageWidth = (Ovaries$LongestOocyteWidth1..mm. + Ovaries$LongestOocyteWidth2..mm.) / 2

Ovaries$ID = paste("RooibosTea_",Ovaries$Treatment,"_1216_1646_ArUcoTag#",Ovaries$Tag,sep="")
DegVelMeanRT = DegVelMean[DegVelMean$Col == "RooibosTea_QR_1216_1646" | DegVelMean$Col == "RooibosTea_QL_1216_1646",]
DegOvMeanRT = merge(DegVelMeanRT, Ovaries, by="ID")
DegOvMeanRT$Queen = DegOvMeanRT$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")

HBVelMeanRT = HBVelMean[HBVelMean$Col == "RooibosTea_QR_1216_1646" | HBVelMean$Col == "RooibosTea_QL_1216_1646",]
HBOvMeanRT = merge(HBVelMeanRT, Ovaries, by="ID")
HBOvMeanRT$Queen = HBOv# Plotting the graph for HBOvMeanRT with the styling from the first plot

# Swap levels of QR to match the first plot
HBOvMeanRT$Treatment <- factor(HBOvMeanRT$Treatment, levels = c("QR", "QL"), labels = c("Queenright", "Queenless"))


ggplot(HBOvMeanRT, aes(x=AverageWidth, y=Sum)) +
  geom_point(aes(color = factor(Treatment)), size = 2, alpha = 0.9, width = 0.2) +
  geom_smooth(method   = lm,  # Change method to lm for linear model
              se       = T,  # Standard error
              size     = 1,  # Adjust size to match first plot
              linetype = 1,  # Line type
              alpha    = .7,  # Transparency of the line
              aes(color = factor(Treatment))) +  # Color by treatment
  scale_color_manual(values=c("#629CC0", "#161414"), labels = c("Queenless", "Queenright"), guide = guide_legend(direction = "horizontal")) +  # Adjust colors and labels
  scale_x_continuous() +  # Use a continuous scale for x
  scale_y_continuous() +  # Use a continuous scale for y
  labs(title="Total Interaction Count vs Max Oocyte Width", color = "Treatment") +  # Adjust title and legend title
  xlab("Average Max Oocyte Width (mm)") + ylab("Total Interaction Count") +  # Adjust axis labels
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),  # Keep vertical grid lines
    panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"),  # Keep vertical grid lines
    panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(),  # Remove horizontal grid lines
    axis.line.x = element_line(color = "black", size = 0.5),  # Add x-axis line
    axis.line.y = element_line(color = "black", size = 0.5),  # Add y-axis line
    strip.text = element_text(size = 14, face = "bold"),  # Make facet plot titles larger and bold
    axis.text.y.right = element_blank(),  # Remove right y-axis text
    axis.ticks.y.right = element_blank()  # Remove right y-axis ticks
  ) +
  facet_wrap(~ Treatment, scales = "free", nrow = 2) + # + # Facet wrap by Treatment with free x and y scales and 2 rows
  xlim(0, 1)  # Set x-axis limits

# Saving the plot
ggsave("../figures/fig4_ovary_plot.jpg", width = 8.5, height = 6, dpi =600)
