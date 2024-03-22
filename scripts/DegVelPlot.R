# Load Velocity Data  ----------------------------------------

library(tidyverse)
setwd("~/Downloads/IBRGData_110123")
prefixes <- c("RooibosTea_QR_1216_1646", "RooibosTea_QL_1216_1646", "MexHotChoc_QR_1216_1646", "MexHotChoc_QL_1216_1646", "20230213_1745_AlmdudlerGspritzt_C1", "20230213_1745_AlmdudlerGspritzt_C0", "20221209_1613_QR", "20221209_1613_QL", "20221123_1543_AmericanoLatte_QR", "20221123_1543_AmericanoLatte_QL")
Day <- 1
Day1 <- c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020", "021", "022", "023", "024")
Day2 <- c("025", "026", "027", "028", "029", "030", "031", "032", "033", "034", "035", "036", "037", "038", "039", "040", "041", "042", "043", "044", "045", "046", "047", "048")
Day3 <- c("049", "050", "051", "052", "053", "054", "055", "056", "057", "058", "059", "060", "061", "062", "063", "064", "065", "066", "067", "068", "069", "070", "071", "072")
Day4 <- c("073", "074", "075", "076", "077", "078", "079", "080", "081", "082", "083", "084", "085", "086", "087", "088", "089", "090", "091", "092", "093", "094", "095")
Days <- c(Day1, Day2, Day3, Day4)
Start = 0

for (i in 1:length(prefixes)) {
  for (j in 1:length(Days)) {
    if (file.exists(paste(prefixes[i], Days[j], "velstrackqueen.csv", sep = "_"))) {
      Vel <- read.csv(paste(prefixes[i], Days[j], "velstrackqueen.csv", sep = "_"))
      Vel$X <- gsub("'", "", Vel$X)
      Vel$X <- gsub("b", "", Vel$X)
      Vel$X <- sub("\\.0", "", Vel$X)
      colnames(Vel) <- c("Node", "mean_vel", "move_perc")
      Vel$Day <- floor((j - 1) / 9) + 1
      Vel$Hour <- Days[j]
      Vel$Col <- prefixes[i]
      Vel$QR <- i %% 2 == 1
      Vel$ID <- paste(Vel$Col, Vel$Node, sep = "_")
      Vel$Queen <- Vel$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if (Start == 1) {
        TotalVel <- rbind(TotalVel, Vel)
      }
      if (Start == 0) {
        TotalVel <- Vel
        Start <- 1
      }
    }
  }
}
TotalVel <- na.omit(TotalVel)
TotalVel <- TotalVel[TotalVel$mean_vel < 15, ]

Start=0
for (i in 1:length(prefixes)) {
  for (j in 1:length(Days)) {
    if(file.exists(paste(prefixes[i],Days[j],"DegHead.csv",sep = "_"))) {
      Deg = read.csv(paste(prefixes[i],Days[j],"DegHead.csv",sep = "_"))
      Deg$Node = sub('\\.0','',Deg$Node)
      Deg$Day = floor((j-1)/9)+1
      Deg$Hour = Days[j]
      Deg$Col = prefixes[i]
      Deg$QR = i %% 2 == 1
      Deg$ID = paste(Deg$Col, Deg$Node, sep='_')
      Deg$Mod = "Head_Head"
      Deg$Trial = str_extract(Deg$Col, ".+?(?=_)")
      Deg$Queen = Deg$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalDeg = rbind(TotalDeg, Deg)
      }
      if(Start == 0){
        TotalDeg = Deg
        Start = 1
      }
    }
  }
}

DegVel <- TotalDeg %>% right_join(TotalVel, by=c("ID", "Hour","Day","QR","Queen"))
DegVelMean <- aggregate(cbind(move_perc,mean_vel,Degree,QR,Queen) ~ ID, DegVel, mean)
DegVelMean$Col <- sub("_[^_]+$", "", DegVelMean$ID)

DegVelMean = DegVelMean[DegVelMean$move_perc > 0.25,]
ggplot(DegVelMean, aes(x = move_perc, y = Degree, colour = interaction(QR, Queen))) +
  geom_smooth(
    method = lm, # Change method to lm for linear model
    se = T, # Standard error
    size = .75, # Adjust size to match first plot
    linetype = 1, # Line type
    alpha = .5, # Transparency of the line
    aes(color = interaction(QR,Queen))
  ) + # Color by treatment
  geom_point(aes(color = interaction(QR, Queen)), size = 1, alpha = 1) +
  scale_color_manual(values = c("#CEB175", "#629CC0", "#E54E21"), labels = c("Queenless Worker", "Queenright Worker", "Queen"), guide = guide_legend(direction = "vertical")) + # Adjust colors and labels
  scale_fill_manual(values = c("#CEB175", "#629CC0","#E54E21"), labels = c("Queenless Worker", "Queenright Worker", "Queen"), guide = guide_legend(direction = "vertical")) +
  scale_x_continuous() + # Use a continuous scale for x
  scale_y_continuous() + # Use a continuous scale for y
  # labs(title="Total Interaction Count vs Max Oocyte Width", color = "Treatment") +  # Adjust title and legend title
  xlab("Movement Percentage") +
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
  theme_classic()+

DegVelMean = DegVelMean[DegVelMean$move_perc > 0.25,]
ggplot(DegVelMean, aes(x = move_perc, y = Degree, colour = interaction(QR, Queen))) +
  geom_smooth(
    method = lm, # Change method to lm for linear model
    se = T, # Standard error
    size = .75, # Adjust size to match first plot
    linetype = 1, # Line type
    alpha = .5, # Transparency of the line
    aes(color = interaction(QR,Queen))
  ) + # Color by treatment
  geom_point(aes(color = interaction(QR, Queen)), size = 1, alpha = 1) +
  scale_color_manual(values = c("#CEB175", "#629CC0", "#E54E21"), labels = c("Queenless Worker", "Queenright Worker", "Queen"), guide = guide_legend(direction = "vertical")) + # Adjust colors and labels
  scale_fill_manual(values = c("#CEB175", "#629CC0","#E54E21"), labels = c("Queenless Worker", "Queenright Worker", "Queen"), guide = guide_legend(direction = "vertical")) +
  scale_x_continuous() + # Use a continuous scale for x
  scale_y_continuous() + # Use a continuous scale for y
  # labs(title="Total Interaction Count vs Max Oocyte Width", color = "Treatment") +  # Adjust title and legend title
  xlab("Movement Percentage") +
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
  theme_classic()+
  facet_wrap(~Col, nrow = 5)
