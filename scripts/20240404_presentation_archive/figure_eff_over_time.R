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

Start = 0
for(i in 1:10){
  for(j in 1:96){
    if(file.exists(paste(prefixes[i],Days[j],"NWP.csv",sep = "_"))) {
      NWP = read.csv(paste(prefixes[i],Days[j],"NWP.csv",sep = "_"))
      NWP$Day = floor((j-1)/9)+1
      NWP$Hour = Days[j]
      NWP$Col = prefixes[i]
      NWP$QR = i %% 2 == 1
      NWP$Mod = "Head_Head"
      if(Start == 1){
        TotalNWP = rbind(TotalNWP, NWP)
      }
      if(Start == 0){
        TotalNWP = NWP
        Start = 1
      }
    }
  }
}

Eff = TotalNWP[TotalNWP$params ==  "GlobalEfficiency",]
EffMean <- aggregate(values ~ Hour + QR, Eff, mean)

# Assortativity Plotting ------------------------------------------------
ggplot(data = EffMean, aes(x = as.integer(Hour), y = values, group = interaction(Col, Mod))) +
  geom_jitter(aes(color = QR), size = 1, alpha = 0.8, width = 0.2) +
  geom_smooth(aes(group = QR, color = QR),
    method = lm,
    se = T,
    size = 1.5,
    linetype = 1,
    alpha = .7
  ) +
  scale_color_manual(values = c("#629CC0", "#161414"), labels = c("Queenright", "Queenless"), guide = guide_legend(direction = "horizontal")) +
  scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA)) + # Set x-axis limits and breaks similar to the first script
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(1, 1), # Move legend to top right
    legend.justification = c(1, 1), # Align legend to top right
    legend.background = element_rect(fill = alpha("white", 0.8)),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"), # Keep vertical grid lines
    panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"), # Keep vertical grid lines
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(), # Remove horizontal grid lines
    axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
    axis.line.y = element_line(color = "black", size = 0.5), # Add y-axis line
    strip.text = element_text(size = 14, face = "bold") # Make facet plot titles larger and bold
  ) +
  labs(title = "Efficiency Over Time", color = "") +
  xlab("Time (Hour)") +
  ylab("Efficiency")

ggsave("../figures/si_efficiency_over_time.jpg", width = 8.5, height = 3, dpi = 600)



fm <- lmer(formula = values ~ 1 + QR + Day + Day:QR + (1 | Col), data = TotalAss) # to run the model
fm.null <- lmer(formula = values ~ 1 + Day + (1 | Col), data = TotalAss) # to run the model
anova(fm, fm.null)







Eff = TotalNWP[TotalNWP$params ==  “GlobalEfficiency”,]
EffMean <- aggregate(values ~ Hour + QR, Eff, mean)
fm <- lmer(formula = values ~ 1 + QR + Day + QR:Day + (1|Col), data = Eff) #to run the model
#plot(as.numeric(dfQL$values) - as.numeric(dfQR$values), main=“MHC”, xlab=“Hour”, ylab=“Difference in Average Clustering Coefficient”)
#t.test(as.numeric(dfQL$values), as.numeric(dfQR$values), paired = TRUE, alternative = “two.sided”)
ggplot(data = EffMean,
       aes(x   = Hour,
           y   = values,
           group = as.factor(QR)))+
  geom_line(aes(color=QR),
            size     = 1,
            alpha    = .7)+
  geom_smooth(aes(color=QR),method   = lm,
              se       = T,
              size     = 1.5,
              linetype = 1,
              alpha    = .7)+
  theme_classic()+
  ylab(“Mean Weighted Efficiency”)+
  scale_color_manual(name   =” Treatment”,
                     labels = c(“QL”, “QR”),
                     values = c(“#161414”, “#629CC0"))
Deg = TotalNWP[TotalNWP$params ==  “Sum”,]
DegMean <- aggregate(values ~ Hour + QR + Col, Deg, mean)
fm <- lmer(formula = values ~ 1 + QR + Day + QR:Day + (1|Col), data = Eff)