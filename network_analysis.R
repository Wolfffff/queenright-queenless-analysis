# Load Degree Data — Head And Body ----------------------------------------

library(lme4)
library(tidyverse)
setwd("~/Downloads/IBRGData_110123")
prefixes = c("RooibosTea_QR_1216_1646", "RooibosTea_QL_1216_1646", "MexHotChoc_QR_1216_1646", "MexHotChoc_QL_1216_1646", "20230213_1745_AlmdudlerGspritzt_C1", "20230213_1745_AlmdudlerGspritzt_C0", "20221209_1613_QR", "20221209_1613_QL", "20221123_1543_AmericanoLatte_QR", "20221123_1543_AmericanoLatte_QL")
Day = 1
Day1 = c("016", "017", "018", "019", "020", "021", "022", "023", "024")
Day2 = c("040", "041", "042", "043", "044", "045", "046", "047", "048")
Day3 = c("064", "065", "066", "067", "068", "069", "070", "071", "072")
Day4 = c("088", "089", "090", "091", "092", "093", "094", "095")
Days = c(Day1, Day2, Day3, Day4)
Start = 0 



for(i in 1:10){
  for(j in 1:35){
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

for(i in 1:10){
  for(j in 1:35){
    if(file.exists(paste(prefixes[i],Days[j],"DegBody.csv",sep = "_"))) {
      Deg = read.csv(paste(prefixes[i],Days[j],"DegBody.csv",sep = "_"))
      Deg$Node = sub('\\.0','',Deg$Node)
      Deg$Day = floor((j-1)/9)+1
      Deg$Hour = Days[j]
      Deg$Col = prefixes[i]
      Deg$QR = i %% 2 == 1
      Deg$ID = paste(Deg$Col, Deg$Node, sep='_')
      Deg$Mod = "Head_Body"
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

TotalDeg = TotalDeg[TotalDeg$Degree > 100,]

# Degree — Head-Head vs Head-Body -----------------------------------------------------------

QRDeg = TotalDeg[TotalDeg$QR == TRUE,]
QRDeg = QRDeg[QRDeg$Queen == FALSE,]
ggplot(QRDeg, aes(x=Hour, y=Degree, group=ID)) + 
  #geom_line(aes(group=Mod, color = Mod), alpha=0.1) + 
  geom_smooth(aes(group=Mod, color = Mod)) + 
  theme_classic() + 
  labs(title="QR Worker Degree Head_Head vs Head_Body Unstandardized") +
  scale_color_manual(values=c("#4a6741", "#EECA58"))

ggplot(QRDeg, aes(x=Hour, y=Degree, group=interaction(ID, Mod))) + 
  geom_line(aes(color=Mod), alpha=0.1) + 
  geom_smooth(aes(group=Mod, color = Mod)) + 
  theme_classic() + 
  labs(title="QR Worker Degree Head_Head vs Head_Body Unstandardized") +
  scale_color_manual(values=c("#4a6741", "#EECA58"))

QLDeg = TotalDeg[TotalDeg$QR == FALSE,]
ggplot(QLDeg, aes(x=Hour, y=Degree, group=ID)) + 
  #geom_line(aes(group=Mod, color = Mod), alpha=0.1) + 
  geom_smooth(aes(group=Mod, color = Mod)) + 
  theme_classic() + 
  labs(title="QL Worker Degree Head_Head vs Head_Body Unstandardized") +
  scale_color_manual(values=c("#4a6741", "#EECA58"))

ggplot(QLDeg, aes(x=Hour, y=Degree, group=interaction(ID, Mod))) + 
  geom_line(aes(color=Mod), alpha=0.1) + 
  geom_smooth(aes(group=Mod, color = Mod)) + 
  theme_classic() + 
  labs(title="QL Worker Degree Head_Head vs Head_Body Unstandardized") +
  scale_color_manual(values=c("#4a6741", "#EECA58"))

QueenDeg = TotalDeg[TotalDeg$Queen == TRUE,]
ggplot(QueenDeg, aes(x=Hour, y=Degree, group=ID)) + 
  #geom_line(aes(group=Mod, color = Mod), alpha=0.1) + 
  geom_smooth(aes(group=Mod, color = Mod)) + 
  theme_classic() + 
  labs(title="Queen Degree Head_Head vs Head_Body Unstandardized") +
  scale_color_manual(values=c("#4a6741", "#EECA58"))

ggplot(QueenDeg, aes(x=Hour, y=Degree, group=interaction(ID, Mod))) + 
  geom_line(aes(color=Mod), alpha=0.5) + 
  geom_smooth(aes(group=Mod, color = Mod)) + 
  theme_classic() + 
  labs(title="Queen Worker Degree Head_Head vs Head_Body Unstandardized") +
  scale_color_manual(values=c("#4a6741", "#EECA58"))

fm <- lmer(formula = Degree ~ 1 + Mod + Hour + (1|Col), data = QRDeg) #to run the model
fm.null <- lmer(formula = Degree ~ 1 + Hour + (1|Col), data = QRDeg) #to run the model

library(dplyr)

TotalDegComb %>% 
  group_by(ID) %>% 
  summarise(across(starts_with("Var"), ~sum(., na.rm = TRUE)))

# Modality Prop by Degree-----
library(dplyr)
Start=0
for(i in 1:10){
  for(j in 1:35){
    if(file.exists(paste(prefixes[i],Days[j],"DegHead.csv",sep = "_")) & file.exists(paste(prefixes[i],Days[j],"DegBody.csv",sep = "_"))) {
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
      Deg2 = read.csv(paste(prefixes[i],Days[j],"DegBody.csv",sep = "_"))
      Deg2$Col = prefixes[i]
      Deg2$Node = sub('\\.0','',Deg2$Node)
      Deg2$ID = paste(Deg2$Col, Deg2$Node, sep='_')
      Deg2 = select(Deg2,-c(X,Col,Node))
      Degs = merge(Deg, Deg2, by = "ID")
      if(Start == 1){
        CombDeg = rbind(CombDeg, Degs)
      }
      if(Start == 0){
        CombDeg = Degs
        Start = 1
      }
    }
  }
}
CombDeg$Ratio = CombDeg$Degree.x/(CombDeg$Degree.x+CombDeg$Degree.y)
CombDeg$Sum = CombDeg$Degree.x+CombDeg$Degree.y
CombDegMean <- aggregate(cbind(Ratio,Sum,QR) ~ ID, CombDeg, mean)
CombDegMean$Col <- sub("_[^_]+$", "", CombDegMean$ID)
CombDegMean = CombDegMean[CombDegMean$Sum > 200,]

ggplot(CombDegMean, aes(x=Ratio, y=Sum)) + 
  geom_point() + 
  xlim(0,1) + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(QR)))+
  theme_classic() + 
  labs(title="HeadRatio vs TotalDegree")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ QR+Col, nrow = 2)

# Degree — QL vs QR vs Queen -----------------------------------------------------------

HeadDeg = TotalDeg[TotalDeg$Mod == "Head_Head",]
ggplot(HeadDeg, aes(x=Hour, y=Degree, group=ID)) + 
  #geom_line(aes(color = interaction(QR, Queen), alpha= Queen)) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="Degree in Queens, Queenless Workers, Queenright Workers") + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))

ggplot(HeadDeg, aes(x=Hour, y=Degree, group=ID)) + 
  geom_line(aes(color = interaction(QR, Queen))) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="Degree in Queens, Queenless Workers, Queenright Workers") + 
  theme(legend.position = 'none')  + 
  scale_alpha_discrete(range = c(0.02, 0.25)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))

ggplot(HeadDeg, aes(x=Hour, y=Degree, group=ID)) + 
  geom_jitter(aes(color = interaction(QR, Queen), alpha=Queen)) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="Degree in Queens, Queenless Workers, Queenright Workers") + 
  theme(legend.position = 'none')  + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))+
  facet_wrap(~ Col, nrow = 5)

fm <- lmer(formula = Degree ~ 1 + QR + Day + Day:QR +(1|Col), data = HeadDeg) #to run the model
fm.null <- lmer(formula = Degree ~ 1 + Day + (1|Col), data = HeadDeg) #to run the model

# Degree with significance testing--------
DegMean <- aggregate(cbind(Degree,QR,Queen) ~ Trial+ID, HeadDeg, mean)

ggplot(DegMean, aes(x=interaction(QR,Queen), y=Degree)) +
  geom_violin(aes(fill=interaction(QR,Queen)))+
  geom_jitter(aes(color=interaction(QR,Queen)))+
  theme_classic()+
  scale_fill_manual(name   ="Bee",
                                     labels = c("QL Worker", "QR Worker", "Queen"),
                                     values = c("#161414", "#629CC0", "#7851A9")) +
  scale_color_manual(name   ="Bee",
                    labels = c("QL Worker", "QR Worker", "Queen"),
                    values = c("#161414", "#326C90", "#7851A9")) +
  geom_signif(
    comparisons = list(c("0.0", "1.0")),
    map_signif_level = TRUE, textsize = 2)+
  facet_wrap(~ Trial, nrow = 1)+
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "")

# BodyDegree Data -----------
BodyDeg = TotalDeg[TotalDeg$Mod == "Head_Body",]
ggplot(BodyDeg, aes(x=Hour, y=Degree, group=ID)) + 
  #geom_line(aes(color = interaction(QR, Queen), alpha= Queen)) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="Degree in Queens, Queenless Workers, Queenright Workers") + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))

ggplot(BodyDeg, aes(x=Hour, y=Degree, group=ID)) + 
  geom_line(aes(color = interaction(QR, Queen), alpha= Queen)) + 
  #geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="BodyDegree in Queens, Queenless Workers, Queenright Workers") + 
  theme(legend.position = 'none')  + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))


# Load Velocity Data  ----------------------------------------

setwd("~/Downloads/IBRGData_110123")
prefixes = c("RooibosTea_QR_1216_1646", "RooibosTea_QL_1216_1646", "MexHotChoc_QR_1216_1646", "MexHotChoc_QL_1216_1646", "20230213_1745_AlmdudlerGspritzt_C1", "20230213_1745_AlmdudlerGspritzt_C0", "20221209_1613_QR", "20221209_1613_QL", "20221123_1543_AmericanoLatte_QR", "20221123_1543_AmericanoLatte_QL")
Day = 1
Day1 = c("016", "017", "018", "019", "020", "021", "022", "023", "024")
Day2 = c("040", "041", "042", "043", "044", "045", "046", "047", "048")
Day3 = c("064", "065", "066", "067", "068", "069", "070", "071", "072")
Day4 = c("088", "089", "090", "091", "092", "093", "094", "095")
Days = c(Day1, Day2, Day3, Day4)
Start = 0 

for(i in 1:10){
  for(j in 1:35){
    if(file.exists(paste(prefixes[i],Days[j],"velstrack.csv",sep = "_"))) {
      Vel = read.csv(paste(prefixes[i],Days[j],"velstrack.csv",sep = "_"))
      Vel$X = gsub("'", "", Vel$X)
      Vel$X = gsub("b", "", Vel$X)
      Vel$X = sub('\\.0','',Vel$X)
      colnames(Vel) = c('Node','mean_vel','move_perc')
      Vel$Day = floor((j-1)/9)+1
      Vel$Hour = Days[j]
      Vel$Col = prefixes[i]
      Vel$QR = i %% 2 == 1
      Vel$ID = paste(Vel$Col, Vel$Node, sep='_')
      Vel$Queen = Vel$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalVel = rbind(TotalVel, Vel)
      }
      if(Start == 0){
        TotalVel = Vel
        Start = 1
      }
    }
  }
}
TotalVel = na.omit(TotalVel) 
TotalVel = TotalVel[TotalVel$mean_vel < 15,]


# Plot Velocity Timecourse  ----------------------------------------

ggplot(TotalVel, aes(x=Hour, y=move_perc, group=ID)) + 
  #geom_line(aes(color = interaction(QR, Queen), alpha= Queen)) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="Movement Percentage in Queens, Queenless Workers, Queenright Workers") + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))

ggplot(TotalVel, aes(x=Hour, y=move_perc, group=ID)) + 
  geom_jitter(aes(color = interaction(QR, Queen), alpha= Queen)) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="Movement Percentage in Queens, Queenless Workers, Queenright Workers") + 
  theme(legend.position = 'none')  + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))
ggplot(TotalVel, aes(x=Hour, y=move_perc, group=ID)) + 
  geom_jitter(aes(color = interaction(QR, Queen), alpha= Queen)) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="Movement Percentage in Queens, Queenless Workers, Queenright Workers") + 
  theme(legend.position = 'none')  + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))+
  facet_wrap(~ Col, nrow = 5)


ggplot(TotalVel, aes(x=Hour, y=mean_vel, group=ID)) + 
  #geom_line(aes(color = interaction(QR, Queen), alpha= Queen)) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="Movement Speed in Queens, Queenless Workers, Queenright Workers") + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))

ggplot(TotalVel, aes(x=Hour, y=mean_vel, group=ID)) + 
  geom_line(aes(color = interaction(QR, Queen), alpha= Queen)) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="Movement Speed in Queens, Queenless Workers, Queenright Workers") + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))



# Compare Degree and Velocity   ----------------------------------------
library("dplyr")


DegVel <- HeadDeg %>% right_join(TotalVel, by=c("ID", "Hour","Day","QR"))
DegVelMean <- aggregate(cbind(move_perc,mean_vel,Degree,QR) ~ ID, DegVel, mean)
DegVelMean$Col <- sub("_[^_]+$", "", DegVelMean$ID)


ggplot(DegVelMean, aes(x=move_perc, y=Degree)) + 
  geom_point() + 
  xlim(0,1) + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              color    = "black")+
  theme_classic() + 
  labs(title="Degree vs Movement Percentage")
  
ggplot(DegVelMean, aes(x=mean_vel, y=Degree)) + 
  geom_point() + 
  xlim(0,15) + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              color    = "black")+
  theme_classic() + 
  labs(title="Degree vs Movement Speed")

ggplot(DegVelMean, aes(x=move_perc, y=Degree)) + 
  geom_point() + 
  xlim(0,1) + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(QR)))+
  theme_classic() + 
  labs(title="Degree vs Movement Percentage")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ QR+ Col, nrow = 2)
library(plyr)
ddply(DegVelMean, .(Col), summarise,
      corr=(cor.test(move_perc, Degree,
                     alternative="two.sided", method="kendall")), name=names(corr) )

ggplot(DegVelMean, aes(x=mean_vel, y=Degree)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(QR)))+
  theme_classic() + 
  labs(title="Degree vs Movement Speed")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ QR+ Col, nrow = 2)
  
ggplot(data = DegVel, 
       aes(x   = Day,
           y   = Degree, 
           col = as.factor(QR)))+
  geom_point(size     = 1, 
             alpha    = .7, 
             position = "jitter")+
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7)+
  theme_minimal()+
  ylab("Mean Velocity")+
  scale_color_manual(name   =" Treatment",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0", "#7851A9"))


setwd("~/Downloads/DispersionMeasures_092923")
library(reshape2)
library(tidyverse)
library(dplyr)
library(purrr)

QR = read.csv('MexHotChoc_QR_1216_1646_096_DispersionMetrics.csv')
QR$Queen = QR$Tag == "ArUcoTag#13" | QR$Tag == "ArUcoTag#16"
QR$Dead = QR$Tag == "ArUcoTag#16" 

ggplot(data=QR, aes(x=Day, y=MRSD, group=Tag)) +
  geom_line(aes(color=Queen, linewidth=Queen, linetype =Dead)) +
  scale_linewidth_discrete(range = c(0.3, 0.9))+
  ggtitle("AlmGsp QR Dispersion") + theme_classic() + theme(legend.position = 'none') 



# Dispersion measures   ----------------------------------------

prefixes = c("RooibosTea_QR_1216_1646", "RooibosTea_QL_1216_1646", "MexHotChoc_QR_1216_1646", "MexHotChoc_QL_1216_1646", "20230213_1745_AlmdudlerGspritzt_C1", "20230213_1745_AlmdudlerGspritzt_C0", "20221209_1613_QR", "20221209_1613_QL", "20221123_1543_AmericanoLatte_QR", "20221123_1543_AmericanoLatte_QL")
Start = 0
for(i in 1:10){
  if(file.exists(paste(prefixes[i],"096","DispersionMetrics.csv",sep = "_"))) {
    Disp = read.csv(paste(prefixes[i],"096","DispersionMetrics.csv",sep = "_"))
    Disp$Col = prefixes[i]
    Disp$QR = i %% 2 == 1
    Disp$ID = paste(Disp$Col, Disp$Tag, sep='_')
    Disp$Queen = Disp$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
    if(Start == 1){
      TotalDisp = rbind(TotalDisp, Disp)
    }
    if(Start == 0){
      TotalDisp = Disp
      Start = 1
    }
  }
}

WorkerDisp=TotalDisp[TotalDisp$Queen == FALSE,]
ggplot(data=WorkerDisp, aes(x=Day, y=N50, group=ID)) +
  geom_jitter(aes(color = QR), alpha=0.1) +
  geom_smooth(aes(group = QR, color = QR), method = "loess") +
  #scale_alpha_discrete(range = c(0.1, 0.9))+
  ggtitle("Dispersion in QL vs QR") + theme_classic() +
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))

QRDisp=TotalDisp[TotalDisp$QR == TRUE,]
ggplot(data=QRDisp, aes(x=Day, y=N50, group=ID)) +
  geom_jitter(aes(color = Queen, alpha=Queen)) +
  geom_smooth(aes(group = Queen, color = Queen), method = "loess") +
  scale_alpha_discrete(range = c(0.1, 0.5))+
  ggtitle("Dispersion in Queen vs Worker") + theme_classic() +
  scale_color_manual(name   ="Bee",
                     labels = c("QR Worker", "Queen"),
                     values = c("#629CC0", "#7851A9"))

WorkerDisp=TotalDisp[TotalDisp$Queen == FALSE,]
ggplot(data=WorkerDisp, aes(x=Day, y=N90, group=ID)) +
  geom_jitter(aes(color = QR), alpha=0.1) +
  geom_smooth(aes(group = QR, color = QR), method = "loess") +
  #scale_alpha_discrete(range = c(0.1, 0.9))+
  ggtitle("Dispersion in QL vs QR") + theme_classic() +
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))

QRDisp=TotalDisp[TotalDisp$QR == TRUE,]
ggplot(data=QRDisp, aes(x=Day, y=N90, group=ID)) +
  geom_jitter(aes(color = Queen, alpha=Queen)) +
  geom_smooth(aes(group = Queen, color = Queen), method = "loess") +
  scale_alpha_discrete(range = c(0.1, 0.5))+
  ggtitle("Dispersion in Queen vs Worker") + theme_classic() +
  scale_color_manual(name   ="Bee",
                     labels = c("QR Worker", "Queen"),
                     values = c("#629CC0", "#7851A9"))

WorkerDisp=TotalDisp[TotalDisp$Queen == FALSE,]
ggplot(data=WorkerDisp, aes(x=Day, y=MRSD, group=ID)) +
  geom_jitter(aes(color = QR), alpha=0.1) +
  geom_smooth(aes(group = QR, color = QR), method = "loess") +
  #scale_alpha_discrete(range = c(0.1, 0.9))+
  ggtitle("Dispersion in QL vs QR") + theme_classic() +
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))

QRDisp=TotalDisp[TotalDisp$QR == TRUE,]
ggplot(data=QRDisp, aes(x=Day, y=MRSD, group=ID)) +
  geom_jitter(aes(color = Queen, alpha=Queen)) +
  geom_smooth(aes(group = Queen, color = Queen), method = "loess") +
  scale_alpha_discrete(range = c(0.1, 0.5))+
  ggtitle("Dispersion in Queen vs Worker") + theme_classic() +
  scale_color_manual(name   ="Bee",
                     labels = c("QR Worker", "Queen"),
                     values = c("#629CC0", "#7851A9"))



fm <- lmer(formula = N90 ~ 1 + Queen + Day + Day:Queen +(1|Col), data = QRDisp) #to run the model
fm.null <- lmer(formula = N90 ~ 1 + Day + (1|Col), data = QRDisp) #to run the model
anova(fm, fm.null)
fm <- lmer(formula = N90 ~ 1 + QR + Day + Day:QR +(1|Col), data = WorkerDisp) #to run the model
fm.null <- lmer(formula = N90 ~ 1 + Day + (1|Col), data = WorkerDisp) #to run the model
anova(fm, fm.null)


# Circadian Rhythms -------------

AllHours = sprintf("%03d", 1:96)
Start = 0
for(i in 1:10){
  for(j in 1:96){
    if(file.exists(paste(prefixes[i],AllHours[j],"velstrack.csv",sep = "_"))) {
      Vel = read.csv(paste(prefixes[i],AllHours[j],"velstrack.csv",sep = "_"))
      Vel$X = gsub("'", "", Vel$X)
      Vel$X = gsub("b", "", Vel$X)
      Vel$X = sub('\\.0','',Vel$X)
      colnames(Vel) = c('Node','mean_vel','move_perc')
      Vel$Day = floor((j-1)/24)+1
      Vel$ZG = j %% 24
      Vel$Hour = AllHours[j]
      Vel$Col = prefixes[i]
      Vel$QR = i %% 2 == 1
      Vel$ID = paste(Vel$Col, Vel$Node, sep='_')
      Vel$Queen = Vel$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalVelCirc = rbind(TotalVelCirc, Vel)
      }
      if(Start == 0){
        TotalVelCirc = Vel
        Start = 1
      }
    }
  }
}
TotalVelCirc = na.omit(TotalVelCirc) 
TotalVelCirc = TotalVelCirc[TotalVelCirc$mean_vel < 50,]

MHCQRVelCirc = TotalVelCirc[TotalVelCirc$Col == "MexHotChoc_QR_1216_1646",]
ggplot(MHCQRVelCirc, aes(x=Hour, y=move_perc, group=ID)) + 
  geom_point(aes(color = interaction(QR, Queen), alpha= Queen)) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="Movement Percentage in Queens, Queenright Workers") + 
  theme(legend.position = 'none', axis.text.x=element_blank())  + 
  scale_alpha_discrete(range = c(0.1, 0.4)) +
  geom_vline(xintercept = 24, alpha = 0.5)+
  geom_vline(xintercept = 48, alpha = 0.5)+
  geom_vline(xintercept = 72, alpha = 0.5)+
  scale_color_manual(name   ="Bee",
                     labels = c("QR Worker", "Queen"),
                     values = c("#629CC0", "#7851A9"))

# Individual Variation -------------

setwd("~/Downloads/IBRGData_110123")

Start = 0
for(i in 1:10){
  for(j in 1:35){
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

ggplot(TotalCent, aes(x=Hour, y=Degree, group=ID)) + 
  geom_jitter(aes(color = interaction(QR, Queen), alpha= Queen)) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="Degree in Queens, Queenless Workers, Queenright Workers") + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))

ggplot(TotalCent, aes(x=Hour, y=Closeness, group=ID)) + 
  geom_jitter(aes(color = interaction(QR, Queen), alpha= Queen)) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="Closeness Cent in Queens, Queenless Workers, Queenright Workers") + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))


ggplot(TotalCent, aes(x=Hour, y=Betweenness, group=ID)) + 
  geom_jitter(aes(color = interaction(QR, Queen), alpha= Queen)) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="Betweenness Cent in Queens, Queenless Workers, Queenright Workers") + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))

TotalCentMean <- aggregate(cbind(Degree,Closeness,Betweenness, QR, Queen) ~ ID, TotalCent, mean)

ggplot(TotalCentMean, aes(x=Degree, y=Closeness)) + 
  geom_point(aes(color = interaction(factor(QR), factor(Queen)), alpha= factor(Queen))) + 
  theme_classic() + 
  labs(title="Degree Cent vs Closeness Cent")+
  scale_alpha_discrete(range = c(0.3, 0.8)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))

ggplot(TotalCentMean, aes(x=Degree, y=Betweenness)) + 
  geom_point(aes(color = interaction(factor(QR), factor(Queen)), alpha= factor(Queen))) + 
  theme_classic() + 
  labs(title="Degree Cent vs Betweenness Cent")+
  scale_alpha_discrete(range = c(0.3, 0.8)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))

ggplot(TotalCentMean, aes(x=Betweenness, y=Closeness)) + 
  geom_point(aes(color = interaction(factor(QR), factor(Queen)), alpha= factor(Queen))) + 
  theme_classic() + 
  labs(title="Betweenness Cent vs Closeness Cent")+
  scale_alpha_discrete(range = c(0.3, 0.8)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))


TotalCentMean$Col <- sub("_[^_]+$", "", TotalCentMean$ID)

ggplot(TotalCentMean, aes(x=Degree, y=Closeness)) + 
  geom_point(aes(color = Col)) + 
  theme_classic() + 
  labs(title="Degree Cent vs Closeness Cent, Colored by ID")+
  scale_alpha_discrete(range = c(0.3, 0.8)) 

library(dineq)
gini.wtd(TotalCentMean$Degree)
gini.wtd(TotalCentMean$Closeness)
gini.wtd(TotalCentMean$Betweenness)

TotalCentMeanWorker = TotalCentMean[TotalCentMean$Queen == 0,]

gini.wtd(TotalCentMeanWorker$Degree)
gini.wtd(TotalCentMeanWorker$Closeness)
gini.wtd(TotalCentMeanWorker$Betweenness)

ggplot(TotalCentMeanWorker, aes(x=Degree)) + 
  geom_histogram(color = "#629CC0", fill = "#161414", bins = 20) + 
  scale_x_log10(limits=c(1000,10000)) +
  scale_y_log10() +
  theme_classic() + 
  labs(title="Degree Cent")

ggplot(TotalCentMeanWorker, aes(x=Degree)) + 
  geom_histogram(color = "#629CC0", fill = "#161414", bins = 20) + 
  scale_x_log10(limits=c(1000,10000)) +
  scale_y_log10() +
  theme_classic() + 
  labs(title="Degree Cent Wrapped by Colony")+
  facet_wrap(~ Col, nrow = 2)


ggplot(TotalCentMeanWorker, aes(x=Closeness)) + 
  geom_histogram(color = "#629CC0", fill = "#161414", bins = 20) + 
  theme_classic() + 
  labs(title="Closeness Cent")

ggplot(TotalCentMeanWorker, aes(x=Betweenness)) + 
  geom_histogram(color = "#629CC0", fill = "#161414", bins = 20) + 
  theme_classic() + 
  labs(title="Betweenness Cent")

ggplot(TotalCentMean, aes(x=Closeness, group=Queen)) + 
  geom_histogram(aes(color = factor(Queen)), bins = 10, fill="grey") + 
  theme_classic() + 
  labs(title="Closeness Cent")+
  scale_color_manual(name   ="Bee",
                     labels = c("Worker", "Queen"),
                     values = c("#629CC0", "#7851A9"))+
  facet_wrap(~ Col, nrow = 2)



ggplot(TotalCentMean, aes(x=Betweenness, group=Queen)) + 
  geom_histogram(aes(color = factor(Queen)), bins = 20, fill="grey") + 
  theme_classic() + 
  labs(title="Betweenness Cent")+
  scale_color_manual(name   ="Bee",
                     labels = c("Worker", "Queen"),
                     values = c("#629CC0", "#7851A9")) +
  facet_wrap(~ Col, nrow = 2)



ggplot(TotalCentMean, aes(x=Degree, group=Queen)) + 
  geom_histogram(aes(color = factor(Queen)), bins = 10, fill="grey") + 
  theme_classic() + 
  labs(title="Degree Cent")+
  scale_color_manual(name   ="Bee",
                     labels = c("Worker", "Queen"),
                     values = c("#629CC0", "#7851A9"))+
  facet_wrap(~ Col, nrow = 2)

TotalCentMeanRanked = TotalCentMean %>%
  group_by(Col) %>%
  mutate(Rank = order(order(Degree, decreasing=TRUE)))

ggplot(TotalCentMeanRanked, aes(x=Rank, y=Degree)) + 
  geom_line(aes(color = factor(QR))) + 
  theme_classic() + 
  labs(title="Rank Decay of Degree Centrality")+
  facet_wrap(~ QR + Col, nrow = 2)+
  scale_color_manual(name   ="Colony",
                       labels = c("QL", "QR"),
                       values = c("#161414", "#629CC0"))

ddply(TotalCentMean, .(Col), summarise, corr=(cor.test(Betweenness, Degree, alternative="two.sided", method="kendall")), name=names(corr) )
  

# NWP -------------

setwd("~/Downloads/IBRGData_110123")

Start = 0
for(i in 1:10){
  for(j in 1:34){
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
fm <- lmer(formula = values ~ 1 + QR + Day + QR:Day + (1|Col), data = Eff) #to run the model
#plot(as.numeric(dfQL$values) - as.numeric(dfQR$values), main="MHC", xlab="Hour", ylab="Difference in Average Clustering Coefficient")
#t.test(as.numeric(dfQL$values), as.numeric(dfQR$values), paired = TRUE, alternative = "two.sided")
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
  ylab("Mean Weighted Efficiency")+
  scale_color_manual(name   =" Treatment",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))

Deg = TotalNWP[TotalNWP$params ==  "Sum",]
DegMean <- aggregate(values ~ Hour + QR + Col, Deg, mean)
fm <- lmer(formula = values ~ 1 + QR + Day + QR:Day + (1|Col), data = Eff) #to run the model
#plot(as.numeric(dfQL$values) - as.numeric(dfQR$values), main="MHC", xlab="Hour", ylab="Difference in Average Clustering Coefficient")
#t.test(as.numeric(dfQL$values), as.numeric(dfQR$values), paired = TRUE, alternative = "two.sided")
ggplot(data = DegMean, 
       aes(x   = Hour,
           y   = values, 
           group = Col))+
  geom_line(aes(color=QR),
            size     = 1, 
            alpha    = .7)+
  geom_smooth(aes(color=QR),method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7)+
  theme_classic()+
  ylab("Total Degree")+
  scale_color_manual(name   =" Treatment",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))

Clu = TotalNWP[TotalNWP$params ==  "Average Clustering",]
CluMean <- aggregate(values ~ Hour + QR, Clu, mean)
fm <- lmer(formula = values ~ 1 + QR + Day + QR:Day + (1|Col), data = CluMean) #to run the model
#plot(as.numeric(dfQL$values) - as.numeric(dfQR$values), main="MHC", xlab="Hour", ylab="Difference in Average Clustering Coefficient")
#t.test(as.numeric(dfQL$values), as.numeric(dfQR$values), paired = TRUE, alternative = "two.sided")
ggplot(data = Clu, 
       aes(x   = Hour,
           y   = values, 
           group = as.factor(Col)))+
  geom_line(aes(color=QR),
            size     = 1, 
            alpha    = .7)+
  geom_smooth(aes(group=QR, color=QR),method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7)+
  theme_classic()+
  ylab("Clustering")+
  scale_color_manual(name   =" Treatment",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))
Tra = TotalNWP[TotalNWP$params ==  "Transitivity",]
TraMean <- aggregate(values ~ Hour + QR, Tra, mean)
fm <- lmer(formula = values ~ 1 + QR + Day + QR:Day + (1|Col), data = CluMean) #to run the model
#plot(as.numeric(dfQL$values) - as.numeric(dfQR$values), main="MHC", xlab="Hour", ylab="Difference in Average Clustering Coefficient")
#t.test(as.numeric(dfQL$values), as.numeric(dfQR$values), paired = TRUE, alternative = "two.sided")
ggplot(data = TraMean, 
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
  ylab("Transitivity")+
  scale_color_manual(name   =" Treatment",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))


fm <- lmer(formula = values ~ 1 + QR + Day + Day:QR +(1|Col), data = Eff) #to run the model
fm.null <- lmer(formula = values ~ 1 + Day + (1|Col), data = Eff) #to run the model

fm <- lmer(formula = values ~ 1 + QR + Day + Day:QR +(1|Col), data = Deg) #to run the model
fm.null <- lmer(formula = values ~ 1 + Day + (1|Col), data = Deg) #to run the model

fm <- lmer(formula = values ~ 1 + QR + Day + Day:QR +(1|Col), data = Clu) #to run the model
fm.null <- lmer(formula = values ~ 1 + Day + (1|Col), data = Clu) #to run the model
anova(fm, fm.null)


# Assort -------------

setwd("~/Downloads/IBRGData_110123")

Start = 0
for(i in 1:10){
  for(j in 1:34){
    if(file.exists(paste(prefixes[i],Days[j],"Assort.csv",sep = "_"))) {
      Assort = read.csv(paste(prefixes[i],Days[j],"Assort.csv",sep = "_"))
      Assort$Day = floor((j-1)/9)+1
      Assort$Hour = Days[j]
      Assort$Col = prefixes[i]
      Assort$QR = i %% 2 == 1
      Assort$Mod = "Head_Head"
      if(Start == 1){
        TotalAss = rbind(TotalAss, Assort)
      }
      if(Start == 0){
        TotalAss = Assort
        Start = 1
      }
    }
  }
}

ggplot(data = TotalAss, 
       aes(x   = Hour,
           y   = values, 
           group = as.factor(Col)))+
  geom_line(aes(color=QR),
            size     = 1, 
            alpha    = .7)+
  geom_smooth(aes(group=QR, color=QR),method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7)+
  theme_classic()+
  ylab("Assortativity")+
  scale_color_manual(name   =" Treatment",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))


fm <- lmer(formula = values ~ 1 + QR + Day + Day:QR +(1|Col), data = TotalAss) #to run the model
fm.null <- lmer(formula = values ~ 1 + Day + (1|Col), data = TotalAss) #to run the model
anova(fm, fm.null)
# Directional -------

setwd("~/Downloads/IBRGData_110123")

Start = 0
for(i in 1:10){
  for(j in 1:35){
    if(file.exists(paste(prefixes[i],Days[j],"HHSpeedDirCent.csv",sep = "_"))) {
      DirCent = read.csv(paste(prefixes[i],Days[j],"HHSpeedDirCent.csv",sep = "_"))
      DirCent$Node = sub('\\.0','',DirCent$X)
      DirCent$Day = floor((j-1)/9)+1
      DirCent$Hour = Days[j]
      DirCent$Col = prefixes[i]
      DirCent$QR = i %% 2 == 1
      DirCent$ID = paste(DirCent$Col, DirCent$Node, sep='_')
      DirCent$Queen = DirCent$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalDirCent = rbind(TotalDirCent, DirCent)
      }
      if(Start == 0){
        TotalDirCent = DirCent
        Start = 1
      }
    }
  }
}

TotalDirCent = TotalDirCent[TotalDirCent$InDegree > 50,]
TotalDirCent$Valency = TotalDirCent$InDegree / (TotalDirCent$InDegree + TotalDirCent$OutDegree)

ggplot(TotalDirCent, aes(x=Hour, y=InDegree, group=ID)) + 
  geom_jitter(aes(color = interaction(QR, Queen), alpha= Queen)) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="InDegree in Queens, Queenless Workers, Queenright Workers") + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))

ggplot(TotalDirCent, aes(x=Hour, y=OutDegree, group=ID)) + 
  geom_jitter(aes(color = interaction(QR, Queen), alpha= Queen)) + 
  geom_smooth(aes(group=interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() + 
  labs(title="OutDegree in Queens, Queenless Workers, Queenright Workers") + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QL Worker", "QR Worker", "Queen"),
                     values = c("#161414", "#629CC0", "#7851A9"))

TotalDirCentQR = TotalDirCent[TotalDirCent$QR == TRUE,]
ggplot(TotalDirCentQR, aes(x=Hour, y=Valency, group=ID)) + 
  geom_jitter(aes(color = Queen, alpha= Queen)) + 
  geom_smooth(aes(group= Queen, color = Queen)) +
  theme_classic() + 
  labs(title="Initiation in Queens and Workers",  y="Initiation") + 
  scale_alpha_discrete(range = c(0.1, 0.4)) + 
  scale_color_manual(name   ="Bee",
                     labels = c("QR Worker", "Queen"),
                     values = c("#629CC0", "#7851A9"))

fm <- lmer(formula = Valency ~ 1 + Queen + Day + Day:Queen +(1|Col), data = TotalDirCentQR) #to run the model
fm.null <- lmer(formula = Valency ~ 1 + Day + (1|Col), data = TotalDirCentQR) #to run the model
anova(fm, fm.null)


TotalDirCentMean <- aggregate(cbind(InDegree,OutDegree,Closeness,Betweenness, Valency,QR, Queen) ~ ID+Col, TotalDirCent, mean)
TotalDirCentRanked = TotalDirCentMean %>%
  group_by(TotalDirCentRanked$Col) %>%
  mutate(Rank = order(order(Valency, decreasing=TRUE)))

ggplot(TotalDirCentRanked, aes(x=Rank, y=Valency)) + 
  geom_line(aes(color = factor(QR))) + 
  theme_classic() + 
  labs(title="Rank Decay of Initiation", y="Initiation")+
  facet_wrap(~ QR + Col, nrow = 2)+
  scale_color_manual(name   ="Colony",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))

TotalDirCentMean$Sum = TotalDirCentMean$InDegree+TotalDirCentMean$OutDegree

ggplot(TotalDirCentMean, aes(x=Valency, y=Sum)) + 
  geom_point() + 
  xlim(0,1) + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(QR)))+
  theme_classic() + 
  labs(title="Initiation vs TotalDegree", x="Initiation")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ QR+Col, nrow = 2)


# DirNWP--------
setwd("~/Downloads/IBRGData_110123")

Start = 0
for(i in 1:10){
  for(j in 1:34){
    if(file.exists(paste(prefixes[i],Days[j],"HHSpeedDirNWP.csv",sep = "_"))) {
      DirNWP = read.csv(paste(prefixes[i],Days[j],"HHSpeedDirNWP.csv",sep = "_"))
      DirNWP$Day = floor((j-1)/9)+1
      DirNWP$Hour = Days[j]
      DirNWP$Col = prefixes[i]
      DirNWP$QR = i %% 2 == 1
      DirNWP$Mod = "Head_Head"
      if(Start == 1){
        TotalDirNWP = rbind(TotalDirNWP, DirNWP)
      }
      if(Start == 0){
        TotalDirNWP = DirNWP
        Start = 1
      }
    }
  }
}

DirEff = TotalDirNWP[TotalDirNWP$params ==  "GlobalEfficiency",]
DirEffMean <- aggregate(values ~ Hour + QR, DirEff, mean)
fm <- lmer(formula = values ~ 1 + QR + Day + QR:Day + (1|Col), data = DirEff) #to run the model
#plot(as.numeric(dfQL$values) - as.numeric(dfQR$values), main="MHC", xlab="Hour", ylab="Difference in Average Clustering Coefficient")
#t.test(as.numeric(dfQL$values), as.numeric(dfQR$values), paired = TRUE, alternative = "two.sided")
ggplot(data = DirEffMean, 
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
  ylab("Mean Weighted Efficiency")+
  scale_color_manual(name   =" Treatment",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))

DirDeg = TotalNWP[TotalNWP$params ==  "Sum",]
DirDegMean <- aggregate(values ~ Hour + QR + Col, DirDeg, mean)
fm <- lmer(formula = values ~ 1 + QR + Day + QR:Day + (1|Col), data = DirDeg) #to run the model
#plot(as.numeric(dfQL$values) - as.numeric(dfQR$values), main="MHC", xlab="Hour", ylab="Difference in Average Clustering Coefficient")
#t.test(as.numeric(dfQL$values), as.numeric(dfQR$values), paired = TRUE, alternative = "two.sided")
ggplot(data = DirDegMean, 
       aes(x   = Hour,
           y   = values, 
           group = Col))+
  geom_line(aes(color=QR),
            size     = 1, 
            alpha    = .7)+
  geom_smooth(aes(color=QR),method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7)+
  theme_classic()+
  ylab("Total Degree")+
  scale_color_manual(name   =" Treatment",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))


# Key Time Points--------
DegVel <- HeadDeg %>% right_join(TotalVel, by=c("ID", "Hour","Day","QR"))
DegVelMeanDay <- aggregate(cbind(move_perc,mean_vel,Degree,QR) ~ ID+Day, DegVel, mean)
DegVelMeanDay$Col <- sub("_[^_]+$", "", DegVelMeanDay$ID)


DegVelMeanDayAL <- DegVelMeanDay[DegVelMeanDay$Col=="20221123_1543_AmericanoLatte_QL",]

ggplot(DegVelMeanDayAL, aes(x=move_perc, y=Degree)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              color    = "black")+
  theme_classic() + 
  labs(title="Degree vs Movement Speed by Day (Americano)")+
  facet_wrap(~ Day, nrow = 1)

DegVelMeanDayRT <- DegVelMeanDay[DegVelMeanDay$Col=="RooibosTea_QL_1216_1646",]

ggplot(DegVelMeanDayRT, aes(x=move_perc, y=Degree)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              color    = "black")+
  theme_classic() + 
  labs(title="Degree vs Movement Speed by Day (Rooibos)")+
  facet_wrap(~ Day, nrow = 1)

# Top10 and Ovaries -----------
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

Ovaries = read.csv('OvaryMeasurements.csv')
Ovaries$AverageLength = (Ovaries$LongestOocyteLength1..mm. + Ovaries$LongestOocyteLength2..mm.) / 2
Ovaries$AverageWidth = (Ovaries$LongestOocyteWidth1..mm. + Ovaries$LongestOocyteWidth2..mm.) / 2

ggplot(Ovaries, aes(x=AverageLength)) + 
  geom_histogram(aes(color = factor(Treatment)))+
  theme_classic() + 
  labs(title="Average Length of Longest Oocyte")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ Treatment, nrow = 2)


ggplot(Ovaries, aes(x=AverageWidth)) + 
  geom_histogram(aes(color = factor(Treatment)))+
  theme_classic() + 
  labs(title="Average Width of Longest Oocyte")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ Treatment, nrow = 2)

ggplot(Ovaries, aes(x=AverageWidth)) + 
  geom_boxplot(aes(color = factor(Treatment)))+
  theme_classic(base_size=16) + 
  labs(title="Average Width of Longest Oocyte")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ Treatment, nrow = 2)


ggplot(Ovaries, aes(x=AverageLength, y=AverageWidth)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(Treatment)))+
  theme_classic() + 
  labs(title="Avg Length vs Avg Width")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ Treatment, nrow = 2)

Ovaries$ID = paste("RooibosTea_",Ovaries$Treatment,"_1216_1646_ArUcoTag#",Ovaries$Tag,sep="")
DegVelMeanRT = DegVelMean[DegVelMean$Col == 'RooibosTea_QR_1216_1646' | DegVelMean$Col == 'RooibosTea_QL_1216_1646',]
DegOvMeanRT = merge(DegVelMeanRT, Ovaries, by='ID')
DegOvMeanRT$Queen = DegOvMeanRT$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")


ggplot(DegOvMeanRT, aes(x=AverageWidth, y=Degree)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(Treatment)))+
  theme_classic(base_size = 16) + 
  labs(title="RooTea: Degree vs Avg Width")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ Treatment, nrow = 2)

ddply(DegOvMeanRT, .(Col), summarise,
      corr=(cor.test(AverageWidth, Degree,
                     alternative="two.sided", method="spearman")), name=names(corr) )

ggplot(DegOvMeanRT, aes(x=AverageWidth, y=move_perc)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(Treatment)))+
  theme_classic() + 
  labs(title="Velocity vs Avg Width")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ Treatment, nrow = 2)

ddply(DegOvMeanRT, .(Col), summarise,
      corr=(cor.test(AverageWidth, move_perc,
                     alternative="two.sided", method="spearman")), name=names(corr) )

ggplot(DegOvMeanRT, aes(x=AverageWidth, y=mean_vel)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(Treatment)))+
  theme_classic() + 
  labs(title="Velocity vs Avg Width")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ Treatment, nrow = 2)

HBVelMeanRT = HBVelMean[HBVelMean$Col == 'RooibosTea_QR_1216_1646' | HBVelMean$Col == 'RooibosTea_QL_1216_1646',]
HBOvMeanRT = merge(HBVelMeanRT, Ovaries, by='ID')
HBOvMeanRT$Queen = HBOvMeanRT$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")

ggplot(HBOvMeanRT, aes(x=AverageWidth, y=Sum)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(Treatment)))+
  theme_classic() + 
  labs(title="RooTea: Degree vs Avg Width, Head-Body")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ Treatment, nrow = 2)

ggplot(HBOvMeanRT, aes(x=AverageWidth, y=InDegree)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(Treatment)))+
  theme_classic() + 
  labs(title="InDegree vs Avg Width, Head-Body")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ Treatment, nrow = 2)

ggplot(HBOvMeanRT, aes(x=AverageWidth, y=OutDegree)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(Treatment)))+
  theme_classic() + 
  labs(title="OutDegree vs Avg Width, Head-Body")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ Treatment, nrow = 2)


ddply(HBOvMeanRT, .(Col), summarise,
      corr=(cor.test(AverageWidth, Sum,
                     alternative="two.sided", method="spearman")), name=names(corr) )


OvariesAM = Ovaries[Ovaries$Colony=="ArgentinanMate",]
OvariesAM$ID = paste("20221209_1613_",OvariesAM$Treatment,"_ArUcoTag#",OvariesAM$Tag,sep="")

DegVelMeanAM = DegVelMean[DegVelMean$Col == '20221209_1613_QR' | DegVelMean$Col == '20221209_1613_QL',]
DegOvMeanAM = merge(DegVelMeanAM, OvariesAM, by='ID')
DegOvMeanAM$Queen = DegOvMeanAM$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")

ggplot(DegOvMeanAM, aes(x=AverageWidth, y=Degree)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(Treatment)))+
  theme_classic() + 
  labs(title="ArgMat: Degree vs Avg Width")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ Treatment, nrow = 2)

ddply(DegOvMeanAM, .(Col), summarise,
      corr=(cor.test(AverageWidth, Degree,
                     alternative="two.sided", method="spearman")), name=names(corr) )


OvariesAG = Ovaries[Ovaries$Colony=="AlmdudlerGspritzt",]
OvariesAG$ID = paste("20230213_1745_AlmdudlerGspritzt_",OvariesAG$Treatment,"_ArUcoTag#",OvariesAG$Tag,sep="")

DegVelMeanAG = DegVelMean[DegVelMean$Col == '20230213_1745_AlmdudlerGspritzt_C0' | DegVelMean$Col == '20230213_1745_AlmdudlerGspritzt_C1',]
DegOvMeanAG = merge(DegVelMeanAG, OvariesAG, by='ID')
DegOvMeanAG$Queen = DegOvMeanAG$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
DegOvMeanAGWorker  = DegOvMeanAG[DegOvMeanAG$Queen == FALSE,]
ggplot(DegOvMeanAGWorker, aes(x=AverageWidth, y=Degree)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(Treatment)))+
  theme_classic() + 
  labs(title="AlmGsp: Degree vs Avg Width")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ Treatment, nrow = 2)

ddply(DegOvMeanAGWorker, .(Col), summarise,
      corr=(cor.test(AverageWidth, Degree,
                     alternative="two.sided", method="spearman")), name=names(corr) )

OvariesAL = Ovaries[Ovaries$Colony=="AmericanoLatte",]
OvariesAL$ID = paste("20221123_1543_AmericanoLatte_",OvariesAL$Treatment,"_ArUcoTag#",OvariesAL$Tag,sep="")

DegVelMeanAL = DegVelMean[DegVelMean$Col == '20221123_1543_AmericanoLatte_QL' | DegVelMean$Col == '20221123_1543_AmericanoLatte_QR',]
DegOvMeanAL = merge(DegVelMeanAL, OvariesAL, by='ID')
DegOvMeanAL$Queen = DegOvMeanAL$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
DegOvMeanALWorker  = DegOvMeanAL[DegOvMeanAL$Queen == FALSE,]
ggplot(DegOvMeanALWorker, aes(x=AverageWidth, y=Degree)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(Treatment)))+
  theme_classic() + 
  labs(title="AmLat: Degree vs Avg Width")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ Treatment, nrow = 2)

ddply(DegOvMeanALWorker, .(Col), summarise,
      corr=(cor.test(AverageWidth, Degree,
                     alternative="two.sided", method="spearman")), name=names(corr) )

OvariesMHC = Ovaries[Ovaries$Colony=="MexicanHotChocolate",]
OvariesMHC$ID = paste("MexHotChoc_",OvariesMHC$Treatment,"_1216_1646_ArUcoTag#",OvariesMHC$Tag,sep="")

DegVelMeanMHC = DegVelMean[DegVelMean$Col == 'MexHotChoc_QL_1216_1646' | DegVelMean$Col == 'MexHotChoc_QR_1216_1646',]
DegOvMeanMHC = merge(DegVelMeanMHC, OvariesMHC, by='ID')
DegOvMeanMHC$Queen = DegOvMeanMHC$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
DegOvMeanMHCWorker  = DegOvMeanMHC[DegOvMeanMHC$Queen == FALSE,]
ggplot(DegOvMeanMHCWorker, aes(x=AverageWidth, y=Degree)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(Treatment)))+
  theme_classic() + 
  labs(title="MHC: Degree vs Avg Width")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ Treatment, nrow = 2)

ddply(DegOvMeanMHCWorker, .(Col), summarise,
      corr=(cor.test(AverageWidth, Degree,
                     alternative="two.sided", method="spearman")), name=names(corr) )
# DispersionVsDegree -----
TotalDisp=TotalDisp
TotalDisp$Day <- as.factor(sub("Day", "", TotalDisp$Day))
DegVelMeanDay$Day = as.factor(DegVelMeanDay$Day)
DegVelDisp <- TotalDisp %>% right_join(DegVelMeanDay, by=c("ID", "Day","QR"))

ggplot(DegVelDisp, aes(x=N90, y=Degree)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(QR)))+
  theme_classic() + 
  labs(title="Dispersion vs Degree")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ QR+ Col.x, nrow = 2)

ggplot(DegVelDisp, aes(x=N90, y=move_perc)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(QR)))+
  theme_classic() + 
  labs(title="Dispersion vs Movement Percentage")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ QR+ Col.x, nrow = 2)

ggplot(DegVelDisp, aes(x=N90, y=mean_vel)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(QR)))+
  theme_classic() + 
  labs(title="Dispersion vs Movement Velocity")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ QR+ Col.x, nrow = 2)


# BodyBody ---------

Start = 0
for(i in 1:10){
  for(j in 1:35){
    if(file.exists(paste(prefixes[i],Days[j],"HHSpeedDirCent.csv",sep = "_"))) {
      DirCent = read.csv(paste(prefixes[i],Days[j],"HHSpeedDirCent.csv",sep = "_"))
      DirCent = select(DirCent, -4:-5)
      DirCent$Node = sub('\\.0','',DirCent$X)
      DirCent$Day = floor((j-1)/9)+1
      DirCent$Hour = Days[j]
      DirCent$Col = prefixes[i]
      DirCent$QR = i %% 2 == 1
      DirCent$ID = paste(DirCent$Col, DirCent$Node, sep='_')
      DirCent$Mod = "Head_Head"
      DirCent$Queen = DirCent$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalDeg3Mod = rbind(TotalDeg3Mod, DirCent)
      }
      if(Start == 0){
        TotalDeg3Mod = DirCent
        Start = 1
      }
    }
  }
}
for(i in 1:10){
  for(j in 1:35){
    if(file.exists(paste(prefixes[i],Days[j],"HBSpeedDirCent.csv",sep = "_"))) {
      Deg = read.csv(paste(prefixes[i],Days[j],"HBSpeedDirCent.csv",sep = "_"))
      Deg = select(Deg, -4:-5)
      Deg$Node = sub('\\.0','',Deg$X)
      Deg$Day = floor((j-1)/9)+1
      Deg$Hour = Days[j]
      Deg$Col = prefixes[i]
      Deg$QR = i %% 2 == 1
      Deg$ID = paste(Deg$Col, Deg$Node, sep='_')
      Deg$Mod = "Head_Body"
      Deg$Queen = Deg$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalDeg3Mod = rbind(TotalDeg3Mod, Deg)
      }
      if(Start == 0){
        TotalDeg3Mod = Deg
        Start = 1
      }
    }
  }
}


for(i in 1:10){
  for(j in 1:35){
    if(file.exists(paste(prefixes[i],Days[j],"DegBodyBody.csv",sep = "_"))) {
      Deg = read.csv(paste(prefixes[i],Days[j],"DegBodyBody.csv",sep = "_"))
      Deg$InDegree = Deg$Degree/2
      Deg$OutDegree = Deg$Degree/2
      Deg = select(Deg, -3)
      Deg$Node = sub('\\.0','',Deg$Node)
      Deg$Day = floor((j-1)/9)+1
      Deg$Hour = Days[j]
      Deg$Col = prefixes[i]
      Deg$QR = i %% 2 == 1
      Deg$ID = paste(Deg$Col, Deg$Node, sep='_')
      Deg$Mod = "Body_Body"
      Deg$Trial = str_extract(Deg$Col, ".+?(?=_)")
      Deg$Queen = Deg$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalDeg3Mod = rbind(TotalDeg3Mod, Deg)
      }
      if(Start == 0){
        TotalDeg3Mod = Deg
        Start = 1
      }
    }
  }
}

QRDeg = TotalDeg3Mod[TotalDeg3Mod$QR == TRUE,]
ggplot(QRDeg, aes(x=Hour, y=Degree, group=interaction(Mod,Queen))) + 
  #geom_line(aes(group=Mod, color = Mod), alpha=0.1) + 
  geom_smooth(aes(group=interaction(Mod,Queen), color = Mod, linetype=Queen)) + 
  theme_classic() + 
  labs(title="QR Worker Degree Head_Head vs Head_Body Unstandardized") +
  scale_color_manual(labels = c("Body_Body", "Head_Body", "Head_Head"), values=c("#000000", "#4a6741", "#EECA58"))

ModalityMeans <- aggregate(cbind(Degree) ~ Mod + Queen + QR, TotalDeg3Mod, mean)

ggplot(TotalDeg3Mod, aes(fill=Mod, y=Degree, x=interaction(Queen,QR))) +
  geom_bar(position='dodge', stat='identity') + 
  theme_classic() + 
  labs(title="QR Worker Degree Head_Head vs Head_Body Unstandardized", x="Bee") +
  scale_x_discrete(labels=c("QL Worker", "QR Worker", "Queen"))+
  scale_fill_manual(labels = c("Body_Body", "Head_Body", "Head_Head"), values=c("#000000", "#4a6741", "#EECA58"))


# HeadBodyDirectionality-----
setwd("~/Downloads/IBRGData_110123")


Start = 0
for(i in 1:10){
  for(j in 1:35){
    if(file.exists(paste(prefixes[i],Days[j],"HBBodyDirCent.csv",sep = "_"))) {
      HeadButt = read.csv(paste(prefixes[i],Days[j],"HBBodyDirCent.csv",sep = "_"))
      HeadButt$Node = sub('\\.0','',HeadButt$X)
      HeadButt$Day = floor((j-1)/9)+1
      HeadButt$Hour = Days[j]
      HeadButt$Col = prefixes[i]
      HeadButt$QR = i %% 2 == 1
      HeadButt$ID = paste(HeadButt$Col, HeadButt$Node, sep='_')
      HeadButt$Queen = HeadButt$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalHeadButt = rbind(TotalHeadButt, HeadButt)
      }
      if(Start == 0){
        TotalHeadButt = HeadButt
        Start = 1
      }
    }
  }
}

TotalHeadButt = TotalHeadButt[TotalHeadButt$OutDegree > 50,]
TotalHeadButt$Valency = TotalHeadButt$OutDegree / (TotalHeadButt$InDegree + TotalHeadButt$OutDegree)


TotalHeadButtMean <- aggregate(cbind(InDegree,OutDegree,Closeness,Betweenness, Valency,QR, Queen) ~ ID+Col, TotalHeadButt, mean)
TotalHeadButtMean = TotalHeadButtMean[!grepl("#82", TotalHeadButtMean$ID),]
TotalHeadButtMean = TotalHeadButtMean[!grepl("#88", TotalHeadButtMean$ID),]
TotalHeadButtMean = TotalHeadButtMean[!grepl("#84", TotalHeadButtMean$ID),]
TotalHeadButtMean$Col <- sub("_[^_]+$", "", TotalHeadButtMean$ID)

VelMean <- aggregate(cbind(move_perc,mean_vel) ~ ID, TotalVel, mean)

HBVelMean <- TotalHeadButtMean %>% right_join(VelMean, by=c("ID"))
HBVelMean <- HBVelMean %>% drop_na()

ggplot(HBVelMean, aes(x=Valency, y=move_perc)) + 
  geom_point(aes(shape    = factor(Queen))) + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(QR)))+
  theme_classic() + 
  labs(title="Initiation vs Movement Percentage")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ QR+ Col, nrow = 2)


ggplot(HBVelMean, aes(x=Valency, y=mean_vel)) + 
  geom_point(aes(shape    = factor(Queen))) + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(QR)))+
  theme_classic() + 
  labs(title="Initiation vs Movement Speed")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ QR+ Col, nrow = 2)
ddply(HBVelMean, .(Col), summarise,
      corr=(cor.test(Valency, mean_vel,
                     alternative="two.sided", method="kendall")), name=names(corr) )

HBVelMean$Sum = HBVelMean$InDegree + HBVelMean$OutDegree

ggplot(HBVelMean, aes(x=Valency, y=Sum)) + 
  geom_point(aes(shape    = factor(Queen))) + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(QR)))+
  theme_classic() + 
  labs(title="Initiation vs Degree")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ QR+ Col, nrow = 2)

HBVelWorkers = HBVelMean[HBVelMean$Queen == 0,]
ggplot(HBVelWorkers, aes(x=Valency, y=Sum)) + 
  geom_point(aes(shape    = factor(Queen))) + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              aes(color    = factor(QR)))+
  theme_classic() + 
  labs(title="Initiation vs Degree")+
  scale_color_manual(name   ="Bee",
                     labels = c("QL", "QR"),
                     values = c("#161414", "#629CC0"))+
  facet_wrap(~ QR+ Col, nrow = 2)


ddply(HBVelMean, .(Col), summarise,
      corr=(cor.test(MethodSpeed, Sum,
                     alternative="two.sided", method="kendall")), name=names(corr) )

HBMeanDay <- aggregate(cbind(InDegree,OutDegree,Closeness,Betweenness, Valency,QR, Queen) ~ ID+Col+Day, TotalHeadButt, mean)
HBMeanDay$Sum = HBMeanDay$InDegree + HBMeanDay$OutDegree

HBMeanDayAL <- HBMeanDay[HBMeanDay$Col=="20221123_1543_AmericanoLatte_QL",]

ggplot(HBMeanDayAL, aes(x=Sum, y=Valency)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              color    = "black")+
  theme_classic() + 
  labs(title="Degree vs Movement Speed by Day (Americano)")+
  facet_wrap(~ Day, nrow = 1)

HBMeanDayRT <- HBMeanDay[HBMeanDay$Col=="RooibosTea_QL_1216_1646",]

ggplot(HBMeanDayRT, aes(x=Sum, y=Valency)) + 
  geom_point() + 
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7,
              color    = "black")+
  theme_classic() + 
  labs(title="Degree vs Movement Speed by Day (Rooibos)")+
  facet_wrap(~ Day, nrow = 1)


