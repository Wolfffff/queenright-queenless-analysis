library(lme4)
library(tidyverse)
library(dplyr)
library(stringr)
library("wesanderson")
library(ggnewscale)
library('corrr')
library('ggcorrplot')
library("FactoMineR")
library("factoextra")

setwd("~/Downloads/QueenModelComplete_062424/")
prefixes = c("RooibosTea_QR_1216_1646", "RooibosTea_QL_1216_1646", "MexHotChoc_QR_1216_1646", "MexHotChoc_QL_1216_1646", "20230213_1745_AlmdudlerGspritzt_C1", "20230213_1745_AlmdudlerGspritzt_C0", "20221209_1613_QR", "20221209_1613_QL", "20221123_1543_AmericanoLatte_QR", "20221123_1543_AmericanoLatte_QL")
Day = 1
Days <- list()
Start = 0
#Total Degree
for(i in 1:10){
  for(j in 0:95){
    if(file.exists(paste(prefixes[i],sprintf("%03d", j),"Head_to_Head_False_Centralities.csv",sep = "_"))) {
      Deg = read.csv(paste(prefixes[i],sprintf("%03d", j),"Head_to_Head_False_Centralities.csv",sep = "_"))
      Deg$Node = sub('\\.0','',Deg$X)
      Deg$ExpDay = as.factor(floor((j/24)+1))
      Deg$Day = as.factor(floor(((j+17)/24)+1))
      Deg$Zeit = as.numeric((j+17)%%24)
      Deg$Hour = j
      Deg$Col = prefixes[i]
      Deg$QR = i %% 2 == 1
      Deg$ID = paste(Deg$Col, Deg$Node, Deg$Hour, sep='_')
      Deg$Bee = paste(Deg$Col, Deg$Node, sep='_')
      Deg$Mod = "Head_Head"
      Deg$Trial = str_extract(Deg$Col, ".+?(?=_)")
      Deg$Queen = Deg$Bee %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalDeg = rbind(TotalDeg, Deg)
      }
      if(Start == 0){
        TotalDeg = Deg
        Start = 1
      }
    }
    else{
      print("HeadCent")
      print(prefixes[i])
      print(j)
    }
  }
}
TotalDeg <- TotalDeg %>%
  mutate(degree = ifelse(Trial == "MexHotChoc", degree * 1.428, degree))
TotalDeg <- TotalDeg %>%
  mutate(close = ifelse(Trial == "MexHotChoc", close * 1.428, close))
names(TotalDeg)[names(TotalDeg) == "degree"] <- "Degree"
names(TotalDeg)[names(TotalDeg) == "between"] <- "Between"
names(TotalDeg)[names(TotalDeg) == "close"] <- "Close"
names(TotalDeg)[names(TotalDeg) == "eigen"] <- "Eigen"

#Bout Degree

Start = 0
for(i in 1:10){
  for(j in 0:95){
    if(file.exists(paste(prefixes[i],sprintf("%03d", j),"Head_to_Head_True_Centralities.csv",sep = "_"))) {
      Deg = read.csv(paste(prefixes[i],sprintf("%03d", j),"Head_to_Head_True_Centralities.csv",sep = "_"))
      Deg$Node = sub('\\.0','',Deg$X)
      Deg$ExpDay = as.factor(floor((j/24)+1))
      Deg$Day = as.factor(floor(((j+17)/24)+1))
      Deg$Zeit = as.numeric((j+17)%%24)
      Deg$Hour = j
      Deg$Col = prefixes[i]
      Deg$QR = i %% 2 == 1
      Deg$ID = paste(Deg$Col, Deg$Node, Deg$Hour, sep='_')
      Deg$Bee = paste(Deg$Col, Deg$Node, sep='_')
      Deg$Mod = "Head_Head"
      Deg$Trial = str_extract(Deg$Col, ".+?(?=_)")
      Deg$Queen = Deg$Bee %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalBout = rbind(TotalBout, Deg)
      }
      if(Start == 0){
        TotalBout = Deg
        Start = 1
      }
    }
    else{
      print("BoutCent")
      print(prefixes[i])
      print(j)
    }
  }
}

TotalBout <- TotalBout %>%
  mutate(degree = ifelse(Trial == "MexHotChoc", degree * 1.428, degree))
TotalBout <- TotalBout %>%
  mutate(close = ifelse(Trial == "MexHotChoc", close * 1.428, close))
TotalBoutSubset <- TotalBout[, c("ID", "degree", "between", "close", "eigen")]
names(TotalBoutSubset)[names(TotalBoutSubset) == "degree"] <- "boutDegree"
names(TotalBoutSubset)[names(TotalBoutSubset) == "between"] <- "boutBetween"
names(TotalBoutSubset)[names(TotalBoutSubset) == "close"] <- "boutClose"
names(TotalBoutSubset)[names(TotalBoutSubset) == "eigen"] <- "boutEigen"
BigDataSheet <- merge(TotalDeg, TotalBoutSubset, by = "ID")

#Body Degree

Start = 0
for(i in 1:10){
  for(j in 0:95){
    if(file.exists(paste(prefixes[i],sprintf("%03d", j),"Head_to_Body_False_Centralities.csv",sep = "_"))) {
      Deg = read.csv(paste(prefixes[i],sprintf("%03d", j),"Head_to_Body_False_Centralities.csv",sep = "_"))
      Deg$Node = sub('\\.0','',Deg$X)
      Deg$ExpDay = as.factor(floor((j/24)+1))
      Deg$Day = as.factor(floor(((j+17)/24)+1))
      Deg$Zeit = as.numeric((j+17)%%24)
      Deg$Hour = j
      Deg$Col = prefixes[i]
      Deg$QR = i %% 2 == 1
      Deg$ID = paste(Deg$Col, Deg$Node, Deg$Hour, sep='_')
      Deg$Bee = paste(Deg$Col, Deg$Node, sep='_')
      Deg$Mod = "Head_Head"
      Deg$Trial = str_extract(Deg$Col, ".+?(?=_)")
      Deg$Queen = Deg$Bee %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalBody = rbind(TotalBody, Deg)
      }
      if(Start == 0){
        TotalBody = Deg
        Start = 1
      }
    }
    else{
      print("BodyCent")
      print(prefixes[i])
      print(j)
    }
  }
}

TotalBody <- TotalBody %>%
  mutate(degree = ifelse(Trial == "MexHotChoc", degree * 1.428, degree))
TotalBody <- TotalBody %>%
  mutate(close = ifelse(Trial == "MexHotChoc", close * 1.428, close))
TotalBodySubset <- TotalBody[, c("ID", "degree", "between", "close", "eigen")]
names(TotalBodySubset)[names(TotalBodySubset) == "degree"] <- "bodyDegree"
names(TotalBodySubset)[names(TotalBodySubset) == "between"] <- "bodyBetween"
names(TotalBodySubset)[names(TotalBodySubset) == "close"] <- "bodyClose"
names(TotalBodySubset)[names(TotalBodySubset) == "eigen"] <- "bodyEigen"

BigDataSheet <- merge(BigDataSheet, TotalBodySubset, by = "ID")

#Bout Length
BigDataSheet$AverageBoutLength = BigDataSheet$Degree / BigDataSheet$boutDegree


#Ovary Measurements
Ovaries = read.csv('OvaryMeasurements.csv')
Ovaries$AverageOvaryLength = (Ovaries$LongestOocyteLength1..mm. + Ovaries$LongestOocyteLength2..mm.) / 2
Ovaries$AverageOvaryWidth = (Ovaries$LongestOocyteWidth1..mm. + Ovaries$LongestOocyteWidth2..mm.) / 2
Ovaries$Bee = paste(Ovaries$ColonyID, '_ArUcoTag#', Ovaries$Tag, sep='')
BigDataSheet <- merge(BigDataSheet, Ovaries[, c("Bee", "AverageOvaryLength", "AverageOvaryWidth")], by = "Bee", all.x = TRUE)

#Wing Measurements
Wings = read.csv('WingMeasurements.csv')
Wings$AverageWingLength = (Wings$wing_1_mm + Wings$wing_2_mm) / 2
Wings$TagNumber = str_extract(Wings$ID, "(?<=Tag)\\d+")
Wings$Bee = paste(Wings$ColonyID, '_ArUcoTag#', Wings$TagNumber, sep='')
BigDataSheet <- merge(BigDataSheet, Wings[, c("Bee", "AverageWingLength")], by = "Bee", all.x = TRUE)

#Track Presence
Start=0
for(i in 1:10){
  for(j in 0:95){
    if(file.exists(paste(prefixes[i],sprintf("%03d", j),"8_8_3_1_10_interp_filtered.h5_TrackPresence.csv",sep = "_"))) {
      Deg = read.csv(paste(prefixes[i],sprintf("%03d", j),"8_8_3_1_10_interp_filtered.h5_TrackPresence.csv",sep = "_"))
      Deg$track = sub('\\.0','',Deg$track)
      Deg$ExpDay = as.factor(floor((j/24)+1))
      Deg$Day = as.factor(floor(((j+17)/24)+1))
      Deg$Zeit = as.numeric((j+17)%%24)
      Deg$Hour = j
      Deg$Col = prefixes[i]
      Deg$QR = i %% 2 == 1
      Deg$ID = paste(Deg$Col, Deg$track, Deg$Hour, sep='_')
      Deg$Bee = paste(Deg$Col, Deg$track, sep='_')
      Deg$Mod = "Head_Head"
      Deg$Trial = str_extract(Deg$Col, ".+?(?=_)")
      Deg$Queen = Deg$Bee %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalPresence = rbind(TotalPresence, Deg)
      }
      if(Start == 0){
        TotalPresence = Deg
        Start = 1
      }
    }
    else{
      print("TrackPres")
      print(prefixes[i])
      print(j)
    }
  }
}

TotalPresenceSubset <- TotalPresence[, c("ID", "Presence")]
BigDataSheet <- merge(BigDataSheet, TotalPresenceSubset, by = "ID")

#Antennna Presence
Start=0
for(i in 1:10){
  for(j in 0:95){
    if(file.exists(paste(prefixes[i],sprintf("%03d", j),"8_8_3_1_10_interp_filtered.h5_TrackPresenceAnt.csv",sep = "_"))) {
      Deg = read.csv(paste(prefixes[i],sprintf("%03d", j),"8_8_3_1_10_interp_filtered.h5_TrackPresenceAnt.csv",sep = "_"))
      Deg$track = sub('\\.0','',Deg$track)
      Deg$ExpDay = as.factor(floor((j/24)+1))
      Deg$Day = as.factor(floor(((j+17)/24)+1))
      Deg$Zeit = as.numeric((j+17)%%24)
      Deg$Hour = j
      Deg$Col = prefixes[i]
      Deg$QR = i %% 2 == 1
      Deg$ID = paste(Deg$Col, Deg$track, Deg$Hour, sep='_')
      Deg$Bee = paste(Deg$Col, Deg$track, sep='_')
      Deg$Mod = "Head_Head"
      Deg$Trial = str_extract(Deg$Col, ".+?(?=_)")
      Deg$Queen = Deg$Bee %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalPresence = rbind(TotalPresence, Deg)
      }
      if(Start == 0){
        TotalPresence = Deg
        Start = 1
      }
    }
    else{
      print("AntPres")
      print(prefixes[i])
      print(j)
    }
  }
}

TotalPresenceSubset <- TotalPresence[, c("ID", "Presence")]
names(TotalPresenceSubset)[names(TotalPresenceSubset) == "Presence"] <- "AntPresence"
BigDataSheet <- merge(BigDataSheet, TotalPresenceSubset, by = "ID")

#Vels Data
Start=0
for(i in 1:10){
  for(j in 0:95){
    if(file.exists(paste(prefixes[i],sprintf("%03d", j),"velstrackqueenstandard.csv",sep = "_")) | file.exists(paste(prefixes[i],sprintf("%03d", j),"velstrackqueen.csv",sep = "_"))){
      if(file.exists(paste(prefixes[i],sprintf("%03d", j),"velstrackqueenstandard.csv",sep = "_"))){
        Deg = read.csv(paste(prefixes[i],sprintf("%03d", j),"velstrackqueenstandard.csv",sep = "_"))
      }
      if(file.exists(paste(prefixes[i],sprintf("%03d", j),"velstrackqueen.csv",sep = "_"))){
        Deg = read.csv(paste(prefixes[i],sprintf("%03d", j),"velstrackqueen.csv",sep = "_"))
      }
      Deg$X = sub('\\.0','',Deg$X)
      Deg$ExpDay = as.factor(floor((j/24)+1))
      Deg$Day = as.factor(floor(((j+17)/24)+1))
      Deg$Zeit = as.numeric((j+17)%%24)
      Deg$Hour = j
      Deg$Col = prefixes[i]
      Deg$QR = i %% 2 == 1
      Deg$ID = paste(Deg$Col, Deg$X, Deg$Hour, sep='_')
      Deg$Bee = paste(Deg$Col, Deg$X, sep='_')
      Deg$Mod = "Head_Head"
      Deg$Trial = str_extract(Deg$Col, ".+?(?=_)")
      Deg$Queen = Deg$Bee %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalVels = rbind(TotalVels, Deg)
      }
      if(Start == 0){
        TotalVels = Deg
        Start = 1
      }
    }
    else{
      print("Vels")
      print(prefixes[i])
      print(j)
    }
  }
}
TotalVels <- TotalVels %>%
  mutate(mean_vel = ifelse(Trial == "MexHotChoc", mean_vel / 1.428, mean_vel))

TotalVelsSubset <- TotalVels[, c("ID", "mean_vel", "move_perc")]
BigDataSheet <- merge(BigDataSheet, TotalVelsSubset, by = "ID")

Start=0
for(i in 1:10){
  if(file.exists(paste(prefixes[i],"096_DispersionMetrics.csv",sep = "_"))) {
    Deg = read.csv(paste(prefixes[i],"096_DispersionMetrics.csv",sep = "_"))
    Deg$Tag = sub('\\.0','',Deg$Tag)
    Deg$Col = prefixes[i]
    Deg$QR = i %% 2 == 1
    Deg$Bee = paste(Deg$Col, Deg$Tag, sep='_')
    Deg$Queen = Deg$Bee %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
    if(Start == 1){
      TotalDisp = rbind(TotalDisp, Deg)
    }
    if(Start == 0){
      TotalDisp = Deg
      Start = 1
    }
  }
}

DispNew <- reshape(TotalDisp, idvar=c("Tag", "Col", "QR", "Bee", "Queen"), timevar = "Day", direction="wide")

DispSubset <- DispNew[, c("Bee", "N90.Day1", "N90.Day2", "N90.Day3", "N90.Day4", "MRSD.Day1", "MRSD.Day2", "MRSD.Day3", "MRSD.Day4")]
BigDataSheet <- merge(BigDataSheet, DispSubset, by = "Bee", all.x = TRUE)


#Diectional Degree

Start = 0
for(i in 1:10){
  for(j in 0:95){
    if(file.exists(paste(prefixes[i],sprintf("%03d", j),"DegDir.csv",sep = "_"))) {
      Deg = read.csv(paste(prefixes[i],sprintf("%03d", j),"DegDir.csv",sep = "_"))
      Deg$Node = sub('\\.0','',Deg$Origin.interactor)
      Deg$ExpDay = as.factor(floor((j/24)+1))
      Deg$Day = as.factor(floor(((j+17)/24)+1))
      Deg$Zeit = as.numeric((j+17)%%24)
      Deg$Hour = j
      Deg$Col = prefixes[i]
      Deg$QR = i %% 2 == 1
      Deg$ID = paste(Deg$Col, Deg$Node, Deg$Hour, sep='_')
      Deg$Bee = paste(Deg$Col, Deg$Node, sep='_')
      Deg$Mod = "Head_Head"
      Deg$Trial = str_extract(Deg$Col, ".+?(?=_)")
      Deg$Queen = Deg$Bee %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalDir = rbind(TotalDir, Deg)
      }
      if(Start == 0){
        TotalDir = Deg
        Start = 1
      }
    }
    else{
      print("DegDir")
      print(prefixes[i])
      print(j)
    }
  }
}

DirSubset <- TotalDir[, c("ID", "count_ori", "count_des", "Initiation.Freq")]
BigDataSheet <- merge(BigDataSheet, DirSubset, by = "ID")

Start = 0
for(i in 1:10){
  for(j in 0:95){
    if(file.exists(paste(prefixes[i],sprintf("%03d", j),"Head_to_Head_False_Clustering.csv",sep = "_"))){
      Deg = read.csv(paste(prefixes[i],sprintf("%03d", j),"Head_to_Head_False_Clustering.csv",sep = "_"))
      Deg$Node = sub('\\.0','',Deg$X)
      Deg$ExpDay = as.factor(floor((j/24)+1))
      Deg$Day = as.factor(floor(((j+17)/24)+1))
      Deg$Zeit = as.numeric((j+17)%%24)
      Deg$Hour = j
      Deg$Col = prefixes[i]
      Deg$QR = i %% 2 == 1
      Deg$ID = paste(Deg$Col, Deg$Node, Deg$Hour, sep='_')
      Deg$Bee = paste(Deg$Col, Deg$Node, sep='_')
      Deg$Mod = "Head_Head"
      Deg$Trial = str_extract(Deg$Col, ".+?(?=_)")
      Deg$Queen = Deg$Bee %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalClu = rbind(TotalClu, Deg)
      }
      if(Start == 0){
        TotalClu = Deg
        Start = 1
      }
    }
    else{
      print("Clus")
      print(prefixes[i])
      print(j)
    }
  }
}

TotalCluSubset <- TotalClu[, c("ID", "clust")]
BigDataSheet <- merge(BigDataSheet, TotalCluSubset, by = "ID")


BigDataSheet$Degree = BigDataSheet$Degree / BigDataSheet$AntPresence
BigDataSheet$boutDegree = BigDataSheet$boutDegree / BigDataSheet$AntPresence
BigDataSheet$bodyDegree = BigDataSheet$bodyDegree / BigDataSheet$AntPresence

BigDataSheet <- BigDataSheet %>%
  mutate(TimeOfDay = case_when(
    Zeit<=17 & Zeit>=8 ~ "Day",
    Zeit>17 | Zeit<8 ~ "Night",
    TRUE ~ NA_character_  # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(TimeOfDay = factor(TimeOfDay, levels = c( "Day","Night")))

BigDataSheet$DayTime = paste(BigDataSheet$ExpDay, BigDataSheet$TimeOfDay, sep='_')
