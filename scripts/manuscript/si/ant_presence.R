BDSRanked <- aggregate(cbind(Degree,Close,Eigen,Between,QR,Queen,boutDegree,boutBetween,boutClose,boutEigen,bodyDegree,bodyBetween,bodyClose,bodyEigen,AverageBoutLength,Presence,AntPresence,mean_vel,move_perc,N90.Day4,MRSD.Day4,Initiation.Freq) ~ Bee, BigDataSheet, mean)
BDSRanked$Trial = str_extract(BDSRanked$Bee, ".+?(?=_)")
BDSRanked$Col = str_extract(BDSRanked$Bee, ".+?(?=#)")
BDSRanked = BDSRanked %>% arrange(Col, AntPresence) %>%
  group_by(Col) %>%
  mutate(Rank = rank(AntPresence))

ggplot(BDSRanked, aes(x = Rank, y = AntPresence)) + 
  geom_line(data = subset(BDSRanked, QR==1), aes(group = Col, color=Col)) +
  geom_point(data = subset(BDSRanked, Queen==1),color="purple",size=3)

BDSRankedTotal = BDSRanked %>% arrange(Col, Presence) %>%
  group_by(Col) %>%
  mutate(Rank = rank(Presence))

ggplot(BDSRankedTotal, aes(x = Rank, y = Presence)) + 
  geom_line(data = subset(BDSRankedTotal, QR==1), aes(group = Col, color=Col)) +
  geom_point(data = subset(BDSRankedTotal, Queen==1),color="purple",size=3)
