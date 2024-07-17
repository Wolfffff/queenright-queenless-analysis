```{r Supp Figure 5a (BoutDegree Over Time)}
BDSMeansNoOv <- aggregate(cbind(Degree,Close,Eigen,Between,QR,Queen,boutDegree,boutBetween,boutClose,boutEigen,bodyDegree,bodyBetween,bodyClose,bodyEigen,AverageBoutLength,Presence,AntPresence,mean_vel,move_perc,N90.Day4,MRSD.Day4, Initiation.Freq, clust,Disparity) ~ Bee, BigDataSheet, mean)
BDSMeansNoOv$Trial = str_extract(BDSMeansNoOv$Bee, ".+?(?=_)")

BDSMeansNoOv <- BDSMeansNoOv %>%
  mutate(QR_Queen_Condition = case_when(
    QR==0 & Queen==0 ~ "Queenless",
    QR==1 & Queen==0 ~ "Queenright",
    Queen==1 ~ "Queen",
    TRUE ~ NA_character_  # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c( "Queenright","Queenless", "Queen")))

BDSMeansNoOv$ID = paste(BDSMeansNoOv$Trial,BDSMeansNoOv$QR_Queen_Condition)
BDSMeanOfMean <- aggregate(cbind(Degree,Close,Eigen,Between,QR,Queen,boutDegree,boutBetween,boutClose,boutEigen,bodyDegree,bodyBetween,bodyClose,bodyEigen,AverageBoutLength,Presence,AntPresence,mean_vel,move_perc,N90.Day4,MRSD.Day4, Initiation.Freq, clust,Disparity) ~ ID, BDSMeansNoOv, mean)
BDSMeanOfMean$Trial = str_extract(BDSMeanOfMean$ID, ".+?(?= )")

BDSMeanOfMean <- BDSMeanOfMean %>%
  mutate(QR_Queen_Condition = case_when(
    QR==0 & Queen==0 ~ "Queenless",
    QR==1 & Queen==0 ~ "Queenright",
    Queen==1 ~ "Queen",
    TRUE ~ NA_character_  # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c( "Queenright","Queenless", "Queen")))

ggplot(BDSMeanOfMean, aes(x = fct_rev(as.factor(QR_Queen_Condition)), y = boutDegree)) +
  geom_line(aes(group = Trial), color = "darkgray") +
  geom_point(aes(color = Trial), size = 5) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Mean Number of Interactions per Hour (Unstandardized)") + # Adjust axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    #legend.position = "none",
    text = element_text(size = 16),
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

```

```{r Supp Figure 5b (Presence-Non-standardized Degree Over Time)}

BigDataSheetF2$UnStandardizedDegree = BigDataSheetF2$Degree * BigDataSheetF2$AntPresence

BDSMeansUnStandardized <- aggregate(cbind(UnStandardizedDegree,Close,Eigen,Between,QR,Queen,boutDegree,boutBetween,boutClose,boutEigen,bodyDegree,bodyBetween,bodyClose,bodyEigen,AverageBoutLength,Presence,AntPresence,mean_vel,move_perc,N90.Day4,MRSD.Day4, Initiation.Freq, clust,Disparity) ~ Bee, BigDataSheetF2, mean)
BDSMeansUnStandardized$Trial = str_extract(BDSMeansUnStandardized$Bee, ".+?(?=_)")

BDSMeansUnStandardized <- BDSMeansUnStandardized %>%
  mutate(QR_Queen_Condition = case_when(
    QR==0 & Queen==0 ~ "Queenless",
    QR==1 & Queen==0 ~ "Queenright",
    Queen==1 ~ "Queen",
    TRUE ~ NA_character_  # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c( "Queenright","Queenless", "Queen")))

BDSMeansUnStandardized$ID = paste(BDSMeansUnStandardized$Trial,BDSMeansUnStandardized$QR_Queen_Condition)
BDSMeanOfMeanUnstandardized <- aggregate(cbind(UnStandardizedDegree,Close,Eigen,Between,QR,Queen,boutDegree,boutBetween,boutClose,boutEigen,bodyDegree,bodyBetween,bodyClose,bodyEigen,AverageBoutLength,Presence,AntPresence,mean_vel,move_perc,N90.Day4,MRSD.Day4, Initiation.Freq, clust,Disparity) ~ ID, BDSMeansUnStandardized, mean)
BDSMeanOfMeanUnstandardized$Trial = str_extract(BDSMeanOfMeanUnstandardized$ID, ".+?(?= )")

BDSMeanOfMeanUnstandardized <- BDSMeanOfMeanUnstandardized %>%
  mutate(QR_Queen_Condition = case_when(
    QR==0 & Queen==0 ~ "Queenless",
    QR==1 & Queen==0 ~ "Queenright",
    Queen==1 ~ "Queen",
    TRUE ~ NA_character_  # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c( "Queenright","Queenless", "Queen")))

ggplot(BDSMeanOfMeanUnstandardized, aes(x = fct_rev(as.factor(QR_Queen_Condition)), y = UnStandardizedDegree)) +
  geom_line(aes(group = Trial), color = "darkgray") +
  geom_point(aes(color = Trial), size = 5) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  xlab("") +
  labs(color = "Source Colony") +
  ylab("Mean Number of Interactions per Hour (Unstandardized)") + # Adjust axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    #legend.position = "none",
    text = element_text(size = 16),
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

fm <- lmer(formula = UnStandardizedDegree ~ 1 + TimeOfDay + QR_Queen_Condition + (1|Trial/Day), data = BigDataSheetF2) #to run the model
fm.null <- lmer(formula = UnStandardizedDegree ~ 1 + TimeOfDay + (1|Trial/Day), data = BigDataSheetF2) #to run the model
anova(fm, fm.null)
print(fm)

```
