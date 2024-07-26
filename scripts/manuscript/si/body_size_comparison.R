BDSMeans$OvaryIndex = BDSMeans$AverageOvaryWidth/BDSMeans$AverageWingLength

BDSMeans <- BDSMeans %>%
  mutate(QR_Queen_Condition = case_when(
    QR==0 & Queen==0 ~ "Queenless",
    QR==1 & Queen==0 ~ "Queenright",
    Queen==1 ~ "Queen",
    TRUE ~ NA_character_  # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c( "Queenright","Queenless", "Queen")))

BDSMeansOvNoQueen = BDSMeans[BDSMeans$Queen==FALSE,]

ggplot(BDSMeansOvNoQueen, aes(AverageWingLength, fill = QR_Queen_Condition)) + 
  geom_density(alpha = 0.2) + 
  scale_fill_manual(labels = c("Queenless Worker","Queenright Worker"), values = c("#161414","#429CF0"))+
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5),
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
  facet_wrap(~Trial, scales="free_y")
