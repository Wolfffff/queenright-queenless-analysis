```{r Supp Figure 12 (Influencers are not Assortative)}

setwd("~/Downloads/QueenModelComplete_062424/")
Start=0
for(i in 1:10){
  for(j in 0:95){
    if(file.exists(paste(prefixes[i],sprintf("%03d", j),"Head_to_Head_False_NWP.csv",sep = "_"))) {
      Deg = read.csv(paste(prefixes[i],sprintf("%03d", j),"Head_to_Head_False_NWP.csv",sep = "_"))
    }
    Deg$ExpDay = as.factor(floor((j/24)+1))
    Deg$Day = as.factor(floor(((j+17)/24)+1))
    Deg$Zeit = as.numeric((j+7)%%24)
    Deg$Hour = j
    Deg$Col = prefixes[i]
    Deg$QR = i %% 2 == 1
    Deg$ID = paste(Deg$Col, Deg$Hour)
    Deg$Mod = "Head_Head"
    Deg$Trial = str_extract(Deg$Col, ".+?(?=_)")
    if(Start == 1){
      TotalNWP = rbind(TotalNWP, Deg)
    }
    if(Start == 0){
      TotalNWP = Deg
      Start = 1
    }
  }
}

TotalNWP <- TotalNWP %>%
  mutate(TimeOfDay = case_when(
    Zeit<=17 & Zeit>=8 ~ "Day",
    Zeit>17 | Zeit<8 ~ "Night",
    TRUE ~ NA_character_  # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(TimeOfDay = factor(TimeOfDay, levels = c( "Day","Night")))
TotalNWP$ID = paste(TotalNWP$Trial,TotalNWP$QR)

TotalNWPKeyAssortPer = TotalNWP[TotalNWP$params == 'KeystoneAssPercentile',]

ggplot(TotalNWPKeyAssortPer, aes(x = as.integer(Hour), y = values, group = ID)) +
  geom_jitter(aes(color = QR, alpha = 0.005, size=0.01)) +
  geom_smooth(aes(group = QR, color = QR)) +
  scale_size(range = c(.001, .2)) +
  scale_color_manual(
    labels = c( "Queenless Worker"),
    values = c("#161414"))+
  scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA), expand = c(0, 0)) + # Expand limits to include 0
  labs(color = "") +
  xlab("Hour") +
  ylab("Assortativity Percentile") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(1, 1.02), # Move legend to top right
    legend.justification = c(1, 1), # Align legend to top right
    legend.background = element_rect(fill = alpha("white", 1)),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"), # Keep vertical grid lines
    panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"), # Keep vertical grid lines
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(), # Remove horizontal grid lines
    axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
    axis.line.y = element_line(color = "black", size = 0.5), # Add y-axis line
    strip.text = element_text(size = 10, face = "bold"), # Make facet plot titles larger and bold
    panel.grid = element_blank(),
    panel.border = element_blank()
  ) +
  # scale_y_log10() +
  guides(alpha = "none", size = "none")

```
