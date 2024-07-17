```{r Supp Figure 3 (Presence Metrics)}
ggplot(BDSMeansNoOv, aes(AntPresence, group = as.factor(QR))) + 
  geom_density(alpha = 0.2, aes(color=as.factor(QR))) + 
  scale_color_manual(labels = c("Queenless Worker","Queenright Worker"), values = c("#161414","#429CF0"))+
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
  
```
