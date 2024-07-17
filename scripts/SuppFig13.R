```{r Supp Figure 13 (Influencers are Less Specialized re: directionality)}

ggplot(BDSMeansNoOv, aes(x = Degree, y = bodyDegree, group = QR_Queen_Inf)) +
  new_scale_color() +
  geom_point(aes(color = QR_Queen_Inf,shape=as.factor(QR)), alpha = 1, size = BDSMeansNoOv$PointSize) +
  scale_color_manual(
    labels = c( "Queenright Worker","Queenless Worker","Queen","Influencer"),
    values = c( "#429CF0","#161414","#7851A9","#AA1233"))+
  scale_x_continuous() + # Use a continuous scale for x
  scale_y_continuous() + # Use a continuous scale for y
  xlab("Head Degree Centrality") +
  ylab("Body Degree Centrality") + # Adjust axis labels
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
  ) 
```
