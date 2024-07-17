```{r Supp Figure 4 (Head to Head vs Head to Body)}

library(reshape2)

BigDataSheetQRFilt = data.frame(BigDataSheetQR$Hour,BigDataSheetQR$Degree,BigDataSheetQR$bodyDegree, BigDataSheetQR$ID,BigDataSheetQR$Trial)
colnames(BigDataSheetQRFilt)<- c("Hour","Degree","bodyDegree","ID","Trial") 

d <- melt(BigDataSheetQRFilt, id=c("Hour","ID","Trial"))

BigDataSheetF2$Alpha <- ifelse(BigDataSheetF2$Queen, .5, 0.005)
BigDataSheetF2 <- BigDataSheetF2[sample(nrow(BigDataSheetF2)), ]
BigDataSheetF2$PointSize <- ifelse(BigDataSheetF2$Queen, .003, .001)

ggplot(d, aes(x = as.integer(Hour), y = value/20, group = variable)) +
  geom_jitter(aes(color = variable, alpha = 0.005, size = 0.001)) +
  geom_smooth(aes(group = variable, color = variable)) +
  scale_size(range = c(.001, .2)) +
  scale_color_manual(
    labels = c( "Head-to-Head", "Head-to-Body"),
    values = c( "#DDBC58", "#023020"),
    guide = guide_legend(direction = "horizontal")
  ) +
  scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA), expand = c(0, 0)) + # Expand limits to include 0
  labs(color = "") +
  xlab("Hour") +
  ylab("Time Spent Interacting per Hour (s)") +
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

fm <- lmer(formula = bodyDegree ~ 1 + QR +  (1|Trial) + (1|QR:Trial) + (1|DayTime)  + (1|DayTime:Trial), data = BigDataSheetF2) #to run the model
fm.null <- lmer(formula = bodyDegree ~ 1 +  (1|Trial) + (1|QR:Trial) + (1|DayTime)  + (1|DayTime:Trial), data = BigDataSheetF2)
anova(fm, fm.null)
print(fm)

fm <- lmer(formula = Degree ~ 1 + QR + bodyDegree + (1|Trial) + (1|QR:Trial) + (1|DayTime)  + (1|DayTime:Trial), data = BigDataSheetF2) #to run the model
fm.null <- lmer(formula = Degree ~ 1 + bodyDegree + (1|Trial) + (1|QR:Trial) + (1|DayTime)  + (1|DayTime:Trial), data = BigDataSheetF2)
anova(fm, fm.null)
print(fm)

fm <- lmer(formula = Degree ~ 1 + QR + bodyDegree + QR:bodyDegree + (1|Trial) + (1|QR:Trial) + (1|DayTime)  + (1|DayTime:Trial), data = BigDataSheetF2) #to run the model
fm.null <- lmer(formula = Degree ~ 1 + QR + bodyDegree + (1|Trial) + (1|QR:Trial) + (1|DayTime)  + (1|DayTime:Trial), data = BigDataSheetF2) #to run the model
anova(fm, fm.null)
print(fm)
```
