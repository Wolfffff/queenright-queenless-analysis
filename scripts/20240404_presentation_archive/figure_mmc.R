# Load Degree Data — Head And Body ----------------------------------------
list_of_packages <- c("lme4", "ggplot2", "sp", "tidyverse", "car", "ggthemes", "dplyr")
pak::pkg_install(list_of_packages)
for (p in list_of_packages) {
  library(p, character.only = TRUE)
}


prefixes <- c("RooibosTea_QR_1216_1646", "RooibosTea_QL_1216_1646", "MexHotChoc_QR_1216_1646", "MexHotChoc_QL_1216_1646", "20230213_1745_AlmdudlerGspritzt_C1", "20230213_1745_AlmdudlerGspritzt_C0", "20221209_1613_QR", "20221209_1613_QL", "20221123_1543_AmericanoLatte_QR", "20221123_1543_AmericanoLatte_QL")
Day <- 1
Day1 <- c("016", "017", "018", "019", "020", "021", "022", "023", "024")
Day2 <- c("040", "041", "042", "043", "044", "045", "046", "047", "048")
Day3 <- c("064", "065", "066", "067", "068", "069", "070", "071", "072")
Day4 <- c("088", "089", "090", "091", "092", "093", "094", "095")
Days <- c(Day1, Day2, Day3, Day4)
Start <- 0



for (i in 1:10) {
  for (j in 1:35) {
    if (file.exists(paste(prefixes[i], Days[j], "DegHead.csv", sep = "_"))) {
      Deg <- read.csv(paste(prefixes[i], Days[j], "DegHead.csv", sep = "_"))
      Deg$Node <- sub("\\.0", "", Deg$Node)
      Deg$Day <- floor((j - 1) / 9) + 1
      Deg$Hour <- Days[j]
      Deg$Col <- prefixes[i]
      Deg$QR <- i %% 2 == 1
      Deg$ID <- paste(Deg$Col, Deg$Node, sep = "_")
      Deg$Mod <- "Head_Head"
      Deg$Trial <- str_extract(Deg$Col, ".+?(?=_)")
      Deg$Queen <- Deg$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if (Start == 1) {
        TotalDeg <- rbind(TotalDeg, Deg)
      }
      if (Start == 0) {
        TotalDeg <- Deg
        Start <- 1
      }
    }
  }
}

# A few tags (82 and 88) that spuriously appear, get rid of them.
TotalDeg <- TotalDeg[TotalDeg$Degree > 100, ]

# Degree — Head-Head vs Head-Body -----------------------------------------------------------
# Add a new column to TotalDeg to indicate the group
TotalDeg$Group <- ifelse(TotalDeg$QR & !TotalDeg$Queen, "Queenright Workers",
  ifelse(!TotalDeg$QR, "Queenless Workers",
    ifelse(TotalDeg$Queen, "Queens", NA)
  )
)

# Order the factor levels of the 'Group' column
TotalDeg$Group <- factor(TotalDeg$Group, levels = c("Queenless Workers", "Queenright Workers", "Queens"))
# Filter out rows where Group is NA
TotalDeg <- TotalDeg[!is.na(TotalDeg$Group), ]

# Create the plot
ggplot(TotalDeg, aes(x = as.integer(Hour), y = Degree, group = interaction(ID, Mod))) +
  geom_jitter(aes(color = Mod), size = 0.3, alpha = 0.1, width = 0.2) +
  geom_smooth(aes(group = Mod, color = Mod), size = 1, se = TRUE, linetype = 1, method = "loess") +
  scale_color_manual(values = c("#4a6741", "#EECA58"), labels = c("Head-Body", "Head-Head"), guide = guide_legend(direction = "horizontal")) +
  scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA)) + # Expand limits to include 0
  scale_y_log10() +
  facet_wrap(~Group) +
  labs(title = "Comparison of Hourly Interaction Count of Head-Head and Head-Body Interactions", color = "Metric") +
  xlab("Hour") +
  ylab("Interaction Count") +
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
  )

ggsave("../figures/interaction_type_unstandardized.jpg", width = 8.5, height = 4, dpi = 600)

fm <- lmer(formula = Degree ~ 1 + Mod + Hour + (1 | Col), data = TotalDeg)
fm.null <- lmer(formula = Degree ~ 1 + Hour + (1 | Col), data = TotalDeg)
anova(fm, fm.null)

summary(fm)
