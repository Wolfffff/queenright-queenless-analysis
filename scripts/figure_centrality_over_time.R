# Load Degree Data — Head And Body ----------------------------------------
list_of_packages <- c("lme4", "ggplot2", "sp", "tidyverse", "car", "ggthemes", "dplyr","dineq","plyr")
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

QUEEN_LIST <- c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")

Start <- 0
for (i in 1:10) {
    for (j in 1:96) {
        if (file.exists(paste(prefixes[i], Days[j], "Cent.csv", sep = "_"))) {
            Cent <- read.csv(paste(prefixes[i], Days[j], "Cent.csv", sep = "_"))
            Cent$Node <- sub("\\.0", "", Cent$X)
            Cent$Day <- floor((j - 1) / 9) + 1
            Cent$Hour <- Days[j]
            Cent$Col <- prefixes[i]
            Cent$QR <- i %% 2 == 1
            Cent$ID <- paste(Cent$Col, Cent$Node, sep = "_")
            Cent$Queen <- Cent$ID %in% QUEEN_LIST
            if (Start == 1) {
                TotalCent <- rbind(TotalCent, Cent)
            }
            if (Start == 0) {
                TotalCent <- Cent
                Start <- 1
            }
        }
    }
}

TotalCent <- TotalCent[TotalCent$Degree > 100, ]

# Centrality Plotting ------------------------------------------------
ggplot(data = TotalCent, aes(x = as.integer(Hour), y = Betweenness, group = Col)) + 
  geom_jitter(aes(color = QR), size = 1, alpha = 0.8, width = 0.2) +
  geom_smooth(aes(group=QR, color=QR),method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7) +
  scale_color_manual(values = c("#629CC0", "#161414"), labels = c("Queenright","Queenless"), guide = guide_legend(direction = "horizontal")) +
  scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA)) +  # Set x-axis limits and breaks similar to the first script
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(1, 1),  # Move legend to top right
    legend.justification = c(1, 1),  # Align legend to top right
    legend.background = element_rect(fill = alpha('white', 0.8)),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),  # Keep vertical grid lines
    panel.grid.minor.x = element_line(color = "grey", linetype = "dotted"),  # Keep vertical grid lines
    panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(),  # Remove horizontal grid lines
    axis.line.x = element_line(color = "black", size = 0.5),  # Add x-axis line
    axis.line.y = element_line(color = "black", size = 0.5),  # Add y-axis line
    strip.text = element_text(size = 14, face = "bold")  # Make facet plot titles larger and bold
  ) +
  labs(title = "Centrality Over Time", color = "") +
  xlab("Time (Hour)") + ylab("Betweenness Centrality")

ggsave("../figures/fig3_betweenness_centrality_over_time.jpg", width = 8.5, height = 4, dpi = 600)



fm <- lmer(formula = values ~ 1 + QR + Day + Day:QR +(1|Col), data = TotalAss) #to run the model
fm.null <- lmer(formula = values ~ 1 + Day + (1|Col), data = TotalAss) #to run the model
anova(fm, fm.null)