# Load Degree Data — Head And Body ----------------------------------------
list_of_packages <- c("lme4", "ggplot2", "sp", "tidyverse", "car", "ggthemes", "dplyr", "dineq", "plyr")
pak::pkg_install(list_of_packages)
for (p in list_of_packages) {
  library(p, character.only = TRUE)
}


prefixes <- c("RooibosTea_QR_1216_1646", "RooibosTea_QL_1216_1646", "MexHotChoc_QR_1216_1646", "MexHotChoc_QL_1216_1646", "20230213_1745_AlmdudlerGspritzt_C1", "20230213_1745_AlmdudlerGspritzt_C0", "20221209_1613_QR", "20221209_1613_QL", "20221123_1543_AmericanoLatte_QR", "20221123_1543_AmericanoLatte_QL")
Day <- 1
Day1 <- c("001","002","003","004","005","006","007","008", "009","010","011","012","013","014","015","016", "017", "018", "019", "020", "021", "022", "023", "024")
Day2 <- c("025","026","027","028","029","030","031","032", "033","034","035","036","037","038","039","040", "041", "042", "043", "044", "045", "046", "047", "048")
Day3 <- c("049","050","051","052","053","054","055","056", "057","058","059","060","061","062","063","064", "065", "066", "067", "068", "069", "070", "071", "072")
Day4 <- c("073","074","075","076","077","078","079","080", "081","082","083","084","085","086","087","088", "089", "090", "091", "092", "093", "094", "095", "096")
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

TotalCentMean <- aggregate(cbind(Degree, Closeness, Betweenness, QR, Queen) ~ ID, TotalCent, mean)

TotalCentMean$Col <- sub("_[^_]+$", "", TotalCentMean$ID)

TotalCent$alpha <- ifelse(TotalCent$Queen, .5, 0.005)
TotalCent <- TotalCent[sample(nrow(TotalCent)), ]

ggplot(TotalCent, aes(x = as.integer(Hour), y = Degree, group = ID)) +
  geom_jitter(aes(color = interaction(QR, Queen), alpha = alpha), size = 0.001) +
  geom_smooth(aes(group = interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() +
  # Drop legend for alpha
  # labs(title = "Degree in Queens, Queenless Workers, Queenright Workers") +
  scale_color_manual(
    name = "",
    labels = c("Queenless Worker", "Queenright Worker", "Queen"),
    values = c("#5785C1", "#CB7A5C", "#FBA72A"),
    guide = guide_legend(direction = "horizontal")
  ) +
  scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA), expand = c(0, 0)) + # Expand limits to include 0
  labs(color = "Metric") +
  xlab("Hour") +
  ylab("Interaction Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(1, 1.02), # Move legend to top right
    legend.justification = c(1, 1), # Align legend to top right
    legend.background = element_rect(fill = alpha("white", 0.8)),
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
  scale_y_log10() +
  guides(alpha = "none")

ggsave("../figures/DegreeCent.png", width = 8.5, height = 3, dpi = 600, bg = "white")

ggplot(TotalCent, aes(x = Hour, y = Closeness, group = ID)) +
  geom_jitter(aes(color = interaction(QR, Queen), alpha = Queen)) +
  geom_smooth(aes(group = interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() +
  labs(title = "Closeness Cent in Queens, Queenless Workers, Queenright Workers") +
  scale_alpha_discrete(range = c(0.1, 0.4)) +
  scale_color_manual(
    name = "Bee",
    labels = c("QL Worker", "QR Worker", "Queen"),
    values = c("#161414", "#629CC0", "#7851A9")
  )

ggsave("../figures/ClosenessCent.png", width = 4.25, height = 3, dpi = 600)

ggplot(TotalCent, aes(x = Hour, y = Betweenness, group = ID)) +
  # geom_jitter(aes(color = interaction(QR, Queen), alpha = Queen)) +
  geom_smooth(aes(group = interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() +
  labs(title = "Betweenness Cent in Queens, Queenless Workers, Queenright Workers") +
  scale_alpha_discrete(range = c(0.1, 0.4)) +
  scale_color_manual(
    name = "Bee",
    labels = c("QL Worker", "QR Worker", "Queen"),
    values = c("#ff0000", "#629CC0", "#7851A9")
  )

ggsave("../figures/BetweennessCent.png", width = 4.25, height = 3, dpi = 600)


ggplot(TotalCentMean, aes(x = Degree, y = Closeness)) +
  geom_point(aes(color = interaction(factor(QR), factor(Queen)), alpha = factor(Queen))) +
  theme_classic() +
  labs(title = "Degree Cent vs Closeness Cent") +
  scale_alpha_discrete(range = c(0.3, 0.8)) +
  scale_color_manual(
    name = "Bee",
    labels = c("QL Worker", "QR Worker", "Queen"),
    values = c("#161414", "#629CC0", "#7851A9")
  )

ggplot(TotalCentMean, aes(x = Degree, y = Betweenness)) +
  geom_point(aes(color = interaction(factor(QR), factor(Queen)), alpha = factor(Queen))) +
  theme_classic() +
  labs(title = "Degree Cent vs Betweenness Cent") +
  scale_alpha_discrete(range = c(0.3, 0.8)) +
  scale_color_manual(
    name = "Bee",
    labels = c("QL Worker", "QR Worker", "Queen"),
    values = c("#161414", "#629CC0", "#7851A9")
  )

ggplot(TotalCentMean, aes(x = Betweenness, y = Closeness)) +
  geom_point(aes(color = interaction(factor(QR), factor(Queen)), alpha = factor(Queen))) +
  theme_classic() +
  labs(title = "Betweenness Cent vs Closeness Cent") +
  scale_alpha_discrete(range = c(0.3, 0.8)) +
  scale_color_manual(
    name = "Bee",
    labels = c("QL Worker", "QR Worker", "Queen"),
    values = c("#161414", "#629CC0", "#7851A9")
  )

ggplot(TotalCentMean, aes(x = Degree, y = Closeness)) +
  geom_point(aes(color = Col)) +
  theme_classic() +
  labs(title = "Degree Cent vs Closeness Cent, Colored by ID") +
  scale_alpha_discrete(range = c(0.3, 0.8))


fm <- lmer(formula = values ~ 1 + QR + Day + Day:QR + (1 | Col), data = TotalAss) # to run the model
fm.null <- lmer(formula = values ~ 1 + Day + (1 | Col), data = TotalAss) # to run the model
anova(fm, fm.null)


# Assuming 'degree' is the column you want to plot
list_of_cols <- unique(TotalCentMeanRanked$Col)

for (col in list_of_cols) {

  subset_data <- subset(TotalCentMeanRanked, Col == col)
  
  # Sum 'Degree' by 'ID'
  sum_data <- subset_data %>%
    group_by(ID) %>%
    summarise(Degree = sum(Degree, na.rm = TRUE),
              Queen = first(Queen))
  
  p <- ggplot(sum_data, aes(x = Degree, y = ID,color = as.factor(Queen))) +
    geom_point() +
    labs(title = paste("Cleveland Dot Plot for", col),
         x = "Degree",
         y = "ID") +
    theme_minimal() +
    xlim(0, max(sum_data$Degree))

  ggsave(paste0("../figures/cleveland_dot_plot_", col, ".jpg"), plot = p, width = 8.5, height = 5, dpi = 600)
  
  print(p)
}
