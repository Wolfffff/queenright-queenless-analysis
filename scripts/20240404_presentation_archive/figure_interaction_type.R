# Load Degree Data — Head And Body ----------------------------------------
list_of_packages <- c("lme4", "ggplot2", "sp", "tidyverse", "car", "ggthemes", "dplyr")
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



for (i in 1:10) {
  for (j in 1:96) {
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

for (i in 1:10) {
  for (j in 1:96) {
    if (file.exists(paste(prefixes[i], Days[j], "DegBody.csv", sep = "_"))) {
      Deg <- read.csv(paste(prefixes[i], Days[j], "DegBody.csv", sep = "_"))
      Deg$Node <- sub("\\.0", "", Deg$Node)
      Deg$Day <- floor((j - 1) / 9) + 1
      Deg$Hour <- Days[j]
      Deg$Col <- prefixes[i]
      Deg$QR <- i %% 2 == 1
      Deg$ID <- paste(Deg$Col, Deg$Node, sep = "_")
      Deg$Mod <- "Head_Body"
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

source("../scripts/utils/base_utils.R")
TotalDeg <- filter_data("/Users/wolf/git/queenright-queenless-analysis/meta/experimental_tag_list.csv", TotalDeg)

# Degree — Head-Head vs Head-Body -----------------------------------------------------------
# Add a new column to TotalDeg to indicate the group
TotalDeg$Group <- ifelse(TotalDeg$QR & !TotalDeg$Queen, "Queenright Workers",
  ifelse(!TotalDeg$QR, "Queenless Workers",
    ifelse(TotalDeg$Queen, "Queens", NA)
  )
)

# Order the factor levels of the 'Group' column
TotalDeg$Group <- factor(TotalDeg$Group, levels = c("Queenless Workers", "Queenright Workers", "Queens"))

# Create the plot
# Add a new column for alpha levels
TotalDeg$alpha <- ifelse(TotalDeg$Group == "Queens", 1, 0.005)

# Shuffle the rows of the dataframe
TotalDeg <- TotalDeg[sample(nrow(TotalDeg)),]

# Create the plot
ggplot(TotalDeg, aes(x = as.integer(Hour), y = Degree, group = interaction(ID, Mod))) +
  geom_jitter(aes(color = Mod, alpha = alpha), size = 0.3, width = 0.2) +
  geom_smooth(aes(group = Mod, color = Mod), size = 1, se = TRUE, linetype = 1, method = "loess") +
  scale_color_manual(values = c("#4a6741", "#EECA58"), labels = c("Head-Body", "Head-Head"), guide = guide_legend(direction = "horizontal")) +
  scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA)) + # Expand limits to include 0
  scale_y_log10() +
  facet_wrap(~Group) +
  labs(color = "Metric") +
  xlab("Hour") +
  ylab("Interaction Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(1, .25), # Move legend to top right
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
    strip.text = element_text(size = 10, face = "bold") # Make facet plot titles larger and bold
  ) +
  guides(alpha="none")

ggsave("../figures/interaction_type_unstandardized.jpg", width = 8.5, height = 2.75, dpi = 600)


#### stats ####

df_for_stats <- TotalDeg
df_for_stats$zeit <- as.numeric(df_for_stats$Hour) %% 24
df_for_stats$day <- factor(as.numeric(df_for_stats$Hour) %/% 24 + 1)  # Convert day to a factor
df_for_stats$source_colony <- sapply(strsplit(df_for_stats$Col, "_"), `[`, 1)
df_for_stats$source_colony <- factor(df_for_stats$source_colony, levels = c("RooibosTea", "MexHotChoc", "20230213", "20221209", "20221123"))

fm.null <- lmer(Degree ~ 1 + zeit + (1 | day) + (1 | source_colony) + (1|Queen) + (1|QR), data = df_for_stats)
fm <- lmer(Degree ~ 1 + Mod + zeit + (1 | day) + (1 | source_colony) + (1|Queen) + (1|QR), data = df_for_stats)

anova(fm, fm.null)
summary(fm)

####