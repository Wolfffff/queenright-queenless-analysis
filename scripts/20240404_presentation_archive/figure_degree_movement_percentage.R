# Load Velocity Data  ----------------------------------------

library(tidyverse)
library(lme4)
# setwd("~/Downloads/IBRGData_110123")
prefixes <- c("RooibosTea_QR_1216_1646", "RooibosTea_QL_1216_1646", "MexHotChoc_QR_1216_1646", "MexHotChoc_QL_1216_1646", "20230213_1745_AlmdudlerGspritzt_C1", "20230213_1745_AlmdudlerGspritzt_C0", "20221209_1613_QR", "20221209_1613_QL", "20221123_1543_AmericanoLatte_QR", "20221123_1543_AmericanoLatte_QL")
Day <- 1
Day1 <- c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020", "021", "022", "023", "024")
Day2 <- c("025", "026", "027", "028", "029", "030", "031", "032", "033", "034", "035", "036", "037", "038", "039", "040", "041", "042", "043", "044", "045", "046", "047", "048")
Day3 <- c("049", "050", "051", "052", "053", "054", "055", "056", "057", "058", "059", "060", "061", "062", "063", "064", "065", "066", "067", "068", "069", "070", "071", "072")
Day4 <- c("073", "074", "075", "076", "077", "078", "079", "080", "081", "082", "083", "084", "085", "086", "087", "088", "089", "090", "091", "092", "093", "094", "095")
Days <- c(Day1, Day2, Day3, Day4)
Start = 0

for (i in 1:length(prefixes)) {
  for (j in 1:length(Days)) {
    if (file.exists(paste(prefixes[i], Days[j], "velstrackqueen.csv", sep = "_"))) {
      Vel <- read.csv(paste(prefixes[i], Days[j], "velstrackqueen.csv", sep = "_"))
      Vel$X <- gsub("'", "", Vel$X)
      Vel$X <- gsub("b", "", Vel$X)
      Vel$X <- sub("\\.0", "", Vel$X)
      colnames(Vel) <- c("Node", "mean_vel", "move_perc")
      Vel$Day <- floor((j - 1) / 9) + 1
      Vel$Hour <- Days[j]
      Vel$Col <- prefixes[i]
      Vel$QR <- i %% 2 == 1
      Vel$ID <- paste(Vel$Col, Vel$Node, sep = "_")
      Vel$Queen <- Vel$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if (Start == 1) {
        TotalVel <- rbind(TotalVel, Vel)
      }
      if (Start == 0) {
        TotalVel <- Vel
        Start <- 1
      }
    }
  }
}

Start=0
for (i in 1:length(prefixes)) {
  for (j in 1:length(Days)) {
    if(file.exists(paste(prefixes[i],Days[j],"DegHead.csv",sep = "_"))) {
      Deg = read.csv(paste(prefixes[i],Days[j],"DegHead.csv",sep = "_"))
      Deg$Node = sub('\\.0','',Deg$Node)
      Deg$Day = floor((j-1)/9)+1
      Deg$Hour = Days[j]
      Deg$Col = prefixes[i]
      Deg$QR = i %% 2 == 1
      Deg$ID = paste(Deg$Col, Deg$Node, sep='_')
      Deg$Mod = "Head_Head"
      Deg$Trial = str_extract(Deg$Col, ".+?(?=_)")
      Deg$Queen = Deg$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
      if(Start == 1){
        TotalDeg = rbind(TotalDeg, Deg)
      }
      if(Start == 0){
        TotalDeg = Deg
        Start = 1
      }
    }
  }
}
# TotalVel <- na.omit(TotalVel)

# get unique IDs by Col in TotalVel

source("../scripts/utils/base_utils.R")
deg_filtered <- filter_data("/Users/wolf/git/queenright-queenless-analysis/meta/experimental_tag_list.csv", TotalDeg)
vel_filtered <- filter_data("/Users/wolf/git/queenright-queenless-analysis/meta/experimental_tag_list.csv", TotalVel)

deg_vel <- merge(deg_filtered, vel_filtered, by=c("ID", "Hour","Day","QR","Queen"))
dim(deg_vel)

DegVelMean <- aggregate(cbind(move_perc,mean_vel,Degree,QR,Queen) ~ ID, deg_vel, mean)
dim(DegVelMean)
DegVelMean$Col <- sub("_[^_]+$", "", DegVelMean$ID)

# Unique count per Col
DegVelMean %>%
  group_by(Col) %>%
  summarise(Unique = length(unique(ID)))

DegVelMean$source_colony <- sapply(strsplit(DegVelMean$Col, "_"), `[`, 1)
DegVelMean$source_colony <- factor(DegVelMean$source_colony, levels = c("RooibosTea", "MexHotChoc", "20230213", "20221209", "20221123"))


DegVelMean <- DegVelMean[DegVelMean$move_perc > 0.05,]
DegVelMean$Alpha <- ifelse(DegVelMean$Queen, 0.5, 0.75)
DegVelMean$PointSize <- ifelse(DegVelMean$Queen, 2, 1)

# DegVelMean <- DegVelMean %>%
#   group_by(source_colony) %>%
#   mutate(
#     move_perc = scale(move_perc,scale=FALSE)[,1],  # Scale and extract the scaled values as a vector
#     Degree = scale(Degree,scale=FALSE)[,1]  # Scale and extract the scaled values as a vector
#   ) %>%
#   ungroup()

ggplot(DegVelMean, aes(x = move_perc*(60*60), y = Degree/20, colour = interaction(QR, Queen))) +
  geom_smooth(
    data = subset(DegVelMean, Queen == FALSE), # Subset the data to exclude queens
    method = lm, # Change method to lm for linear model
    se = T, # Standard error
    size = .75, # Adjust size to match first plot
    linetype = 1, # Line type
    aes(color = interaction(QR,Queen))
  ) + # Color by treatment
  geom_point(aes(color = interaction(QR, Queen)), alpha = 1,size= DegVelMean$PointSize) +
  scale_color_manual(values = c("#CEB175", "#629CC0", "#E54E21"), labels = c("Queenless Worker", "Queenright Worker", "Queen"), guide = guide_legend(direction = "vertical")) + # Adjust colors and labels
  scale_x_continuous() + # Use a continuous scale for x
  scale_y_continuous() + # Use a continuous scale for y
#   labs(title="Total Interaction Count vs Movement Percentage", color = "Treatment") +  # Adjust title and legend title
  xlab("Mean Time Spent Moving per Hour (s)") +
  ylab("Mean Time Spent Interacting per Hour (s)") + # Adjust axis labels
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
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

ggsave("../figures/interactions_by_frac_moving_qrql.jpg", width = 6.25, height = 6.25, dpi = 600)
#### stats ####
# Drop queen
df_for_stats <- deg_vel[deg_vel$Queen == FALSE,]
df_for_stats$Col <- sub("_[^_]+$", "", df_for_stats$ID)

df_for_stats$zeit <- as.numeric(df_for_stats$Hour) %% 24
df_for_stats$day <- factor(as.numeric(df_for_stats$Hour) %/% 24 + 1)  # Convert day to a factor
df_for_stats$source_colony <- sapply(strsplit(df_for_stats$Col, "_"), `[`, 1)
df_for_stats$source_colony <- factor(df_for_stats$source_colony, levels = c("RooibosTea", "MexHotChoc", "20230213", "20221209", "20221123"))

df_for_stats$QR <-fct_relevel(as.factor(df_for_stats$QR), "TRUE")
fm.null <- lmer(Degree ~ move_perc + QR + zeit + (1 | day) + (1 | source_colony), data = df_for_stats)
fm <- lmer(Degree ~ move_perc * QR + zeit + (1 | day) + (1 | source_colony), data = df_for_stats)

anova(fm, fm.null)
summary(fm)

#### end ####
# Correlation between movement percentage and degree within each group,

 
fm <- lmer(formula = values ~ 1 + QR + Day + Day:QR + (1 | Col), data = DegVelMean) # to run the model
fm.null <- lmer(formula = values ~ 1 + Day + (1 | Col), data = DegVelMean) # to run the model
anova(fm, fm.null)
summary(fm)
#### end ####


second_plot_df <- DegVelMean
second_plot_df$source_colony <- sapply(strsplit(second_plot_df$Col, "_"), `[`, 1)
second_plot_df$source_colony <- factor(second_plot_df$source_colony, levels = c("RooibosTea", "MexHotChoc", "20230213", "20221209", "20221123"))

grouped_sum$Treatment <- ifelse(grouped_sum$Queen, "Queen", grouped_sum$Treatment)
library(ggnewscale)
ggplot(second_plot_df, aes(x = move_perc, y = Degree, group = interaction(QR, Queen))) +
  geom_smooth(
    data = subset(second_plot_df, Queen == FALSE), # Subset the data to exclude queens
    method = lm, # Change method to lm for linear model
    se = T, # Standard error
    size = .75, # Adjust size to match first plot
    linetype = 1, # Line type
    aes(color = interaction(QR, Queen))
  ) + # Color by treatment
    scale_color_manual(values = c("#CEB175", "#629CC0", "#E54E21"), labels = c("Queenless Worker", "Queenright Worker", "Queen"), guide = guide_legend(direction = "vertical")) + # Adjust colors and labels
  new_scale_color() +
  geom_point(aes(color = source_colony,shape=as.factor(QR)), alpha = 1,size = second_plot_df$PointSize) +
  scale_color_manual(values = wes_palette("Cavalcanti1")) +
  scale_x_continuous() + # Use a continuous scale for x
  scale_y_continuous() + # Use a continuous scale for y
#   labs(title="Total Interaction Count vs Movement Percentage", color = "Treatment") +  # Adjust title and legend title
  xlab("Fraction of Time Spent Moving") +
  ylab("Mean Number of Interactions per Hour") + # Adjust axis labels
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
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

ggsave("../figures/interactions_by_frac_moving_by_col.jpg", width = 6.25, height = 6.25, dpi = 600)
