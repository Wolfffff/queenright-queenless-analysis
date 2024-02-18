# Load Velocity Data  ----------------------------------------


prefixes <- c("RooibosTea_QR_1216_1646", "RooibosTea_QL_1216_1646", "MexHotChoc_QR_1216_1646", "MexHotChoc_QL_1216_1646", "20230213_1745_AlmdudlerGspritzt_C1", "20230213_1745_AlmdudlerGspritzt_C0", "20221209_1613_QR", "20221209_1613_QL", "20221123_1543_AmericanoLatte_QR", "20221123_1543_AmericanoLatte_QL")
Day <- 1
Day1 <- c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020", "021", "022", "023", "024")
Day2 <- c("025", "026", "027", "028", "029", "030", "031", "032", "033", "034", "035", "036", "037", "038", "039", "040", "041", "042", "043", "044", "045", "046", "047", "048")
Day3 <- c("049", "050", "051", "052", "053", "054", "055", "056", "057", "058", "059", "060", "061", "062", "063", "064", "065", "066", "067", "068", "069", "070", "071", "072")
Day4 <- c("073", "074", "075", "076", "077", "078", "079", "080", "081", "082", "083", "084", "085", "086", "087", "088", "089", "090", "091", "092", "093", "094", "095", "096")
Days <- c(Day1, Day2, Day3, Day4)
Start <- 0

for (i in 1:length(prefixes)) {
  for (j in 1:length(Days)) {
    if (file.exists(paste(prefixes[i], Days[j], "velstrack.csv", sep = "_"))) {
      Vel <- read.csv(paste(prefixes[i], Days[j], "velstrack.csv", sep = "_"))
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
TotalVel <- na.omit(TotalVel)
TotalVel <- TotalVel[TotalVel$mean_vel < 15, ]


# Plot Velocity Timecourse  ----------------------------------------

ggplot(TotalVel, aes(x = Hour, y = move_perc, group = ID)) +
  # geom_line(aes(color = interaction(QR, Queen), alpha= Queen)) +
  geom_smooth(aes(group = interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() +
  labs(title = "Movement Percentage in Queens, Queenless Workers, Queenright Workers") +
  scale_alpha_discrete(range = c(0.1, 0.4)) +
  scale_color_manual(
    name = "Bee",
    labels = c("Queenless Worker", "Queenright Worker", "Queen"),
    values = c("#CEB175", "#629CC0", "#E54E21")
  )


# Create the plot
total_vel_no_queen <- TotalVel[!TotalVel$Queen, ]
ggplot(TotalVel, aes(x = as.integer(Hour), y = move_perc, group = ID)) +
  geom_smooth(aes(group = interaction(QR, Queen), color = interaction(QR, Queen))) +
  scale_x_continuous(breaks = c(0, seq(24, 96, by = 24)), limits = c(0, NA),expand=c(0,0)) + # Expand limits to include 0
  labs(color = element_blank()) +
  xlab("Hour") +
  ylab("Fraction of Time Spent Moving") +
  scale_color_manual(
    labels = c("Queenless Worker", "Queenright Worker", "Queen"),
    values = c("#CEB175", "#629CC0", "#E54E21"),
    guide = guide_legend(direction = "horizontal")
  ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(1, 1), # Move legend to top right
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
  guides(alpha = "none", color="none") +
  coord_cartesian(ylim = c(0.65, 0.9))

ggsave("../figures/qlqr_vel_over_time.jpg", width = 4.25, height = 4.25*(2/3), dpi = 600)


ggplot(TotalVel, aes(x = Hour, y = move_perc, group = ID)) +
  geom_jitter(aes(color = interaction(QR, Queen), alpha = Queen)) +
  geom_smooth(aes(group = interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() +
  labs(title = "Movement Percentage in Queens, Queenless Workers, Queenright Workers") +
  theme(legend.position = "none") +
  scale_alpha_discrete(range = c(0.1, 0.4)) +
  scale_color_manual(
    name = "Bee",
    labels = c("Queenless Worker", "Queenright Worker", "Queen"),
    values = c("#CEB175", "#629CC0", "#E54E21")
  )

ggsave("../figures/move_perc.jpg", width = 8.5, height = 2.75, dpi = 600)

ggplot(TotalVel, aes(x = Hour, y = move_perc, group = ID)) +
  geom_jitter(aes(color = interaction(QR, Queen), alpha = Queen)) +
  geom_smooth(aes(group = interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() +
  labs(title = "Movement Percentage in Queens, Queenless Workers, Queenright Workers") +
  theme(legend.position = "none") +
  scale_alpha_discrete(range = c(0.1, 0.4)) +
  scale_color_manual(
    name = "Bee",
    labels = c("Queenless Worker", "Queenright Worker", "Queen"),
    values = c("#CEB175", "#629CC0", "#E54E21")
  ) +
  facet_wrap(~Col, nrow = 5)


ggplot(TotalVel, aes(x = Hour, y = mean_vel, group = ID)) +
  # geom_line(aes(color = interaction(QR, Queen), alpha= Queen)) +
  geom_smooth(aes(group = interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() +
  labs(title = "Movement Speed in Queens, Queenless Workers, Queenright Workers") +
  scale_alpha_discrete(range = c(0.1, 0.4)) +
  scale_color_manual(
    name = "Bee",
    labels = c("Queenless Worker", "Queenright Worker", "Queen"),
    values = c("#CEB175", "#629CC0", "#E54E21")
  )

ggsave("../figures/mean_vel_2.jpg", width = 8.5, height = 2.75, dpi = 600)

ggplot(TotalVel, aes(x = Hour, y = mean_vel, group = ID)) +
  geom_line(aes(color = interaction(QR, Queen), alpha = Queen)) +
  geom_smooth(aes(group = interaction(QR, Queen), color = interaction(QR, Queen))) +
  theme_classic() +
  labs(title = "Movement Speed in Queens, Queenless Workers, Queenright Workers") +
  scale_alpha_discrete(range = c(0.1, 0.4)) +
  scale_color_manual(
    name = "Bee",
    labels = c("Queenless Worker", "Queenright Worker", "Queen"),
    values = c("#CEB175", "#629CC0", "#E54E21")
  )

ggsave("../figures/mean_vel_3.jpg", width = 8.5, height = 2.75, dpi = 600)