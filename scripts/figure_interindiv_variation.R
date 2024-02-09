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


Start <- 0
for (i in 1:10) {
    for (j in 1:35) {
        if (file.exists(paste(prefixes[i], Days[j], "Cent.csv", sep = "_"))) {
            Cent <- read.csv(paste(prefixes[i], Days[j], "Cent.csv", sep = "_"))
            Cent$Node <- sub("\\.0", "", Cent$X)
            Cent$Day <- floor((j - 1) / 9) + 1
            Cent$Hour <- Days[j]
            Cent$Col <- prefixes[i]
            Cent$QR <- i %% 2 == 1
            Cent$ID <- paste(Cent$Col, Cent$Node, sep = "_")
            Cent$Queen <- Cent$ID %in% c("RooibosTea_QR_1216_1646_ArUcoTag#52", "MexHotChoc_QR_1216_1646_ArUcoTag#13", "20230213_1745_AlmdudlerGspritzt_C1_ArUcoTag#24", "20221209_1613_QR_ArUcoTag#47", "20221123_1543_AmericanoLatte_QR_ArUcoTag#43")
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

ggplot(TotalCent, aes(x = Hour, y = Degree, group = ID)) +
    geom_jitter(aes(color = interaction(QR, Queen), alpha = Queen)) +
    geom_smooth(aes(group = interaction(QR, Queen), color = interaction(QR, Queen))) +
    theme_classic() +
    labs(title = "Degree in Queens, Queenless Workers, Queenright Workers") +
    scale_alpha_discrete(range = c(0.1, 0.4)) +
    scale_color_manual(
        name = "Bee",
        labels = c("QL Worker", "QR Worker", "Queen"),
        values = c("#161414", "#629CC0", "#7851A9")
    )

ggsave("../figures/DegreeCent.png", width = 8.5, height = 8.5, dpi = 600)

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

ggsave("../figures/ClosenessCent.png", width = 8.5, height = 8.5, dpi = 600)

ggplot(TotalCent, aes(x = Hour, y = Betweenness, group = ID)) +
    geom_jitter(aes(color = interaction(QR, Queen), alpha = Queen)) +
    geom_smooth(aes(group = interaction(QR, Queen), color = interaction(QR, Queen))) +
    theme_classic() +
    labs(title = "Betweenness Cent in Queens, Queenless Workers, Queenright Workers") +
    scale_alpha_discrete(range = c(0.1, 0.4)) +
    scale_color_manual(
        name = "Bee",
        labels = c("QL Worker", "QR Worker", "Queen"),
        values = c("#161414", "#629CC0", "#7851A9")
    )

ggsave("../figures/BetweennessCent.png", width = 8.5, height = 8.5, dpi = 600)

TotalCentMean <- aggregate(cbind(Degree, Closeness, Betweenness, QR, Queen) ~ ID, TotalCent, mean)

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


TotalCentMean$Col <- sub("_[^_]+$", "", TotalCentMean$ID)

ggplot(TotalCentMean, aes(x = Degree, y = Closeness)) +
    geom_point(aes(color = Col)) +
    theme_classic() +
    labs(title = "Degree Cent vs Closeness Cent, Colored by ID") +
    scale_alpha_discrete(range = c(0.3, 0.8))

library(dineq)
gini.wtd(TotalCentMean$Degree)
gini.wtd(TotalCentMean$Closeness)
gini.wtd(TotalCentMean$Betweenness)

TotalCentMeanWorker <- TotalCentMean[TotalCentMean$Queen == 0, ]

gini.wtd(TotalCentMeanWorker$Degree)
gini.wtd(TotalCentMeanWorker$Closeness)
gini.wtd(TotalCentMeanWorker$Betweenness)

ggplot(TotalCentMeanWorker, aes(x = Degree)) +
    geom_histogram(color = "#629CC0", fill = "#161414", bins = 20) +
    scale_x_log10(limits = c(1000, 10000)) +
    scale_y_log10() +
    theme_classic() +
    labs(title = "Degree Cent")

ggplot(TotalCentMeanWorker, aes(x = Degree)) +
    geom_histogram(color = "#629CC0", fill = "#161414", bins = 20) +
    scale_x_log10(limits = c(1000, 10000)) +
    scale_y_log10() +
    theme_classic() +
    labs(title = "Degree Cent Wrapped by Colony") +
    facet_wrap(~Col, nrow = 2)


ggplot(TotalCentMeanWorker, aes(x = Closeness)) +
    geom_histogram(color = "#629CC0", fill = "#161414", bins = 20) +
    theme_classic() +
    labs(title = "Closeness Cent")

ggplot(TotalCentMeanWorker, aes(x = Betweenness)) +
    geom_histogram(color = "#629CC0", fill = "#161414", bins = 20) +
    theme_classic() +
    labs(title = "Betweenness Cent")

ggplot(TotalCentMean, aes(x = Closeness, group = Queen)) +
    geom_histogram(aes(color = factor(Queen)), bins = 10, fill = "grey") +
    theme_classic() +
    labs(title = "Closeness Cent") +
    scale_color_manual(
        name = "Bee",
        labels = c("Worker", "Queen"),
        values = c("#629CC0", "#7851A9")
    ) +
    facet_wrap(~Col, nrow = 2)



ggplot(TotalCentMean, aes(x = Betweenness, group = Queen)) +
    geom_histogram(aes(color = factor(Queen)), bins = 20, fill = "grey") +
    theme_classic() +
    labs(title = "Betweenness Cent") +
    scale_color_manual(
        name = "Bee",
        labels = c("Worker", "Queen"),
        values = c("#629CC0", "#7851A9")
    ) +
    facet_wrap(~Col, nrow = 2)



ggplot(TotalCentMean, aes(x = Degree, group = Queen)) +
    geom_histogram(aes(color = factor(Queen)), bins = 10, fill = "grey") +
    theme_classic() +
    labs(title = "Degree Cent") +
    scale_color_manual(
        name = "Bee",
        labels = c("Worker", "Queen"),
        values = c("#629CC0", "#7851A9")
    ) +
    facet_wrap(~Col, nrow = 2)

TotalCentMeanRanked <- TotalCentMean %>%
    group_by(Col) %>%
    mutate(Rank = order(order(Degree, decreasing = TRUE)))

ggplot(TotalCentMeanRanked, aes(x = Rank, y = Degree)) +
    geom_line(aes(color = factor(QR))) +
    theme_classic() +
    labs(title = "Rank Decay of Degree Centrality") +
    facet_wrap(~ QR + Col, nrow = 2) +
    scale_color_manual(
        name = "Colony",
        labels = c("QL", "QR"),
        values = c("#161414", "#629CC0")
    )




# ddply(TotalCentMean, .(Col), summarise, corr = (cor.test(Betweenness, Degree, alternative = "two.sided", method = "kendall")), name = names(corr))

# Initialize an empty list to store the results
results <- list()

# Get the unique values of 'Col'
cols <- unique(TotalCentMean$Col)

# Loop over the unique values of 'Col'
for (col in cols) {
  # Subset the data for the current value of 'Col'
  data <- subset(TotalCentMean, Col == col)
  
  # Calculate the correlation
  corr <- summarise(cor.test(data$Betweenness, data$Degree, alternative = "two.sided", method = "kendall"))
  
  # Store the result in the list, using 'col' as the name
  results[[col]] <- corr
}

# Print the results
print(results)