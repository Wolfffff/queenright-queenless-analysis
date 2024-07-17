```{r Supp Figure 8 (PCA With Trials)}
numerical_data$QR = BDSMeansNoOv[complete.cases(BDSMeans), ]$QR
numerical_data$Trial = BDSMeansNoOv[complete.cases(BDSMeans), ]$Trial
numerical_data$Queen = BDSMeansNoOv[complete.cases(BDSMeans), ]$Queen

numerical_data <- numerical_data %>%
  mutate(QR_Queen_Condition = case_when(
    QR!=1 & Queen!=1 ~ "Queenless",
    QR==1 & Queen!=1 ~ "Queenright",
    Queen==1 ~ "Queen",
    TRUE ~ NA_character_  # This handles any other case, which shouldn't exist in your scenario
  )) %>%
  mutate(QR_Queen_Condition = factor(QR_Queen_Condition, levels = c( "Queenright","Queenless", "Queen")))

fviz_pca_ind(data.pca, label="none", habillage=numerical_data$Trial,
             addEllipses=TRUE, ellipse.level=0.95, palette = "Dark2") + theme_minimal() + labs(title ="Standardized PCA")

pca_scores <- data.pca$scores
data_with_pca <- data.frame(BDSMeansNoOv, PC1 = pca_scores[, 1], PC2 = pca_scores[, 2])
```
