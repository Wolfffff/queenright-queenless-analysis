```{r Supp Figure 7 (PCA Biplot)}

numerical_data <- BDSMeansNoOv[,c(2:5,8:15,19:21,23:25)]
numerical_data = numerical_data[complete.cases(numerical_data), ]
data_normalized <- scale(numerical_data)
data.pca <- princomp(data_normalized)
fviz_pca_var(data.pca, col.var = "black")
```
