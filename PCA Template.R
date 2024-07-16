# Load required libraries
library(tidyverse)
library(caret)

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(df$target, p = .8, list = FALSE, times = 1)
dfTrain <- df[trainIndex,]
dfTest <- df[-trainIndex,]

# PCA
pca_model <- prcomp(dfTrain[, -1], scale. = TRUE)
pca_pred <- predict(pca_model, newdata = dfTest[, -1])

# Explained variance
explained_variance <- summary(pca_model)$importance[2, ]

# Fine-tuning: Selecting number of components
cum_var <- cumsum(explained_variance)
n_components <- which(cum_var >= 0.95)[1]

# Visualization
pca_data <- data.frame(pca_pred)
ggplot(pca_data, aes(PC1, PC2, color = dfTest$target)) +
  geom_point() +
  ggtitle("PCA - First Two Principal Components")
