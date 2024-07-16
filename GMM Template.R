# Load required libraries
library(tidyverse)
library(mclust)
library(ggplot2)

# Train and test partition
set.seed(123)
trainIndex <- createDataPartition(df$target, p = .8, list = FALSE, times = 1)
dfTrain <- df[trainIndex,]
dfTest <- df[-trainIndex,]

# GMM
gmm_model <- Mclust(dfTrain)
gmm_pred <- predict(gmm_model, dfTest)$classification
gmm_results <- table(gmm_pred, dfTest$target)

# Fine-tuning: Selecting number of components
bic <- mclustBIC(dfTrain)
best_gmm_model <- Mclust(dfTrain, G = which.max(bic))
best_gmm_pred <- predict(best_gmm_model, dfTest)$classification
best_gmm_results <- table(best_gmm_pred, dfTest$target)

# Visualization
ggplot(data = dfTest, aes(x = target, y = as.factor(best_gmm_pred))) +
  geom_point() +
  ggtitle("GMM Predictions")
