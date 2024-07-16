# Load required libraries
library(tidyverse)
library(caret)
library(class)
library(ggplot2)

# Train and test partition
set.seed(123)
trainIndex <- createDataPartition(df$target, p = .8, list = FALSE, times = 1)
dfTrain <- df[trainIndex,]
dfTest <- df[-trainIndex,]

# KNN
knn_grid <- expand.grid(k = seq(1, 20, by = 2))
control <- trainControl(method = "cv", number = 10)
knn_model <- train(target ~ ., data = dfTrain, method = "knn", trControl = control, tuneGrid = knn_grid)
knn_pred <- predict(knn_model, newdata = dfTest)
knn_results <- postResample(knn_pred, dfTest$target)

# Visualization
ggplot(data = dfTest, aes(x = target, y = knn_pred)) +
  geom_point() +
  ggtitle("KNN Predictions")
