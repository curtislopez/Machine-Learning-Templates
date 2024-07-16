# Load required libraries
library(tidyverse)
library(caret)
library(e1071)
library(ggplot2)

# Train and test partition
set.seed(123)
trainIndex <- createDataPartition(df$target, p = .8, list = FALSE, times = 1)
dfTrain <- df[trainIndex,]
dfTest <- df[-trainIndex,]

# SVM
svm_grid <- expand.grid(C = 2^(-5:2), sigma = 2^(-15:3))
control <- trainControl(method = "cv", number = 10)
svm_model <- train(target ~ ., data = dfTrain, method = "svmRadial", trControl = control, tuneGrid = svm_grid)
svm_pred <- predict(svm_model, newdata = dfTest)
svm_results <- postResample(svm_pred, dfTest$target)

# Visualization
ggplot(data = dfTest, aes(x = target, y = svm_pred)) +
  geom_point() +
  ggtitle("SVM Predictions")
