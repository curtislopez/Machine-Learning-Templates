# Load required libraries
library(tidyverse)
library(caret)
library(randomForest)
library(ggplot2)

# Train and test partition
set.seed(123)
trainIndex <- createDataPartition(df$target, p = .8, list = FALSE, times = 1)
dfTrain <- df[trainIndex,]
dfTest <- df[-trainIndex,]

# Random Forest
rf_grid <- expand.grid(mtry = seq(2, ncol(dfTrain) - 1, by = 2))
control <- trainControl(method = "cv", number = 10)
rf_model <- train(target ~ ., data = dfTrain, method = "rf", trControl = control, tuneGrid = rf_grid)
rf_pred <- predict(rf_model, newdata = dfTest)
rf_results <- postResample(rf_pred, dfTest$target)

# Visualization
ggplot(data = dfTest, aes(x = target, y = rf_pred)) +
  geom_point() +
  ggtitle("Random Forest Predictions")
