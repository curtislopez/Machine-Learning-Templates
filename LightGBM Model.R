# Load required libraries
library(tidyverse)
library(lightgbm)
library(ggplot2)

# Train and test partition
set.seed(123)
trainIndex <- createDataPartition(df$target, p = .8, list = FALSE, times = 1)
dfTrain <- df[trainIndex,]
dfTest <- df[-trainIndex,]

# LightGBM
lgb_train <- lgb.Dataset(data = as.matrix(dfTrain[-1]), label = dfTrain$target)
lgb_test <- as.matrix(dfTest[-1])
params <- list(objective = "regression", metric = "rmse")
lgb_model <- lgb.train(params, lgb_train, 100)
lgb_pred <- predict(lgb_model, lgb_test)
lgb_results <- postResample(lgb_pred, dfTest$target)

# Fine-tuning
tune_grid <- expand.grid(num_leaves = seq(10, 50, by = 10), learning_rate = c(0.01, 0.05, 0.1))
control <- trainControl(method = "cv", number = 10)
lgb_tuned_model <- train(as.matrix(dfTrain[-1]), dfTrain$target, method = "lightgbm", trControl = control, tuneGrid = tune_grid)
lgb_tuned_pred <- predict(lgb_tuned_model, as.matrix(dfTest[-1]))
lgb_tuned_results <- postResample(lgb_tuned_pred, dfTest$target)

# Visualization
ggplot(data = dfTest, aes(x = target, y = lgb_tuned_pred)) +
  geom_point() +
  ggtitle("LightGBM Predictions")
