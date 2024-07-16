# Load required libraries
library(tidyverse)
library(caret)
library(ggplot2)

# Train and test partition
set.seed(123)
trainIndex <- createDataPartition(df$target, p = .8, list = FALSE, times = 1)
dfTrain <- df[trainIndex,]
dfTest <- df[-trainIndex,]

# Logistic Regression
logistic_grid <- expand.grid(intercept = TRUE)
control <- trainControl(method = "cv", number = 10)
logistic_model <- train(target ~ ., data = dfTrain, method = "glm", family = binomial, trControl = control, tuneGrid = logistic_grid)
logistic_pred <- predict(logistic_model, newdata = dfTest)
logistic_results <- postResample(logistic_pred, dfTest$target)

# Visualization
ggplot(data = dfTest, aes(x = target, y = logistic_pred)) +
  geom_point() +
  ggtitle("Logistic Regression Predictions")
