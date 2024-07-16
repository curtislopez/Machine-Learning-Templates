# Load required libraries
library(tidyverse)
library(caret)
library(MASS)
library(ggplot2)

# Train and test partition
set.seed(123)
trainIndex <- createDataPartition(df$target, p = .8, list = FALSE, times = 1)
dfTrain <- df[trainIndex,]
dfTest <- df[-trainIndex,]

# Quadratic Discriminant Analysis
control <- trainControl(method = "cv", number = 10)
qda_grid <- expand.grid(ncomp = seq(1, min(ncol(dfTrain)-1, 5)))

qda_model <- train(target ~ ., data = dfTrain, method = "qda", trControl = control, tuneGrid = qda_grid)
qda_pred <- predict(qda_model, newdata = dfTest)
qda_results <- postResample(qda_pred, dfTest$target)

# Visualization
ggplot(data = dfTest, aes(x = target, y = qda_pred)) +
  geom_point() +
  ggtitle("QDA Predictions")

# Fine-tuning
print(qda_model$bestTune)
print(qda_results)
