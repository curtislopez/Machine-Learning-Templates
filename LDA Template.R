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

# Linear Discriminant Analysis
control <- trainControl(method = "cv", number = 10)
lda_grid <- expand.grid(ncomp = seq(1, min(ncol(dfTrain)-1, 5)))

lda_model <- train(target ~ ., data = dfTrain, method = "lda", trControl = control, tuneGrid = lda_grid)
lda_pred <- predict(lda_model, newdata = dfTest)
lda_results <- postResample(lda_pred, dfTest$target)

# Visualization
ggplot(data = dfTest, aes(x = target, y = lda_pred)) +
  geom_point() +
  ggtitle("LDA Predictions")

# Fine-tuning
print(lda_model$bestTune)
print(lda_results)
