# Load required libraries
library(tidyverse)
library(psych)

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(df$target, p = .8, list = FALSE, times = 1)
dfTrain <- df[trainIndex,]
dfTest <- df[-trainIndex,]

# Factor Analysis
fa_model <- fa(dfTrain[, -1], nfactors = 3, rotate = "varimax")
fa_pred <- predict(fa_model, dfTest[, -1])

# Fine-tuning: Selecting number of factors
fa_tune <- fa.parallel(dfTrain[, -1], fa = "fa")

# Visualization
fa_scores <- data.frame(fa_pred)
ggplot(fa_scores, aes(X1, X2, color = dfTest$target)) +
  geom_point() +
  ggtitle("Factor Analysis - First Two Factors")
