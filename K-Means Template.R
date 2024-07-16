# Load required libraries
library(tidyverse)
library(ggplot2)

# Train and test partition
set.seed(123)
trainIndex <- createDataPartition(df$target, p = .8, list = FALSE, times = 1)
dfTrain <- df[trainIndex,]
dfTest <- df[-trainIndex,]

# K-Means
set.seed(123)
kmeans_model <- kmeans(dfTrain, centers = 3)
kmeans_pred <- predict(kmeans_model, dfTest)
kmeans_results <- table(kmeans_pred, dfTest$target)

# Fine-tuning: Elbow method to find optimal number of clusters
wss <- (nrow(dfTrain)-1)*sum(apply(dfTrain, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(dfTrain, centers = i)$tot.withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# Visualization
ggplot(data = dfTest, aes(x = target, y = as.factor(kmeans_pred))) +
  geom_point() +
  ggtitle("K-Means Predictions")
