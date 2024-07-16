# Load libraries
library(ggplot2)
library(MPV)
library(DAAG)
library(dplyr)
library(leaps)


# get data n stuff
df <- read.csv("womp womp.csv")

# Train and test partition
set.seed(123)
trainIndex <- sort(sample(df, nrow(df)*0.8))
train <- df[trainIndex,]
test <- df[-trainIndex,]

# Linear regression
lm_model <- lm(target ~ ., data = train)
lm_pred <- predict(lm_model, newdata = test)

# stepwise - a lil bit of fine tuning


#results
test$error <- lm_model$target - lm_pred
test$abs.error <- abs(test$error)

# model summary
summary(lm_model)
anova_thing <- anova(lm_model)

# k fold validation
cvoutput<-cv.lm(data = df, form.lm=formula(target ~.), m=5, dots = FALSE,seed=123, printit = TRUE)

# Visualization
ggplot(data = test, aes(x = target, y = lm_pred)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("Linear Regression Predictions")



