# Load required libraries
library(tidyverse)
library(arules)
library(arulesViz)

# Prepare the data for association rule learning
# Assuming df is a transactions data frame
df_transactions <- as(df, "transactions")

# Apriori algorithm
apriori_model <- apriori(df_transactions, parameter = list(supp = 0.01, conf = 0.8))

# Fine-tuning
params <- expand.grid(supp = seq(0.01, 0.1, by = 0.01), conf = seq(0.5, 0.9, by = 0.1))
best_model <- NULL
best_lift <- -Inf

for (i in 1:nrow(params)) {
  model <- apriori(df_transactions, parameter = list(supp = params$supp[i], conf = params$conf[i]))
  if (length(model) > 0) {
    max_lift <- max(quality(model)$lift)
    if (max_lift > best_lift) {
      best_model <- model
      best_lift <- max_lift
    }
  }
}

# Inspect best model
inspect(head(sort(best_model, by = "lift"), 10))

# Visualization
plot(best_model, method = "graph", control = list(type = "items"))
