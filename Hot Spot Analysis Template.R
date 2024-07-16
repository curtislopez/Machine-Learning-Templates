# Load required libraries
library(tidyverse)
library(spatstat)

# Assuming df contains spatial data with coordinates (x, y) and a variable of interest
coordinates(df) <- ~x + y

# Kernel Density Estimation
kde <- density.ppp(df, sigma = bw.ppl(df))

# Hot Spot Analysis (Getis-Ord Gi*)
hotspot <- spatstat::Ghat(df)

# Fine-tuning: Adjusting the bandwidth for KDE
bw_values <- seq(0.1, 2, by = 0.1)
best_bw <- bw.ppl(df)
best_score <- -Inf

for (bw in bw_values) {
  kde <- density.ppp(df, sigma = bw)
  score <- max(kde$z)  # Example scoring function
  if (score > best_score) {
    best_bw <- bw
    best_score <- score
  }
}

kde <- density.ppp(df, sigma = best_bw)

# Visualization
plot(kde, main = "Kernel Density Estimation")
plot(hotspot, main = "Hot Spot Analysis")
