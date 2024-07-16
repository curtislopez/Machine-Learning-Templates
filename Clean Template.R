# Load required libraries
library(tidyverse)
library(mice)
library(zoo)
library(VIM)
library(lubridate)

# Read the data from CSV file
df <- read.csv("example.csv", stringsAsFactors = FALSE)

# Remove rows with missing 'name'

df_cleaned <- df %>%
  filter(!is.na(name))

# Fill NA's with the mean ex_col
df_cleaned <- df_cleaned %>%
  mutate(ex_col = ifelse(is.na(ex_col), round(mean(ex_col, na.rm = TRUE), 0), ex_col))

# Fill missing 'gender' with 'Unknown'
df_cleaned <- df_cleaned %>%
  mutate(gender = replace_na(gender, "Unknown"))

# Fill missing 'salary' with the median salary
df_cleaned <- df_cleaned %>%
  mutate(salary = ifelse(is.na(salary), median(salary, na.rm = TRUE), salary))

# Convert 'joining_date' to Date type and handle missing dates
df_cleaned <- df_cleaned %>%
  mutate(joining_date = as.Date(joining_date, format = "%Y-%m-%d"),
         joining_date = replace_na(joining_date, as.Date("2020-01-01")))

# Interpolate missing 'column_name'
df_cleaned <- df_cleaned %>%
  mutate(column_name = na.approx(column_name))

# Create an indicator variable for missing 'ex_col'
df_cleaned <- df_cleaned %>%
  mutate(ex_col_missing = ifelse(is.na(ex_col), 1, 0),
         ex_col = ifelse(is.na(ex_col), mean(ex_col, na.rm = TRUE), ex_col))

# Impute missing values using KNN
df_cleaned <- kNN(df_cleaned, variable = "column_name", k = 5)

# Impute missing values using predictive modeling
imputed_data <- mice(df_cleaned, method = "rf")
df_cleaned <- complete(imputed_data)

# Display the cleaned data
print(df_cleaned)

# Save the cleaned data to a new CSV file
write.csv(df_cleaned, "cleaned_data.csv", row.names = FALSE)
