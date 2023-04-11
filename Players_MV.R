# Load the required packages
library(tidyverse)
library(caret)
library(glmnet)
library(randomForest)

# Read the dataset
players <- read_csv("players_fifa23.csv")

# Explore the dataset
summary(players)
str(players)
glimpse(players)

# Check for missing values
colSums(is.na(players))

# Drop columns with too many missing values
players <- players %>% select(-c("NationalPosition", "NationalNumber", "ClubNumber", "LongShots", "Aggression", "Interceptions", "Positioning", "Vision", "HeadingAccuracy", "LongPassing", "Acceleration", "Agility", "Reactions", "Jumping"))

# Impute missing values with median for numeric columns and mode for categorical columns
num_cols <- sapply(players, is.numeric)
cat_cols <- sapply(players, is.character)

players[num_cols] <- lapply(players[num_cols], function(x) {
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
})

players[cat_cols] <- lapply(players[cat_cols], function(x) {
  ifelse(is.na(x), names(which.max(table(x))), x)
})


# Convert categorical columns to factors
players[cat_cols] <- lapply(players[cat_cols], as.factor)

# Create a new column for market value in millions of Euros
players$Market_Value_Millions <- players$ValueEUR / 1000000

# Split the data into training and testing sets (80/20 ratio)
set.seed(123)
train_index <- createDataPartition(players$Market_Value_Millions, p = 0.8, list = FALSE)
train_data <- players[train_index, ]
test_data <- players[-train_index, ]

# Check the dimensions of the training and testing sets
dim(train_data)
dim(test_data)

# Create a formula for the regression model
formula <- Market_Value_Millions ~ .

# Train a linear regression model using lm function
model_lm <- lm(formula, data = train_data)

summary(model_lm)


