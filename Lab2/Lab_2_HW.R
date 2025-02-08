# Load required libraries
library(ggplot2)       # For visualization
library(readr)         # For reading CSV files
library(dplyr)         # For data manipulation
library(rpart)         # For Decision Tree Regression
library(randomForest)  # For Random Forest Regression

# Load dataset
dataset <- read_csv("Downloads/Spring 25/Data Analytics/R Lab/Data Analytics S25 - Lab2/NY-House-Dataset.csv")

# Remove outliers in Price
dataset <- dataset[dataset$PRICE < 195000000,]

# Remove missing values
dataset <- na.omit(dataset)

# Log transformation for better linearity
dataset$logPRICE <- log10(dataset$PRICE)
dataset$logPROPERTYSQFT <- log10(dataset$PROPERTYSQFT)

# Fit Linear Regression Model (With Interactions)
model_lm <- lm(logPRICE ~ logPROPERTYSQFT * BEDS * BATH, data = dataset)
summary(model_lm)

# Fit Decision Tree Model
model_tree <- rpart(logPRICE ~ logPROPERTYSQFT + BEDS + BATH, data = dataset, method = "anova")
print(model_tree)

# Fit Random Forest Model
set.seed(123)
model_rf <- randomForest(logPRICE ~ logPROPERTYSQFT + BEDS + BATH, data = dataset, ntree = 500, mtry = 2)
print(model_rf)

# Function to plot predictions vs actual values
plot_predictions <- function(model, title) {
  predicted_values <- predict(model, dataset)
  ggplot(dataset, aes(x = logPRICE, y = predicted_values)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    labs(title = title, x = "Actual Log Price", y = "Predicted Log Price")
}

# Function to plot residuals
plot_residuals <- function(model, title) {
  residual_values <- dataset$logPRICE - predict(model, dataset)
  ggplot(dataset, aes(x = predict(model, dataset), y = residual_values)) +
    geom_point() +
    labs(title = title, x = "Predicted Log Price", y = "Residuals")
}

# Generate and save plots
pdf("Downloads/Spring 25/Data Analytics/R Lab/Data Analytics S25 - Lab2/model_plots.pdf")
print(plot_predictions(model_lm, "Linear Regression: Predictions vs Actual"))
print(plot_residuals(model_lm, "Linear Regression: Residuals"))
print(plot_predictions(model_tree, "Decision Tree: Predictions vs Actual"))
print(plot_residuals(model_tree, "Decision Tree: Residuals"))
print(plot_predictions(model_rf, "Random Forest: Predictions vs Actual"))
print(plot_residuals(model_rf, "Random Forest: Residuals"))
dev.off()

# Save model summary statistics
sink("Downloads/Spring 25/Data Analytics/R Lab/Data Analytics S25 - Lab2/model_summaries.txt")
cat("Linear Regression Summary:\n")
print(summary(model_lm))
cat("\nDecision Tree Summary:\n")
print(model_tree)
cat("\nRandom Forest Summary:\n")
print(model_rf)
sink()
