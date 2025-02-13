# Load required libraries
library(ggplot2)       # For visualization
library(readr)         # For reading CSV files
library(dplyr)         # For data manipulation
library(car)           # For additional regression diagnostics

# Load dataset
dataset <- read_csv("/Users/zabir/Downloads/Spring-25/Data Analytics/R Lab/Data Analytics S25 - Lab2/NY-House-Dataset.csv")

summary(dataset)

# Data cleaning
# Remove outliers in Price
dataset <- dataset %>% 
  filter(PRICE < quantile(PRICE, 0.99))  # Removing the top 1% of the Price as outliers

# Filter dataset: Remove properties with extremely high prices (outliers)
dataset <- dataset[dataset$PRICE < 195000000,]

# Remove specific row where property square footage has an unusual value
dataset <- dataset[dataset$PROPERTYSQFT != 2184.207862,]

# Remove missing values
dataset <- na.omit(dataset)

# Variable transformations for better model fit
dataset$logPRICE <- log(dataset$PRICE)
dataset$logPROPERTYSQFT <- log(dataset$PROPERTYSQFT)

# Fit Linear Regression Models
model1 <- lm(logPRICE ~ logPROPERTYSQFT, data = dataset)
model2 <- lm(logPRICE ~ logPROPERTYSQFT + BEDS, data = dataset)
model3 <- lm(logPRICE ~ logPROPERTYSQFT + BEDS + BATH, data = dataset)

# Print model summaries
sink("/Users/zabir/Downloads/Spring-25/Data Analytics/R Lab/Data Analytics S25 - Lab2/model_summaries.txt")
cat("Model 1 Summary:\n")
print(summary(model1))
cat("\nModel 2 Summary:\n")
print(summary(model2))
cat("\nModel 3 Summary:\n")
print(summary(model3))
sink()

# Function to plot the most significant variable vs Price
plot_significant_vs_price <- function(model, data, significant_var, title) {
  ggplot(data, aes_string(x = significant_var, y = "logPRICE")) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = title, x = significant_var, y = "Log Price")
}

# Function to plot residuals
plot_residuals <- function(model, title) {
  residuals <- resid(model)
  fitted <- fitted.values(model)
  ggplot(data.frame(fitted, residuals), aes(x = fitted, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, col = "red") +
    labs(title = title, x = "Fitted Log Price", y = "Residuals")
}

# Generate and save plots
pdf("/Users/zabir/Downloads/Spring-25/Data Analytics/R Lab/Data Analytics S25 - Lab2/model_plots.pdf")
print(plot_significant_vs_price(model1, dataset, "logPROPERTYSQFT", "Model 1: logPROPERTYSQFT vs Log Price"))
print(plot_residuals(model1, "Model 1: Residuals"))
print(plot_significant_vs_price(model2, dataset, "logPROPERTYSQFT", "Model 2: logPROPERTYSQFT + BEDS  vs Log Price"))
print(plot_residuals(model2, "Model 2: Residuals"))
print(plot_significant_vs_price(model3, dataset, "logPROPERTYSQFT", "Model 3: logPROPERTYSQFT + BEDS + BATH vs Log Price"))
print(plot_residuals(model3, "Model 3: Residuals"))
dev.off()

# Comparing models
cat("\nComparing Models:\n")
cat("Model 1 (R-squared):", summary(model1)$r.squared, "\n")
cat("Model 2 (R-squared):", summary(model2)$r.squared, "\n")
cat("Model 3 (R-squared):", summary(model3)$r.squared, "\n")
