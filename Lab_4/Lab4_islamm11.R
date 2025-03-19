# Load necessary libraries
library(readr)       # For reading CSV files
library(class)       # For kNN
library(caret)       # For confusion matrix and accuracy metrics
library(ggplot2)     # For visualization
library(ggfortify)   # For PCA visualization

# Open a PDF device to save plots
pdf("wine_analysis_plots.pdf", width = 8, height = 10)

# Open a text file to save text outputs
sink("wine_analysis_output.txt")

# Read dataset
wine <- read_csv("/Users/zabir/Downloads/Spring-25/Data Analytics/R Lab/Lab 4/wine/wine.csv")
colnames(wine) <- c("class", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash", "Magnesium", 
                    "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", 
                    "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")

# Convert 'class' to factor (ensure it is treated as a categorical variable)
wine$class <- as.factor(wine$class)

# Standardize the data (excluding the class column)
wine_data <- wine[, -1]
wine_data <- scale(wine_data)

# Perform PCA
pca <- prcomp(wine_data, center = TRUE, scale. = TRUE)

# Check the variance explained by each principal component
pca_summary <- summary(pca)
cat("PCA Summary:\n")
print(pca_summary)

# Plot the data on the 1st and 2nd principal components
cat("\nPlotting PCA Results (Original Dataset):\n")
autoplot(pca, data = wine, colour = 'class', size = 3)

# Identify the variables that contribute the most to the 1st PC
pc1_loadings <- pca$rotation[, 1]
pc1_loadings_sorted <- sort(abs(pc1_loadings), decreasing = TRUE)
cat("\nVariables Contributing the Most to the 1st PC:\n")
print(pc1_loadings_sorted)

# Drop the variables least contributing to the 1st PC (e.g., drop the ones with low absolute loadings)
# Select the top contributing variables
important_vars <- names(pc1_loadings_sorted)[1:8]  # Choose top 8 contributing variables

# Create a new dataset with the selected important variables
wine_data_reduced <- wine_data[, important_vars]

# Perform PCA again with the reduced dataset
pca_reduced <- prcomp(wine_data_reduced, center = TRUE, scale. = TRUE)

# Plot the reduced PCA data on the 1st and 2nd principal components
cat("\nPlotting PCA Results (Reduced Dataset):\n")
autoplot(pca_reduced, data = wine, colour = 'class', size = 3)

# Train a kNN classifier on the original dataset
set.seed(123)  # Set seed for reproducibility
knn_model_original <- train(class ~ ., data = wine, method = "knn", tuneLength = 10)

# Train a kNN classifier on the reduced dataset (after PCA)
wine_scores <- data.frame(pca$x[, 1:3])  # Use the first 3 principal components as features
wine_scores$class <- wine$class
knn_model_reduced <- train(class ~ ., data = wine_scores, method = "knn", tuneLength = 10)

# Compare the two models using confusion matrices and precision/recall/F1 metrics
# Predict on original dataset using kNN
pred_original <- predict(knn_model_original, newdata = wine)

# Predict on reduced dataset using kNN
pred_reduced <- predict(knn_model_reduced, newdata = wine_scores)

# Ensure the predicted values are factors with the same levels as the actual values
pred_original <- factor(pred_original, levels = levels(wine$class))
pred_reduced <- factor(pred_reduced, levels = levels(wine$class))

# Generate confusion matrices
conf_matrix_original <- confusionMatrix(pred_original, wine$class)
conf_matrix_reduced <- confusionMatrix(pred_reduced, wine$class)

# Print confusion matrices
cat("\nConfusion Matrix for Original Model:\n")
print(conf_matrix_original)

cat("\nConfusion Matrix for Reduced Model:\n")
print(conf_matrix_reduced)

# Compare precision, recall, and F1 score for both models
precision_recall_f1_original <- postResample(pred_original, wine$class)
precision_recall_f1_reduced <- postResample(pred_reduced, wine$class)

# Print evaluation metrics
cat("\nMetrics for Original Model:\n")
print(precision_recall_f1_original)

cat("\nMetrics for Reduced Model:\n")
print(precision_recall_f1_reduced)

# Close the PDF device
dev.off()

# Close the text file
sink()

cat("\nAll outputs saved to 'wine_analysis_output.txt' and 'wine_analysis_plots.pdf'.\n")