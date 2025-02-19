#########################
##### Abalone KNN ######
#########################

# Load necessary libraries
library(readr)
library(class)
library(caret)  # For confusion matrix and accuracy
library(ggplot2)  # For visualization

# Read dataset
abalone <- read_csv("abalone_dataset.csv")

# Assign column names
colnames(abalone) <- c("sex", "length", "diameter", "height", "whole_weight", 
                       "shucked_weight", "viscera_weight", "shell_weight", "rings")

# Convert "rings" column into categorical age groups
abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, breaks = c(-1, 8, 11, 35), labels = c("young", "adult", "old"))
abalone$rings <- as.factor(abalone$rings)

# Remove the "sex" variable since KNN requires numeric variables
aba <- abalone
aba$sex <- NULL

# Normalize numeric variables using Min-Max Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))

# Split the dataset into training (70%) and testing (30%)
set.seed(123)  # For reproducibility
ind <- sample(2, nrow(aba), replace = TRUE, prob = c(0.7, 0.3))
KNNtrain <- aba[ind == 1, ]
KNNtest <- aba[ind == 2, ]

##############################################
### Model 1: Using a Subset of Features ######
##############################################

# Select features: whole_weight, shell_weight, viscera_weight
KNNtrain_subset1 <- KNNtrain[, c("whole_weight", "shell_weight", "viscera_weight", "rings")]
KNNtest_subset1 <- KNNtest[, c("whole_weight", "shell_weight", "viscera_weight", "rings")]

# Determine optimal k value (square root of training samples)
k_value <- round(sqrt(nrow(KNNtrain)))

# Train KNN Model 1
KNNpred1 <- knn(train = KNNtrain_subset1[, 1:3], test = KNNtest_subset1[, 1:3], 
                cl = KNNtrain_subset1$rings, k = k_value)

# Evaluate Model 1 with a confusion matrix
confusion_matrix1 <- table(KNNpred1, KNNtest_subset1$rings)
accuracy1 <- sum(diag(confusion_matrix1)) / sum(confusion_matrix1)

##############################################
### Model 2: Using Another Subset of Features ######
##############################################

# Select features: length, diameter, height
KNNtrain_subset2 <- KNNtrain[, c("length", "diameter", "height", "rings")]
KNNtest_subset2 <- KNNtest[, c("length", "diameter", "height", "rings")]

# Train KNN Model 2
KNNpred2 <- knn(train = KNNtrain_subset2[, 1:3], test = KNNtest_subset2[, 1:3], 
                cl = KNNtrain_subset2$rings, k = k_value)

# Evaluate Model 2 with a confusion matrix
confusion_matrix2 <- table(KNNpred2, KNNtest_subset2$rings)
accuracy2 <- sum(diag(confusion_matrix2)) / sum(confusion_matrix2)

# Compare both models
cat("Accuracy of Model 1 (Subset 1 - whole_weight, shell_weight, viscera_weight):", accuracy1, "\n")
cat("Accuracy of Model 2 (Subset 2 - length, diameter, height):", accuracy2, "\n")

# Determine the better-performing model
if (accuracy1 > accuracy2) {
  best_model_train <- KNNtrain_subset1
  best_model_test <- KNNtest_subset1
  best_features <- colnames(KNNtrain_subset1)[1:3]
  best_conf_matrix <- confusion_matrix1
  best_accuracy <- accuracy1
  cat("Model 1 (Subset 1) performed better.\n")
} else {
  best_model_train <- KNNtrain_subset2
  best_model_test <- KNNtest_subset2
  best_features <- colnames(KNNtrain_subset2)[1:3]
  best_conf_matrix <- confusion_matrix2
  best_accuracy <- accuracy2
  cat("Model 2 (Subset 2) performed better.\n")
}

##########################################
### Finding Optimal k for Best Model #####
##########################################

# Function to find the best k value and return accuracies
find_best_k <- function(train, test, features, class_col) {
  k_values <- seq(1, 100, by = 2)  # Testing odd values of k from 1 to 99
  accuracies <- numeric(length(k_values))
  
  for (i in seq_along(k_values)) {
    k <- k_values[i]
    pred <- knn(train = train[, features], test = test[, features], cl = train[[class_col]], k = k)
    conf_matrix <- table(pred, test[[class_col]])
    accuracies[i] <- sum(diag(conf_matrix)) / sum(conf_matrix)
  }
  
  best_k <- k_values[which.max(accuracies)]
  return(list(best_k = best_k, k_values = k_values, accuracies = accuracies))
}

# Get the best k value for the better-performing model
best_k_results <- find_best_k(best_model_train, best_model_test, best_features, "rings")
optimal_k <- best_k_results$best_k
accuracies <- best_k_results$accuracies  # Store the accuracy vector
k_values <- best_k_results$k_values

cat("Optimal k value for the best model:", optimal_k, "\n")

# Train the best model with the optimal k
final_KNNpred <- knn(train = best_model_train[, best_features], 
                     test = best_model_test[, best_features], 
                     cl = best_model_train$rings, k = optimal_k)

# Final confusion matrix
final_conf_matrix <- table(final_KNNpred, best_model_test$rings)
final_accuracy <- sum(diag(final_conf_matrix)) / sum(final_conf_matrix)

cat("Final Model Accuracy with optimal k (", optimal_k, "):", final_accuracy, "\n")

##########################################
### Save Results and Plot to PDF #########
##########################################

pdf("KNN_Abalone_Results.pdf")

# Redirect all text output to the PDF
sink("KNN_Abalone_Results.txt")

cat("Summary of KNN Analysis on Abalone Dataset\n\n")
cat("\nModel 1 Confusion Matrix:\n")
print(confusion_matrix1)
cat("\nModel 1 Accuracy:", accuracy1, "\n")

cat("\nModel 2 Confusion Matrix:\n")
print(confusion_matrix2)
cat("\nModel 2 Accuracy:", accuracy2, "\n")

cat("\nOptimal k value:", optimal_k, "\n")

cat("\nFinal Confusion Matrix:\n")
print(final_conf_matrix)
cat("\nFinal Model Accuracy:", final_accuracy, "\n")

sink()  # Stop redirecting text output

# Plot Accuracy vs. k-value
plot_df <- data.frame(k_values, accuracies)
ggplot(plot_df, aes(x = k_values, y = accuracies)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ggtitle("K-value vs. Accuracy") +
  xlab("K Value") +
  ylab("Accuracy") +
  theme_minimal()

dev.off()

cat("Results and plots saved to 'KNN_Abalone_Results.pdf'!\n")
