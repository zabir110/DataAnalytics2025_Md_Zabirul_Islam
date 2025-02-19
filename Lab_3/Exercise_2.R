#########################
##### K-Means Clustering ######
#########################

# Load necessary libraries
library(readr)
library(ggplot2)
library(cluster)
library(factoextra)

# Read dataset
abalone <- read_csv("abalone_dataset.csv")

# Assign column names
colnames(abalone) <- c("sex", "length", "diameter", "height", "whole_weight", 
                       "shucked_weight", "viscera_weight", "shell_weight", "rings")

# Convert "rings" to a numerical variable for clustering
abalone$rings <- as.numeric(abalone$rings)

# Remove the "sex" column (non-numeric) and keep the best feature subset from Exercise 1
# Assuming best features from Exercise 1 were: "length", "diameter", "height"
best_features <- c("length", "diameter", "height")
abalone_subset <- abalone[, best_features]

# Normalize the selected features using Min-Max Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
abalone_subset <- as.data.frame(lapply(abalone_subset, normalize))

##############################
### Finding Optimal K (Elbow Method)
##############################

# Function to calculate total within-cluster sum of squares (WSS)
wss_values <- numeric()
k_values <- 1:10  # Testing values from K = 1 to 10

for (k in k_values) {
  km_model <- kmeans(abalone_subset, centers = k, nstart = 25)
  wss_values[k] <- km_model$tot.withinss
}

# Find the optimal K using the elbow method
optimal_k <- which(diff(diff(wss_values)) == min(diff(diff(wss_values)))) + 1
cat("Optimal number of clusters (K) found:", optimal_k, "\n")

##############################
### Train K-Means Model with Optimal K
##############################
set.seed(123)
kmeans_model <- kmeans(abalone_subset, centers = optimal_k, nstart = 25)

# Assign clusters to the dataset
abalone_subset$cluster <- as.factor(kmeans_model$cluster)

#################################
### Save Results & Plot Clusters
#################################

pdf("KMeans_Abalone_Results.pdf")

# Elbow Plot
plot(k_values, wss_values, type = "b", pch = 19, col = "blue",
     xlab = "Number of Clusters (K)", ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal K")

# Cluster Visualization using Two Features
ggplot(abalone_subset, aes(x = length, y = diameter, color = cluster)) +
  geom_point(size = 3) +
  ggtitle(paste("K-Means Clustering (K =", optimal_k, ")")) +
  theme_minimal()

dev.off()

cat("Results and plots saved to 'KMeans_Abalone_Results.pdf'!\n")
