#--------------APO-----------------------

# Load required library
library(readr)

# Load the dataset
APO_data <- read_csv("Downloads/lab1_dataset.csv")

# Attach the dataset to make column access easier
attach(APO_data)

# Print out values of the APO.new column
APO.new

# Identify NA values in the APO.new column
NAs <- is.na(APO.new)

# Filter out NA values to create a new array
APO.new.noNAs <- APO.new[!NAs]

# Generate and print summary statistics for APO.new
summary_stats <- summary(APO.new)
print(summary_stats)

# Compute and print the five-number summary, excluding NA values
five_num_summary <- fivenum(APO.new, na.rm = TRUE)
print(five_num_summary)

# Plotting
# Set up the PDF device to capture all plots
pdf("APO_new_Plots.pdf")

# Create a stem-and-leaf plot for APO.new
stem(APO.new)

# Create histograms and density plots
hist(APO.new)

# Print out min and max values of APO.new to check the range
min_value <- min(APO.new, na.rm = TRUE)
max_value <- max(APO.new, na.rm = TRUE)
print(paste("Min value:", min_value))
print(paste("Max value:", max_value))

# Adjusting the breaks parameter to include the max_value
# Ensure the max_value is included in the histogram range
breaks_hist <- seq(min_value, max_value + 1, length.out = 60)  # +1 to ensure max_value is included

# Generate a histogram covering the entire range of APO.new
hist(APO.new, breaks = breaks_hist, prob = TRUE)

lines(density(APO.new, na.rm = TRUE, bw = 1.))
lines(density(APO.new, na.rm = TRUE, bw = "SJ"))
rug(APO.new)

# More density plots with different parameters
hist(APO.new, breaks = breaks_hist, prob = TRUE)
lines(density(APO.new, na.rm = TRUE, bw = 1.))
rug(APO.new)

hist(APO.new, breaks = breaks_hist, prob = TRUE)
lines(density(APO.new, na.rm = TRUE, bw = 'SJ'))
rug(APO.new)

# Create a sequence of numbers and calculate normal density function values
x <- seq(min_value, max_value, by = 1)  # Adjusted to use the actual range of APO.new
q <- dnorm(x, mean = 42, sd = 5, log = FALSE)
lines(x, q)
lines(x, .4 * q)
q <- dnorm(x, mean = 65, sd = 5, log = FALSE)
lines(x, .12 * q)

# Exercise 2: Fitting a distribution beyond histograms
# CDF, Q-Q plots for normal and t distributions
plot(ecdf(APO.new), do.points = FALSE, verticals = TRUE)
qqnorm(APO.new)
qqline(APO.new)
qqplot(rnorm(250), APO.new, xlab = "Q-Q plot for norm dsn")
qqline(APO.new)
qqplot(rt(250, df = 5), APO.new, xlab = "Q-Q plot for t dsn")
qqline(APO.new)

# Close the PDF device
dev.off()

#--------------WRS-----------------------

# Load required library
library(readr)

# Load the dataset
WRS_data <- read_csv("Downloads/lab1_dataset.csv")

# Attach the dataset to make column access easier
attach(WRS_data)

# Print out values of the WRS.new column
WRS.new

# Identify NA values in the WRS.new column
NAs <- is.na(WRS.new)

# Filter out NA values to create a new array
WRS.new.noNAs <- WRS.new[!NAs]

# Generate and print summary statistics for WRS.new
summary_stats <- summary(WRS.new)
print(summary_stats)

# Compute and print the five-number summary, excluding NA values
five_num_summary <- fivenum(WRS.new, na.rm = TRUE)
print(five_num_summary)

# Plotting
# Set up the PDF device to capture all plots
pdf("WRS_new_Plots.pdf")

# Create a stem-and-leaf plot for WRS.new
stem(WRS.new)

# Create histograms and density plots
hist(WRS.new)

# Print out min and max values of WRS.new to check the range
min_value <- min(WRS.new, na.rm = TRUE)
max_value <- max(WRS.new, na.rm = TRUE)
print(paste("Min value:", min_value))
print(paste("Max value:", max_value))

# Ensure the max_value is included in the histogram range
breaks_hist <- seq(min_value, max_value + 1, length.out = 60)  # +1 to ensure max_value is included

# Generate a histogram covering the entire range of WRS.new
hist(WRS.new, breaks = breaks_hist, prob = TRUE)

lines(density(WRS.new, na.rm = TRUE, bw = 1.))
lines(density(WRS.new, na.rm = TRUE, bw = "SJ"))
rug(WRS.new)

# More density plots with different parameters
hist(WRS.new, breaks = breaks_hist, prob = TRUE)
lines(density(WRS.new, na.rm = TRUE, bw = 1.))
rug(WRS.new)

hist(WRS.new, breaks = breaks_hist, prob = TRUE)
lines(density(WRS.new, na.rm = TRUE, bw = 'SJ'))
rug(WRS.new)

# Create a sequence of numbers and calculate normal density function values
x <- seq(min_value, max_value, by = 1)  # Adjusted to use the actual range of WRS.new
q <- dnorm(x, mean = 42, sd = 5, log = FALSE)
lines(x, q)
lines(x, .4 * q)
q <- dnorm(x, mean = 65, sd = 5, log = FALSE)
lines(x, .12 * q)

# Exercise 2: Fitting a distribution beyond histograms
# CDF, Q-Q plots for normal and t distributions
plot(ecdf(WRS.new), do.points = FALSE, verticals = TRUE)
qqnorm(WRS.new)
qqline(WRS.new)
qqplot(rnorm(250), WRS.new, xlab = "Q-Q plot for norm dsn")
qqline(WRS.new)
qqplot(rt(250, df = 5), WRS.new, xlab = "Q-Q plot for t dsn")
qqline(WRS.new)

# Close the PDF device
dev.off()
