# Load the mass spectrometry datasets
ms_train <- read.csv("ms.train.2024.csv")
ms_test <- read.csv("ms.test.2024.csv")

# Load necessary packages
source("wrappers.R")
library(rpart)
library(kknn)
library(boot)

# Part 3.1
# Create a function to compute MSE
compute_mse <- function(predicted, true) {
  mean((predicted - true)^2)
}

# Initialize a vector to store MSEs
mse_values <- numeric(25)

# Loop over k = 1 to 25
for (k in 1:25) {
  knn_model <- kknn(intensity ~ MZ, ms_train, ms_test, k = k, kernel = "optimal")
  predictions <- fitted(knn_model)

  # Calculate MSE
  mse_values[k] <- compute_mse(predictions, ms_test$intensity)
}

# Plot MSE vs k
plot(1:25, mse_values, type = "b", xlab = "k-values", ylab = "Mean-Squared Error (MSE)", main = "MSE for different k-values")

# Part 3.2
# Function to plot spectrum for a given k
plot_spectrum <- function(k) {
  knn_model <- kknn(intensity ~ MZ, ms_train, ms_test, k = k, kernel = "optimal")
  predictions <- fitted(knn_model)

  # Plot training data, true spectrum, and estimated spectrum
  plot(ms_train$MZ, ms_train$intensity,
    col = "blue", pch = 20, cex = 0.5,
    main = paste("k =", k), xlab = "MZ", ylab = "Intensity"
  )
  lines(ms_test$MZ, ms_test$intensity, col = "orange", lwd = 2)
  lines(ms_test$MZ, predictions, col = "red", lwd = 2)
  legend("topleft",
    legend = c("Training Data", "True Spectrum", "Estimated Spectrum"),
    col = c("blue", "orange", "red"), lwd = 2
  )
}

# Plot for k = 2, 6, 12, and 25
par(mfrow = c(2, 2)) # 4 plots in one window
plot_spectrum(2)
plot_spectrum(6)
plot_spectrum(12)
plot_spectrum(25)

# Part 3.3
for (k in c(2, 6, 12, 25)) {
  # Print MSE for each k
  print(paste("MSE for k =", k, ":", mse_values[k]))
}

# Part 3.4
cv_knn <- train.kknn(intensity ~ MZ,
  data = ms_train, kmax = 25,
  kernel = "optimal"
)
cv_knn$best.parameters$k

# Part 3.5
# Compute residuals for the best k-value from cross-validation
knn <- kknn(intensity ~ MZ, ms_train, ms_test, k = 6, kernel = "optimal")
predicted_values <- fitted(knn)
residuals <- ms_test$intensity - predicted_values

# Estimate the standard deviation of the noise
noise_sd <- sd(residuals)
noise_sd

# Part 3.7
# Find the MZ value corresponding to the maximum estimated intensity
max_MZ <- ms_test$MZ[which.max(predicted_values)]
max_intensity <- predicted_values[which.max(predicted_values)]
cat("MZ value corresponding to the maximum estimated intensity:", max_MZ, "\n")

# Part 3.8
# Define function to compute intensity at the max_MZ
bootstrap_intensity <- function(data, indices, k) {
  sample_data <- data[indices, ]
  knn_model <- kknn(intensity ~ MZ, sample_data, ms_test, k = k, kernel = "optimal")
  predicted_values <- fitted(knn_model)
  return(predicted_values[which.max(ms_test$MZ == max_MZ)])
}

# Bootstrap with the k = 6, k = 3, and k = 20
boot_k6 <- boot(data = ms_train, statistic = bootstrap_intensity, R = 5000, k = 6)
boot_k3 <- boot(data = ms_train, statistic = bootstrap_intensity, R = 5000, k = 3)
boot_k20 <- boot(data = ms_train, statistic = bootstrap_intensity, R = 5000, k = 20)

# Compute confidence intervals
cat("Confidence interval for k = 6:\n")
boot.ci(boot.out = boot_k6, conf = 0.95, type = "bca")

cat("Confidence interval for k = 3:\n")
boot.ci(boot.out = boot_k3, conf = 0.95, type = "bca")

cat("Confidence interval for k = 20:\n")
boot.ci(boot.out = boot_k20, conf = 0.95, type = "bca")

