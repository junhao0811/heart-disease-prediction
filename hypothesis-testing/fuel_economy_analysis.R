# Part 1.1
# Load the data
df <- read.csv("fuel.csv")

# Calculate the mean, variance, and number of observations for all-wheel drive vehicles
mean_awd <- mean(df$FA[df$Type == "A"])
var_awd <- var(df$FA[df$Type == "A"])
n_awd <- length(df$FA[df$Type == "A"])

# Display the results
cat("Mean fuel efficiency for all-wheel drive vehicles:", mean_awd, "\n")
cat("Variance for all-wheel drive vehicles:", var_awd, "\n")
cat("Number of observations for all-wheel drive vehicles:", n_awd, "\n")

# Part 1.2
# Calculate the mean, variance, and number of observations for part-time four-wheel drive vehicles
mean_pt4wd <- mean(df$FA[df$Type == "P"])
var_pt4wd <- var(df$FA[df$Type == "P"])
n_pt4wd <- length(df$FA[df$Type == "P"])

# Output the results
cat("Mean fuel efficiency for part-time four-wheel drive vehicles:", mean_pt4wd, "\n")
cat("Variance for part-time four-wheel drive vehicles:", var_pt4wd, "\n")
cat("Number of observations for part-time four-wheel drive vehicles:", n_pt4wd, "\n")

pnorm(1.8678)

# Part 2.1
# Use the probability mass function given
geom_pmf <- function(y, phi) {
  (exp(phi) + 1)^(-y-1) * exp(y * phi)
}

# Create a sequence of y values from 0 to 20
y_values <- 0:20

# Calculate probabilities for different values of phi
p_phi0 <- geom_pmf(y_values, 0)
p_phi1 <- geom_pmf(y_values, 1)
p_phi2 <- geom_pmf(y_values, 2)

# Plot the results
plot(y_values, p_phi0, type = "b", col = "blue", xlab = "y", ylab = "P(y | Φ)",
     main = "Geometric Probability Mass Function for different Φ", pch = 1)
lines(y_values, p_phi1, type = "b", col = "red", pch = 1)
lines(y_values, p_phi2, type = "b", col = "green", pch = 1)
legend("topright", legend = c("Φ = 0", "Φ = 1", "Φ = 2"), col = c("blue", "red", "green"),
       pch = 1)

# Part 3.2
2 * pnorm(-6.1968)

# Part 3.3
binom.test(x=168, n=240, p=0.5)

# Part 3.4
2 * pnorm(-5.0764)
