# Read the data
data <- read.csv("dogbites.2024.csv")

# 5.1: Estimate rate, lambda, of dog bite incidents
lambda <- mean(data$dog.bites)

# 5.2a: Probability of 3 or less admissions for a dog-bite in a day
ppois(3, lambda)

# 5.2b: Most likely number of dog-bite admissions to occur on any given day
floor(lambda)

# 5.2c: Probability of seeing between 15 and 25 dog-bite admissions over a period of 4 days
ppois(25, lambda * 4) - ppois(14, lambda * 4)

# 5.2d: Probability of seeing four or more dog-bite admissions for at least 10 days in a 14 day period
1 - pbinom(9, 14, 1 - ppois(3, lambda))
