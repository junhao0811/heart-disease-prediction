# Part 1.1

# Load the data
concrete <- read.csv("concrete.csv")

# Fit a multiple linear model
model <- lm(Strength ~ ., data = concrete)

# See the summary of the model
summary <- summary(model)
summary

# Part 1.2

# Extract coefficients summary
pvalues <- coefficients(summary)[, 4]

# Use Bonferroni procedure with α = 0.05
α = 0.05
names(pvalues)[pvalues < α]

# Part 1.4

# Use the stepwise selection procedure with the BIC penalty
stepwise_model <- step(model, k = log(nrow(concrete)), direction = "both", trace = 0)
stepwise_model
summary(stepwise_model)

# Part 1.5
new_concrete_mix <- data.frame(Cement = 491, Blast.Furnace.Slag = 26,
                               Fly.Ash = 123, Water = 210,
                               Superplasticizer = 3.9, Coarse.Aggregate = 882,
                               Fine.Aggregate = 699, Age = 28)

prediction <- predict(stepwise_model, newdata = new_concrete_mix,
                      interval = "confidence", level = 0.95)

prediction