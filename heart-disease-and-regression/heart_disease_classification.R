source("my.prediction.stats.R")
source("wrappers.R")
library(boot)
library(pROC)
library(rpart)

heart_train <- read.csv("heart_train.csv")

# Part 2.1
# Use cross-validation with 10 folds and 5,000 repetitions
cv <- learn.tree.cv(HD ~ ., data = heart_train, nfolds = 10, m = 5000)

# Obtain the optimal pruning parameter "best.cp" from the cross-validation
best_cp <- cv$best.cp
cat(paste("Best cp:", best_cp))

# Plot cv results
plot.tree.cv(cv)

# Prune the tree using the optimal pruning parameter "best.cp"
pruned_tree <- prune.rpart(tree = cv, cp = cv$best.cp)
pruned_tree

# Part 2.2
# Plot the tree
plot(cv$best.tree)

# Summary of the tree structure
text(cv$best.tree, pretty = 12)

# Part 2.5
# Fit logistic regression model
heart_train$HD <- ifelse(heart_train$HD == "Y", 1, 0)
logistic_model <- glm(HD ~ ., data = heart_train, family = binomial)

# Stepwise selection using BIC
stepwise_selection <- step(logistic_model,
    direction = "both", trace = 0,
    k = log(nrow(heart_train))
)

# Summary of the final logistic regression model
summary(stepwise_selection)

# Part 2.7
# Load the test data
heart_test <- read.csv("heart_test.csv")
heart_test$HD <- factor(heart_test$HD, levels = c("N", "Y"))

# Predict using the decision tree
tree_pred <- predict(cv$best.tree, newdata = heart_test, type = "prob")[, 2]

# Predict using the logistic regression model
logistic_pred <- predict(stepwise_selection,
    newdata = heart_test,
    type = "response"
)

# Compute prediction statistics
tree_stats <- my.pred.stats(tree_pred, heart_test$HD)
logistic_stats <- my.pred.stats(logistic_pred, heart_test$HD)

# Part 2.8
# Calculate the odds of having heart disease for the 60th patient in the test dataset
patient_60 <- heart_test[60, ]

# Compute probabilities using the tree model found using cross-validation for the 60th patient
tree_prob_60 <- predict(cv$best.tree, newdata = patient_60, type = "prob")[, "Y"]

# Compute probabilities using the step-wise logistic regression model for the 60th patient
logistic_prob_60 <- predict(stepwise_selection, newdata = patient_60, type = "response")

# Compute the odds of having heart disease for the 60th patient for both models
tree_odds_60 <- tree_prob_60 / (1 - tree_prob_60)
tree_odds_60

logistic_odds_60 <- logistic_prob_60 / (1 - logistic_prob_60)
logistic_odds_60

# Part 2.9
# Bootstrap function for logistic regression probabilities
logistic_prob <- function(data, indices, model, patient_row) {
  sample_data <- data[indices, ]  # Bootstrap sample of the dataset
  # Predict probability for the specific patient based on bootstrapped sample
  pred <- predict(model, newdata = sample_data[patient_row, , drop = FALSE], type = "response")
  return(pred)
}

# Perform bootstrapping for the 65th patient
boot_65 <- boot(data = heart_test, statistic = logistic_prob, R = 5000, 
                model = stepwise_selection, patient_row = 65)

# Perform bootstrapping for the 66th patient
boot_66 <- boot(data = heart_test, statistic = logistic_prob, R = 5000, 
                model = stepwise_selection, patient_row = 66)

# Calculate confidence intervals for both patients
ci_65 <- boot.ci(boot_65, conf = 0.95, type = "bca")
ci_66 <- boot.ci(boot_66, conf = 0.95, type = "bca")

# Display the confidence intervals
print(ci_65)
print(ci_66)