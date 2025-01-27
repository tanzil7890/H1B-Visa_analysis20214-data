# Load required libraries
library(ggplot2)  # For visualization
library(cowplot)  # For combining plots
library(caret)  # For model evaluation
library(e1071)  # For confusion matrix
library(pROC)  # For ROC curve analysis

# Load the dataset
# Replace the path with the actual location of your file
h1b <- read.csv("//Users//mohammadtanzilidrisi//Tanzil//h1B_analysis//H1B-Visa_analysis20214-data//data//H1B.csv", row.names = NULL)

# Convert relevant columns to numeric
h1b$WAGE_RATE_FROM <- as.numeric(h1b$LCA_CASE_WAGE_RATE_FROM)
h1b$EMPLOYER_POSTAL_CODE <- as.numeric(h1b$LCA_CASE_EMPLOYER_POSTAL_CODE)
h1b$EMP_DIFF <- as.numeric(h1b$EMP_DIFF)
h1b$APP_DIFF <- as.numeric(h1b$APP_DIFF)
h1b$TOTAL_WORKERS <- as.numeric(h1b$TOTAL_WORKERS)
h1b$YR_SOURCE <- as.numeric(h1b$YR_SOURCE_PUB_1)
h1b$NAICS_CODE <- as.numeric(h1b$LCA_CASE_NAICS_CODE)

# Convert the target variable to a factor
h1b$STATUS <- as.factor(h1b$STATUS)

# Select relevant columns for the logistic regression
h1b_df <- h1b[, c("STATUS", "WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", 
                  "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "YR_SOURCE", "NAICS_CODE")]

# Build logistic regression model
logistic_reg <- glm(STATUS ~ ., data = h1b_df, family = "binomial")

# Display summary of the logistic regression model
summary(logistic_reg)

# Calculate Pseudo R-squared
ll.null <- logistic_reg$null.deviance / -2
ll.proposed <- logistic_reg$deviance / -2
pseudo_r_squared <- (ll.null - ll.proposed) / ll.null

# Calculate p-value for the Pseudo R-squared
p_value <- 1 - pchisq(2 * (ll.proposed - ll.null), df = (length(logistic_reg$coefficients) - 1))

# Analyze residuals
residuals <- residuals(logistic_reg, type = "response")

# Plot diagnostic plots for residual analysis
plot(logistic_reg)

# Predict probabilities of the target variable
predicted.data <- data.frame(
  probability.of.status = logistic_reg$fitted.values,
  status = h1b_df$STATUS
)

# Sort the predicted probabilities
predicted.data <- predicted.data[order(predicted.data$probability.of.status, decreasing = FALSE), ]
predicted.data$rank <- 1:nrow(predicted.data)

# Visualize predicted probabilities
ggplot(data = predicted.data, aes(x = rank, y = probability.of.status)) +
  geom_point(aes(color = status), alpha = 1, shape = 4, stroke = 2) +
  xlab("Index") +
  ylab("Predicted probability of Status")

# Predict values for the dataset
pdata <- predict(logistic_reg, newdata = h1b_df, type = "response")

# Convert probabilities to class labels
pdataF <- as.factor(ifelse(pdata > 0.5, "CERTIFIED", "REJECTED"))

# Generate confusion matrix to evaluate the model
conf_matrix <- confusionMatrix(pdataF, h1b_df$STATUS)

# Display accuracy, sensitivity, and specificity
accuracy <- conf_matrix$overall["Accuracy"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
balanced_accuracy <- conf_matrix$byClass["Balanced Accuracy"]

# Display model evaluation metrics
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Balanced Accuracy:", balanced_accuracy, "\n")
