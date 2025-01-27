# Load required libraries
library(MASS)  # For LDA
library(ggplot2)  # For visualization
library(memisc)  # For advanced data handling
library(ROCR)  # For performance metrics
library(dplyr)  # For data manipulation
library(klaR)  # For partition plots

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

# Convert relevant columns to factors
h1b$STATUS <- as.factor(h1b$STATUS)
h1b$VISA_CLASS <- as.factor(h1b$VISA_CLASS)

# Select relevant columns for analysis
h1b_df <- h1b[, c("STATUS", "VISA_CLASS", "WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", 
                  "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "YR_SOURCE", "NAICS_CODE")]

# Split the dataset into training and test sets
set.seed(42)
smp_size_raw <- floor(0.75 * nrow(h1b_df))
train_ind_raw <- sample(nrow(h1b_df), size = smp_size_raw)
train_raw.df <- as.data.frame(h1b_df[train_ind_raw, ])
test_raw.df <- as.data.frame(h1b_df[-train_ind_raw, ])

# Build the LDA model using the training dataset
h1b_lda <- lda(formula = train_raw.df$VISA_CLASS ~ ., data = train_raw.df)

# Display the LDA model summary
summary(h1b_lda)

# Plot residuals to visualize observations in the discriminant axes
plot(h1b_lda)

# Generate partition plots to examine class separation
partimat(VISA_CLASS ~ STATUS + VISA_CLASS + WAGE_RATE_FROM + EMPLOYER_POSTAL_CODE + 
           APP_DIFF + EMP_DIFF + TOTAL_WORKERS + YR_SOURCE, 
         data = train_raw.df, method = "lda")

# Perform predictions on the test dataset using the LDA model
h1b.lda.predict <- predict(h1b_lda, newdata = test_raw.df)

# Extract predicted class labels
predicted_labels <- h1b.lda.predict$class

# Calculate the accuracy of the model
actual_labels <- test_raw.df$VISA_CLASS
accuracy <- mean(predicted_labels == actual_labels)

# Display the accuracy of the model
cat("Accuracy:", accuracy, "\n")

# Additional analysis: Examine the LDA scores for the test dataset
h1b.lda.predict$x

# Visualize variable contributions to discriminant axes
partimat(VISA_CLASS ~ STATUS + WAGE_RATE_FROM + EMPLOYER_POSTAL_CODE + 
           APP_DIFF + EMP_DIFF, data = train_raw.df, method = "lda")
