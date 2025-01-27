# Load required libraries
library(readr)
library(dplyr)

# Load the main dataset (H1_B-2014)
# Replace the path with the actual location of your file
h1b <- read_csv("path/to/H1_B-2014.csv")

# Load the NAICS data file
# Replace the path with the actual location of your file
naics_data <- read_csv("path/to/NAICS_data.csv")

# Load the occupations data file
# Replace the path with the actual location of your file
occupations <- read_csv("path/to/occupations.csv")

# Display the structure of the datasets to understand their format
str(h1b)
str(naics_data)
str(occupations)

# Check for missing values in the main dataset
sum(is.na(h1b))

# Clean the data by removing rows with missing dependent variables
h1b <- h1b %>% filter(!is.na(STATUS))

# Convert relevant columns to appropriate data types
h1b$APP_DIFF <- as.numeric(h1b$APP_DIFF)  # Convert application difference to numeric
h1b$EMP_DIFF <- as.numeric(h1b$EMP_DIFF)  # Convert employment difference to numeric
h1b$TOTAL_WORKERS <- as.numeric(h1b$TOTAL_WORKERS)  # Convert total workers to numeric
h1b$LCA_CASE_WAGE_RATE_FROM <- as.numeric(h1b$LCA_CASE_WAGE_RATE_FROM)  # Convert wage rate to numeric

# Merge NAICS data with the main dataset based on the NAICS code
h1b <- merge(h1b, naics_data, by.x = "LCA_CASE_NAICS_CODE", by.y = "NAICS_CODE", all.x = TRUE)

# Merge occupations data with the main dataset based on the SOC code
h1b <- merge(h1b, occupations, by.x = "LCA_CASE_SOC_CODE", by.y = "SOC Code", all.x = TRUE)

# Summarize the data to get an overview of the dependent variable (STATUS)
table(h1b$STATUS)

# Calculate the mean and variance of numeric variables grouped by STATUS
h1b %>%
  group_by(STATUS) %>%
  summarise(
    Mean_Wage = mean(LCA_CASE_WAGE_RATE_FROM, na.rm = TRUE),
    Variance_Wage = var(LCA_CASE_WAGE_RATE_FROM, na.rm = TRUE),
    Mean_Total_Workers = mean(TOTAL_WORKERS, na.rm = TRUE),
    Variance_Total_Workers = var(TOTAL_WORKERS, na.rm = TRUE)
  )

# Visualize the distribution of the dependent variable (STATUS)
ggplot(h1b, aes(x = STATUS)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Application Status", x = "Status", y = "Count")

# Analyze correlations between independent variables
correlation_matrix <- cor(h1b[, c("APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "LCA_CASE_WAGE_RATE_FROM")], use = "complete.obs")

# Visualize the correlation matrix
library(corrplot)
corrplot(correlation_matrix, method = "color")

# Perform t-tests for independent variables grouped by STATUS
t.test(h1b$LCA_CASE_WAGE_RATE_FROM ~ h1b$STATUS)
t.test(h1b$EMP_DIFF ~ h1b$STATUS)
t.test(h1b$APP_DIFF ~ h1b$STATUS)

# Perform ANOVA to analyze the variance of LCA_CASE_WAGE_RATE_FROM by STATUS
anova_results <- aov(LCA_CASE_WAGE_RATE_FROM ~ STATUS, data = h1b)
summary(anova_results)

# Save the cleaned dataset for further analysis
write_csv(h1b, "path/to/cleaned_H1_B-2014.csv")
