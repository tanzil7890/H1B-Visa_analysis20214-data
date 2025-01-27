# Load required libraries
library(psych)
library(readr)

# Load the dataset
h1b <- read.csv("//Users//mohammadtanzilidrisi//Tanzil//h1B_analysis//H1B-Visa_analysis20214-data//data//H1_B-2014.csv", row.names = 1)

# Convert relevant columns to numeric
h1b$WAGE_RATE_FROM <- as.numeric(h1b$LCA_CASE_WAGE_RATE_FROM)
h1b$EMPLOYER_POSTAL_CODE <- as.numeric(h1b$LCA_CASE_EMPLOYER_POSTAL_CODE)
h1b$EMP_DIFF <- as.numeric(h1b$EMP_DIFF)
h1b$APP_DIFF <- as.numeric(h1b$APP_DIFF)
h1b$TOTAL_WORKERS <- as.numeric(h1b$TOTAL_WORKERS)
h1b$YR_SOURCE <- as.numeric(h1b$YR_SOURCE_PUB_1)
h1b$NAICS_CODE <- as.numeric(h1b$LCA_CASE_NAICS_CODE)

# Perform parallel analysis to determine the number of factors
fa.parallel(h1b[, c("WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "YR_SOURCE", "NAICS_CODE")])

# Perform factor analysis with 2 factors
fit.pc <- principal(h1b[, c("WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "YR_SOURCE", "NAICS_CODE")], nfactors = 2, rotate = "varimax")

# Display eigenvalues and loadings
round(fit.pc$values, 3)
fit.pc$loadings

# Check communality for each variable
fit.pc$communality

# Extract factor scores
fit.pc$scores

# Visualize the relationship between variables and factors
fa.diagram(fit.pc)

# Plot correlations within factors
fa.plot(fit.pc)

# Perform Very Simple Structure (VSS) analysis
vss(h1b[, c("WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "YR_SOURCE", "NAICS_CODE")])

# Perform factor analysis with 3 factors
fit.pc_2 <- principal(h1b[, c("WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "YR_SOURCE", "NAICS_CODE")], nfactors = 3, rotate = "varimax")

# Display eigenvalues and loadings for 3 factors
round(fit.pc_2$values, 3)
fit.pc_2$loadings

# Check communality for each variable for 3 factors
fit.pc_2$communality

# Extract factor scores for 3 factors
fit.pc_2$scores

# Visualize the relationship between variables and factors for 3 factors
fa.diagram(fit.pc_2)
