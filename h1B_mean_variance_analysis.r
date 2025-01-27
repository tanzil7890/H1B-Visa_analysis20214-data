# Load required libraries
library(readr)
library(corpcor)
library(Hotelling)
library(car)
library(biotools)
library(stats)

# Load the dataset
h1b <- read_csv("//Users//mohammadtanzilidrisi//Tanzil//h1B_analysis//H1B-Visa_analysis20214-data//data//H1_B-2014.csv")

# Convert relevant columns to numeric
h1b$WAGE_RATE_FROM <- as.numeric(h1b$LCA_CASE_WAGE_RATE_FROM)
h1b$EMPLOYER_POSTAL_CODE <- as.numeric(h1b$LCA_CASE_EMPLOYER_POSTAL_CODE)
h1b$EMP_DIFF <- as.numeric(h1b$EMP_DIFF)
h1b$APP_DIFF <- as.numeric(h1b$APP_DIFF)
h1b$TOTAL_WORKERS <- as.numeric(h1b$TOTAL_WORKERS)
h1b$YR_SOURCE <- as.numeric(h1b$YR_SOURCE_PUB_1)
h1b$NAICS_CODE <- as.numeric(h1b$LCA_CASE_NAICS_CODE)

# Calculate Euclidean distances
x <- dist(scale(h1b[, c("WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "YR_SOURCE", "NAICS_CODE")], center = FALSE))

# Extract numeric columns
num_cols <- h1b[, c("WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "YR_SOURCE", "NAICS_CODE")]

# Calculate column means and covariance matrix
cm <- colMeans(num_cols)
S <- cov(num_cols)

# Perform univariate mean analysis using T-tests
with(data = h1b, t.test(WAGE_RATE_FROM[STATUS == "CERTIFIED"], WAGE_RATE_FROM[STATUS == "REJECTED"], var.equal = TRUE))
with(data = h1b, t.test(EMPLOYER_POSTAL_CODE[STATUS == "CERTIFIED"], EMPLOYER_POSTAL_CODE[STATUS == "REJECTED"], var.equal = TRUE))
with(data = h1b, t.test(APP_DIFF[STATUS == "CERTIFIED"], APP_DIFF[STATUS == "REJECTED"], var.equal = TRUE))
with(data = h1b, t.test(EMP_DIFF[STATUS == "CERTIFIED"], EMP_DIFF[STATUS == "REJECTED"], var.equal = TRUE))
with(data = h1b, t.test(TOTAL_WORKERS[STATUS == "CERTIFIED"], TOTAL_WORKERS[STATUS == "REJECTED"], var.equal = TRUE))
with(data = h1b, t.test(YR_SOURCE[STATUS == "CERTIFIED"], YR_SOURCE[STATUS == "REJECTED"], var.equal = TRUE))
with(data = h1b, t.test(NAICS_CODE[STATUS == "CERTIFIED"], NAICS_CODE[STATUS == "REJECTED"], var.equal = TRUE))

# Perform univariate variance analysis using Levene's test
leveneTest(WAGE_RATE_FROM ~ STATUS, data = h1b)
leveneTest(EMPLOYER_POSTAL_CODE ~ STATUS, data = h1b)
leveneTest(APP_DIFF ~ STATUS, data = h1b)
leveneTest(EMP_DIFF ~ STATUS, data = h1b)
leveneTest(TOTAL_WORKERS ~ STATUS, data = h1b)
leveneTest(YR_SOURCE ~ STATUS, data = h1b)
leveneTest(NAICS_CODE ~ STATUS, data = h1b)

# Perform ANOVA for each variable
summary(aov(WAGE_RATE_FROM ~ STATUS))
summary(aov(EMPLOYER_POSTAL_CODE ~ STATUS))
summary(aov(APP_DIFF ~ STATUS))
summary(aov(EMP_DIFF ~ STATUS))
summary(aov(TOTAL_WORKERS ~ STATUS))
summary(aov(YR_SOURCE ~ STATUS))
summary(aov(NAICS_CODE ~ STATUS))

# Perform multivariate mean analysis using Hotelling's T-squared test
t2testh1b <- hotelling.test(WAGE_RATE_FROM + EMPLOYER_POSTAL_CODE + APP_DIFF + EMP_DIFF + TOTAL_WORKERS + YR_SOURCE + NAICS_CODE ~ STATUS, data = h1b)

# Perform multivariate variance analysis using Mahalanobis distance
h1b_MD <- mahalanobis(num_cols, cm, S)

# Add p-values for Mahalanobis distance
h1b$pvalues <- pchisq(h1b_MD, df = 3, lower.tail = FALSE)

# Perform Box's M test for equality of covariance matrices
boxM(h1b[, c("WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "YR_SOURCE", "NAICS_CODE")], STATUS)

# Perform MANOVA
summary(manova(as.matrix(h1b[, c("WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "YR_SOURCE", "NAICS_CODE")]) ~ STATUS))
