# Load required libraries
library(factoextra)  # For PCA visualization
library(FactoMineR)  # For PCA analysis
library(psych)  # For Factor Analysis

# Load the dataset
# Replace the path with the actual location of your file
h1b <- read.csv("//Users//mohammadtanzilidrisi//Tanzil//h1B_analysis//H1B-Visa_analysis20214-data//data//H1_B-2014-K.csv", row.names = 1)

# Convert relevant columns to numeric
h1b$WAGE_RATE_FROM <- as.numeric(h1b$LCA_CASE_WAGE_RATE_FROM)
h1b$EMPLOYER_POSTAL_CODE <- as.numeric(h1b$LCA_CASE_EMPLOYER_POSTAL_CODE)
h1b$EMP_DIFF <- as.numeric(h1b$EMP_DIFF)
h1b$APP_DIFF <- as.numeric(h1b$APP_DIFF)
h1b$TOTAL_WORKERS <- as.numeric(h1b$TOTAL_WORKERS)
h1b$YR_SOURCE <- as.numeric(h1b$YR_SOURCE_PUB_1)
h1b$NAICS_CODE <- as.numeric(h1b$LCA_CASE_NAICS_CODE)

# Convert VISA_CLASS into numeric for regression analysis
h1b$VISA_CLASS_NUM <- as.numeric(factor(h1b$VISA_CLASS, levels = unique(h1b$VISA_CLASS)))

# Subset numeric columns for regression
h1b_num <- h1b[, c("WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "YR_SOURCE", "NAICS_CODE")]

# Perform multiple regression with VISA_CLASS_NUM as target variable
fit <- lm(VISA_CLASS_NUM ~ WAGE_RATE_FROM + EMPLOYER_POSTAL_CODE + APP_DIFF + EMP_DIFF + TOTAL_WORKERS + YR_SOURCE + NAICS_CODE, data = h1b)
summary(fit)  # Display regression results

# Remove columns with high p-values and perform regression again
fit_1 <- lm(VISA_CLASS_NUM ~ WAGE_RATE_FROM + EMPLOYER_POSTAL_CODE + TOTAL_WORKERS + YR_SOURCE + NAICS_CODE, data = h1b)
summary(fit_1)  # Display regression results

# Analyze residuals with pair plots
library(GGally)
ggpairs(data = h1b_num, title = "H1B Data")

# Calculate confidence intervals for regression coefficients
confint(fit_1, level = 0.95)

# Predict fitted values from the regression model
fitted(fit_1)

# Calculate residuals
residuals(fit_1)

# Plot residual diagnostics for the regression model
plot(fit_1)

# Calculate R-squared value for model accuracy
summary(fit_1)$r.squared

# Perform PCA on the numeric data
h1b_pca <- prcomp(h1b_num, scale. = TRUE)
summary(h1b_pca)  # Display PCA summary

# Scree plot to determine the number of components to retain
eigen_h1b <- h1b_pca$sdev^2
names(eigen_h1b) <- paste("PC", 1:7, sep = "")
plot(eigen_h1b, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree Diagram")

# Perform regression using the first two principal components
PC1 <- h1b_pca$x[, 1]
PC2 <- h1b_pca$x[, 2]
pc_data <- data.frame(PC1, PC2)
pc_data$visa_class_num <- h1b$VISA_CLASS_NUM
fit_pc <- lm(visa_class_num ~ PC1 + PC2, data = pc_data)
summary(fit_pc)

# Perform Factor Analysis to determine factors
fa.parallel(h1b_num)  # Determine the number of factors
fit.pc <- principal(h1b_num, nfactors = 2, rotate = "varimax")
fit.pc$loadings  # Display factor loadings

# Perform regression using the first two factors
loadings <- fit.pc$scores[, c("RC1", "RC2")]
loadings_df <- as.data.frame(loadings)
loadings_df$visa_class_num <- h1b$VISA_CLASS_NUM
fit_rc <- lm(visa_class_num ~ RC1 + RC2, data = loadings_df)
summary(fit_rc)
