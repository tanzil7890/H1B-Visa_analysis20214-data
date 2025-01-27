# Load required libraries
library(readr)
library(factoextra)
library(psych)
library(corrplot)

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

# Calculate the correlation matrix
cor_matrix <- cor(h1b[, c("WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "YR_SOURCE", "NAICS_CODE")])

# Perform PCA on the dataset
h1b_pca <- prcomp(h1b[, c("WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS", "YR_SOURCE", "NAICS_CODE")], scale = TRUE)

# Summary of PCA to determine the number of PCs to retain
summary(h1b_pca)

# Calculate eigenvalues for PCA components
eigen_h1b <- h1b_pca$sdev^2
names(eigen_h1b) <- paste("PC", 1:length(eigen_h1b), sep = "")

# Calculate proportion of variance explained and cumulative variance
propvar <- eigen_h1b / sum(eigen_h1b)
cumvar_h1b <- cumsum(propvar)

# Create a matrix summarizing eigenvalues, proportion of variance, and cumulative variance
matlambdas <- rbind(eigen_h1b, propvar, cumvar_h1b)
rownames(matlambdas) <- c("Eigenvalues", "Prop. variance", "Cum. prop. variance")

# Extract the rotation (weights for each original variable) for each PC
pc_rotations <- h1b_pca$rotation

# Extract scores for each row based on PCs
pc_scores <- h1b_pca$x

# Add PCA scores to the dataset along with visa status
h1btyp_pca <- cbind(data.frame(STATUS = h1b$STATUS), pc_scores)

# Calculate means of PCA scores grouped by visa status
tabmeansPC <- aggregate(h1btyp_pca[, 2:ncol(h1btyp_pca)], by = list(status = h1b$STATUS), mean)

# Calculate standard deviations of PCA scores grouped by visa status
tabsdsPC <- aggregate(h1btyp_pca[, 2:ncol(h1btyp_pca)], by = list(status = h1b$STATUS), sd)

# Plot Scree Diagram
plot(eigen_h1b, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree Diagram")

# Plot Log(eigenvalue) Diagram
plot(log(eigen_h1b), xlab = "Component number", ylab = "log(Component variance)", type = "l", main = "Log(eigenvalue) Diagram")

# Plot PCA variance contributions
plot(h1b_pca)

# Visualize contributions of variables to the PCs
fviz_pca_var(h1b_pca, col.var = "cos2",
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"),
             repel = TRUE)

# Pairwise scatterplots of PCA scores grouped by visa status
pairs.panels(h1b_pca$x, gap = 0, bg = c("red", "blue")[h1b$STATUS], pch = 21)

# Boxplot of each PC score grouped by visa status
sapply(1:7, function(i) {
  plot(h1b$STATUS, h1b_pca$x[, i], xlab = paste("PC", i, sep = ""), ylab = "Visa Status")
})
