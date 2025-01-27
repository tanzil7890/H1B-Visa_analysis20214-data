# Load required libraries
library(readr)
library(cluster)
library(factoextra)
library(magrittr)
library(NbClust)

# Load the dataset
h1b <- read_csv("//Users//mohammadtanzilidrisi//Tanzil//h1B_analysis//H1B-Visa_analysis20214-data//data//H1_B-2014-K.csv")

# Convert relevant columns to numeric
h1b$WAGE_RATE_FROM <- as.numeric(h1b$LCA_CASE_WAGE_RATE_FROM)
h1b$EMPLOYER_POSTAL_CODE <- as.numeric(h1b$LCA_CASE_EMPLOYER_POSTAL_CODE)
h1b$EMP_DIFF <- as.numeric(h1b$EMP_DIFF)
h1b$APP_DIFF <- as.numeric(h1b$APP_DIFF)
h1b$TOTAL_WORKERS <- as.numeric(h1b$TOTAL_WORKERS)
h1b$YR_SOURCE <- as.numeric(h1b$YR_SOURCE_PUB_1)
h1b$NAICS_CODE <- as.numeric(h1b$LCA_CASE_NAICS_CODE)

# Scale the data
matstd.h1b <- scale(h1b[, c("WAGE_RATE_FROM", "EMPLOYER_POSTAL_CODE", "APP_DIFF", "EMP_DIFF", "TOTAL_WORKERS")])
rownames(matstd.h1b) <- h1b$LCA_CASE_JOB_TITLE

# Calculate distance matrix and perform hierarchical clustering
dist.h1b <- dist(matstd.h1b, method = "euclidean")
clush1b.nn <- hclust(dist.h1b, method = "single")

# Determine the optimal number of clusters using the Elbow method
fviz_nbclust(matstd.h1b, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

# Plot dendrogram
plot(as.dendrogram(clush1b.nn), ylab = "Distance between different job titles", 
     ylim = c(0, 10), main = "Dendrogram")

# Perform k-means clustering with 4 clusters
set.seed(123)
kmeans4.h1b <- kmeans(matstd.h1b, 4, nstart = 25)

# Display cluster membership
kmeans4.h1b

# Visualize clusters
fviz_cluster(kmeans4.h1b, data = matstd.h1b, pca = FALSE,
             ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal(),
             geom = "point", pointsize = 3, show.clust.cent = TRUE, repel = TRUE)

# Conduct PCA on scaled data
pca_h1b <- prcomp(matstd.h1b, scale = TRUE)
summary(pca_h1b)

# Extract the first two principal components
PC1 <- pca_h1b$x[, 1]
PC2 <- pca_h1b$x[, 2]
pca_h1b_df <- data.frame(Index = rownames(pca_h1b$x), PC1 = PC1, PC2 = PC2)

# Determine the optimal number of clusters for PCA-transformed data
fviz_nbclust(pca_h1b_df[, c("PC1", "PC2")], kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

# Perform k-means clustering on PCA-transformed data with 4 clusters
set.seed(123)
kmeans4.h1b_pca <- kmeans(pca_h1b_df[, c("PC1", "PC2")], 4, nstart = 25)

# Display cluster membership for PCA-transformed data
kmeans4.h1b_pca

# Visualize clusters for PCA-transformed data
fviz_cluster(kmeans4.h1b_pca, data = pca_h1b_df[, c("PC1", "PC2")],
             ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal(),
             geom = "point", pointsize = 3, show.clust.cent = TRUE, repel = TRUE)
