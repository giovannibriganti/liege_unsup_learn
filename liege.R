set.seed(123) # For reproducibility

# Number of patients
n <- 1000 

# Define clusters of patients
clusters <- 3

# Generating random data for each cluster
weakness <- unlist(lapply(1:clusters, function(i) rnorm(n/clusters, sample(5:15, 1), sample(1:3, 1))))
cognition <- unlist(lapply(1:clusters, function(i) rnorm(n/clusters, sample(20:60, 1), sample(5:10, 1))))
coordination <- unlist(lapply(1:clusters, function(i) rnorm(n/clusters, sample(40:80, 1), sample(5:10, 1))))

# Combine these variables into a data frame
patients <- data.frame(weakness, cognition, coordination)

# Print the first few rows of the data
head(patients)


ggplot(patients, aes(weakness, cognition)) + 
  geom_point() +
  ggtitle("K-Means Clustering")


patients_scaled <- scale(patients)


# Apply k-means with 3 clusters
set.seed(123) 
kmeans_result <- kmeans(patients_scaled, centers=3)

# Add cluster results to the data frame
patients$kmeans_cluster <- as.factor(kmeans_result$cluster)

# Plot the clusters
library(ggplot2)
ggplot(patients, aes(weakness, cognition, color = kmeans_cluster)) + 
  geom_point() +
  ggtitle("K-Means Clustering")


ggplot(patients, aes(weakness, coordination, color = kmeans_cluster)) + 
  geom_point() +
  ggtitle("K-Means Clustering")

ggplot(patients, aes(cognition, coordination, color = kmeans_cluster)) + 
  geom_point() +
  ggtitle("K-Means Clustering")


# Apply hierarchical clustering
hc <- hclust(dist(patients_scaled))

# Cut the dendrogram at 3 clusters
cluster_assignments <- cutree(hc, 3)

# Add cluster results to the data frame
patients$hclust_cluster <- as.factor(cluster_assignments)

# Plot the dendrogram
plot(hc)
abline(h=10, col="red") # Adjust the line position according to your dendrogram.

# Plot the clusters
ggplot(patients, aes(weakness, cognition, color = hclust_cluster)) + 
  geom_point() +
  ggtitle("Hierarchical Clustering")



# Perform PCA
pca_result <- prcomp(patients_scaled, center = TRUE, scale. = TRUE)

# Scree plot to visualize the variance explained by each principal component
plot(pca_result, type = "lines")

# Biplot to visualize the samples and the variable projections
biplot(pca_result, scale = 0)

