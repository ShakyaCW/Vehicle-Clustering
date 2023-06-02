# 1st Subtask

library(ggplot2)
library(readxl)
library(dplyr)

# Import data from excel file
vehicle_data <- read_excel("vehicles.xlsx")
vehicle_data <- vehicle_data[, -1]

# Remove any rows with missing values in the dataframe
vehicle_data <- na.omit(vehicle_data)

variables <- vehicle_data[,-19] #stores attributes
class_variable <- vehicle_data$Class #class types

# Print the number of rows before removing outliers
vehicle_rows <- nrow(vehicle_data)
cat("number of rows before removing outliers = ", vehicle_rows)

outliers_in_columns <- c() # empty vector to store outliers in each column
for(i in 1:ncol(variables)){
  outlier_index <- variables[[i]] %in% boxplot.stats(variables[[i]])$out
  outlier_values <- variables[[i]][outlier_index]
  outliers_in_columns <- c(outliers_in_columns, outlier_values)
}

eleminate <- unique(outliers_in_columns) # remove the duplicated rows
eleminate <- sort(eleminate) # sort the outlire rows in order
good_vehicle_data <- vehicle_data[-eleminate,] # store the final rows after removing the outlires for attributes

variables <- distinct(variables)

# Removing outliers
no_outliers <- variables %>%
  filter_all(all_vars(!. %in% boxplot.stats(.)$out))

# Print the number of rows after removing outliers
n_rows <- nrow(good_vehicle_data)
cat("number of rows remaining after removing outliers = ", n_rows)

# Before scaling
boxplot(no_outliers)

# Check for any points outside of the whiskers in the boxplot
points_outside <- boxplot(no_outliers, plot = FALSE)$out
if(length(points_outside) > 0) {
  cat("There are points outside of the whiskers in the boxplot.")
} else {
  cat("There are no points outside of the whiskers in the boxplot.")
}

# function to calculate the normalized data
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

good_data_norm <- as.data.frame(lapply(good_vehicle_data[,-19], normalize))# normalized data
vehicle_data_no_outliers <- data.frame(scale(good_data_norm)) # scale the normalized data

no_outliers <- as.data.frame(lapply(no_outliers[,], normalize))# normalized data
no_outliers <- data.frame(scale(no_outliers)) # scale the normalized data

# After Scaling
boxplot(no_outliers)

# Load required libraries
library(NbClust)
library(cluster)
library(ggplot2)
library(factoextra)

# 1. NBclust
set.seed(123)
nb <- NbClust(no_outliers, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")

# Using factoextra
fviz_nbclust(no_outliers, kmeans, method = "silhouette")

nbClust_Clsters <- kmeans(no_outliers,2) #find new clusters with the number of clusters suggested by NbClust
nbClust_Clsters
demo_Clusters <- kmeans(no_outliers, centers =3, nstart = 25)

# plot the clusters accordingly
fviz_cluster(list(data = no_outliers, cluster = demo_Clusters$cluster), geom = "point", stand = FALSE, ellipse.type = "convex", ggtheme = theme_minimal())

# 2. Elbow method
k = 1:10 # number of possible clusters
set.seed(42)
WSS = sapply(k, function(k) {kmeans(no_outliers, centers=k)$tot.withinss})
plot(k, WSS, type = "b", pch = 19, frame=FALSE, xlab= "Number of k", ylab="Within sum of
squares")
fviz_nbclust(no_outliers, kmeans, method = "wss")+
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

demo_Clusters <- kmeans(no_outliers, centers =2, nstart = 25)

# plot the clusters accordingly
fviz_cluster(list(data = no_outliers, cluster = demo_Clusters$cluster), geom = "point", stand = FALSE, ellipse.type = "convex", ggtheme = theme_minimal())


# 3. Gap statistic method
set.seed(123)
gap_stat <- clusGap(no_outliers, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

demo_Clusters <- kmeans(no_outliers, centers =3, nstart = 25)

# plot the clusters accordingly
fviz_cluster(list(data = no_outliers, cluster = demo_Clusters$cluster), geom = "point", stand = FALSE, ellipse.type = "convex", ggtheme = theme_minimal())

# 4. Silhouette method
library(cluster)

# Compute the silhouette width for different number of clusters
sil_width <- c()
for (i in 2:10) {
  kmeans_fit <- kmeans(no_outliers, centers = i, nstart = 25)
  kmeans_sil_width <- silhouette(kmeans_fit$cluster, dist(no_outliers))
  sil_width[i] <- mean(kmeans_sil_width[, 3])
}


# Determine the optimal number of clusters
nb_Clusters <- which.max(sil_width)
cat("Number of Clusters =", nb_Clusters, "Gives the highest average silhouette width")

# Plot the silhouette width for each number of clusters
fviz_nbclust(no_outliers, kmeans, method = "silhouette") +
  geom_vline(xintercept = nb_Clusters, linetype = 2) +
  labs(title = "Silhouette method")



# K-means analysis for each k attempt.

# Perform k-means clustering with k=2

k <- 2
kc2 <- kmeans(no_outliers, centers = k, nstart = 25)

# plot the clusters accordingly
fviz_cluster(list(data = no_outliers, cluster = kc2$cluster), 
             geom = "point", stand = FALSE, ellipse.type = "convex", ggtheme = theme_minimal())

kc2_centers <- kc2$centers
kc2_bss <- kc2$betweenss
kc2_tss <- kc2$tot.withinss
kc2_wss <- kc2$withinss

cat("K-Means Clustering with k = 2\n")
cat("Centers:\n")
print(kc2_centers)
cat("Clustered Results:\n")
print(kc2$cluster)
cat("Between-cluster sum of squares (BSS): ", kc2_bss, "\n")
cat("Total sum of squares (TSS): ", kc2_tss, "\n")
cat("Ratio of BSS to TSS: ", kc2_bss/kc2_tss, "\n")
cat("Within-cluster sum of squares (WSS): ", kc2_wss, "\n\n")
table(good_vehicle_data$Class,kc2$cluster)#Confusion Matrix

# Perform k-means clustering with k=3

k <- 3
kc3 <- kmeans(no_outliers, centers = k, nstart = 25)

# plot the clusters accordingly
fviz_cluster(list(data = no_outliers, cluster = kc3$cluster), 
             geom = "point", stand = FALSE, ellipse.type = "convex", ggtheme = theme_minimal())

kc3_centers <- kc3$centers
kc3_bss <- kc3$betweenss
kc3_tss <- kc3$tot.withinss
kc3_wss <- kc3$withinss

cat("K-means clustering with k=3:\n")
cat("Cluster centers:\n")
print(kc3_centers)
cat("Cluster membership:\n")
print(kc3$cluster)
cat("Between-cluster sum of squares (BSS): ", kc3_bss, "\n")
cat("Total sum of squares (TSS): ", kc3_tss, "\n")
cat("Ratio of BSS to TSS: ", kc3_bss/kc3_tss, "\n")
cat("Within-cluster sum of squares (WSS): ", kc3_wss, "\n\n")
table(good_vehicle_data$Class,kc3$cluster)#Confusion Matrix

# Perform k-means clustering with k=4

k <- 4
kc4 <- kmeans(no_outliers, centers = k, nstart = 25)

# plot the clusters accordingly
fviz_cluster(list(data = no_outliers, cluster = kc4$cluster), geom = "point", stand = FALSE, ellipse.type = "convex", ggtheme = theme_minimal())

kc4_centers <- kc4$centers
kc4_bss <- kc4$betweenss
kc4_tss <- kc4$tot.withinss
kc4_wss <- kc4$withinss

cat("K-means clustering with k=4:\n")
cat("Cluster centers:\n")
print(kc4_centers)
cat("Cluster membership:\n")
print(kc4$cluster)
cat("Between-cluster sum of squares (BSS): ", kc4_bss, "\n")
cat("Total sum of squares (TSS): ", kc4_tss, "\n")
cat("Ratio of BSS to TSS: ", kc4_bss/kc4_tss, "\n")
cat("Within-cluster sum of squares (WSS): ", kc4_wss, "\n\n")
table(good_vehicle_data$Class,kc4$cluster)#Confusion Matrix

# Compute silhouette width for k = 2
sil_width_2 <- silhouette(kc2$cluster, dist(no_outliers))
avg_sil_width_2 <- mean(sil_width_2[,3])
cat("Average Silhouette Width Score for k = 2 is", avg_sil_width_2, "\n")

# Compute silhouette width for k = 3
sil_width_3 <- silhouette(kc3$cluster, dist(no_outliers))
avg_sil_width_3 <- mean(sil_width_3[,3])
cat("Average Silhouette Width Score for k = 3 is", avg_sil_width_3, "\n")

# Compute silhouette width for k = 4
sil_width_4 <- silhouette(kc4$cluster, dist(no_outliers))
avg_sil_width_4 <- mean(sil_width_4[,3])
cat("Average Silhouette Width Score for k = 4 is", avg_sil_width_4, "\n")

# Compute the silhouette width for different number of clusters
sil_width <- c()
for (i in 2:10) {
  kmeans_fit <- kmeans(no_outliers, centers = i, nstart = 25)
  kmeans_sil_width <- silhouette(kmeans_fit$cluster, dist(no_outliers))
  sil_width[i] <- mean(kmeans_sil_width[, 3])
}

# Plot silhouette width against number of clusters
plot(1:10, sil_width, type = "b", pch = 19,  
     xlab = "Number of Clusters", ylab = "Silhouette Width",
     main = "Silhouette Width vs Number of Clusters",)
axis(1, at = 1:10)




# 2nd SubTask
# Perform PCA analysis

library(FactoMineR)

# Standardize the data
vehicle_data_std <- scale(no_outliers, center = TRUE, scale = TRUE)

# Perform PCA
vehicle_pca <- PCA(vehicle_data_std, graph = FALSE)

# Show eigenvalues and eigenvectors
print(vehicle_pca$eig)

# Show cumulative scores per principal component
cum_sum <- cumsum(vehicle_pca$eig)/sum(vehicle_pca$eig)
plot(cum_sum, xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained")

# Create new dataset with principal components as attributes
vehicle_pca_data <- as.data.frame(predict(vehicle_pca, newdata = vehicle_data_std))

# Select principal components with cumulative score > 92%
n_components <- length(cum_sum[cum_sum > 0.92])
vehicle_pca_data <- vehicle_pca_data[, 1:n_components]

# 1. NBclust
set.seed(123)
nb <- NbClust(vehicle_pca_data, distance = "euclidean", 
              min.nc = 2, max.nc = 10, method = "kmeans", index = "all")

# Using factoextra
fviz_nbclust(vehicle_pca_data, kmeans, method = "silhouette")

nbClust_Clsters <- kmeans(vehicle_pca_data,2) #find new clusters with the number of clusters suggested by NbClust
nbClust_Clsters
demo_Clusters <- kmeans(vehicle_pca_data, centers =3, nstart = 25)

# plot the clusters accordingly
fviz_cluster(list(data = no_outliers, cluster = demo_Clusters$cluster), 
             geom = "point", stand = FALSE, ellipse.type = "convex", ggtheme = theme_minimal())

# 2. Elbow method
k = 1:10 # number of possible clusters
set.seed(42)
WSS = sapply(k, function(k) {kmeans(vehicle_pca_data, centers=k)$tot.withinss})
plot(k, WSS, type = "b", pch = 19, frame=FALSE, xlab= "Number of k", ylab="Within sum of
squares")
fviz_nbclust(vehicle_pca_data, kmeans, method = "wss")+
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

demo_Clusters <- kmeans(vehicle_pca_data, centers =2, nstart = 25)

# plot the clusters accordingly
fviz_cluster(list(data = vehicle_pca_data, cluster = demo_Clusters$cluster), 
             geom = "point", stand = FALSE, ellipse.type = "convex", ggtheme = theme_minimal())


# 3. Gap statistic method
set.seed(123)
gap_stat <- clusGap(vehicle_pca_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

demo_Clusters <- kmeans(vehicle_pca_data, centers =3, nstart = 25)

# plot the clusters accordingly
fviz_cluster(list(data = vehicle_pca_data, cluster = demo_Clusters$cluster), geom = "point", stand = FALSE, ellipse.type = "convex", ggtheme = theme_minimal())

# 4. Silhouette method
library(cluster)

# Compute the silhouette width for different number of clusters
sil_width <- c()
for (i in 2:10) {
  kmeans_fit <- kmeans(vehicle_pca_data, centers = i, nstart = 25)
  kmeans_sil_width <- silhouette(kmeans_fit$cluster, dist(vehicle_pca_data))
  sil_width[i] <- mean(kmeans_sil_width[, 3])
}

# Determine the optimal number of clusters
nb_Clusters <- which.max(sil_width)
cat("Number of Clusters =", nb_Clusters, "Gives the highest average silhouette width")

# Plot the silhouette width for each number of clusters
fviz_nbclust(vehicle_pca_data, kmeans, method = "silhouette") +
  geom_vline(xintercept = nb_Clusters, linetype = 2) +
  labs(title = "Silhouette method")


#             K-means analysis for each k attempt.

# Perform k-means clustering with k=2

k <- 2
kc2 <- kmeans(vehicle_pca_data, centers = k, nstart = 25)

# plot the clusters accordingly
fviz_cluster(list(data = vehicle_pca_data, cluster = kc2$cluster), geom = "point", stand = FALSE, ellipse.type = "convex", ggtheme = theme_minimal())

kc2_centers <- kc2$centers
kc2_bss <- kc2$betweenss
kc2_tss <- kc2$tot.withinss
kc2_wss <- kc2$withinss

cat("K-Means Clustering with k = 2\n")
cat("Centers:\n")
print(kc2_centers)
cat("Clustered Results:\n")
print(kc2$cluster)
cat("Between-cluster sum of squares (BSS): ", kc2_bss, "\n")
cat("Total sum of squares (TSS): ", kc2_tss, "\n")
cat("Ratio of BSS to TSS: ", kc2_bss/kc2_tss, "\n")
cat("Within-cluster sum of squares (WSS): ", kc2_wss, "\n\n")
table(good_vehicle_data$Class,kc2$cluster)#Confusion Matrix

# Perform k-means clustering with k=3

k <- 3
kc3 <- kmeans(vehicle_pca_data, centers = k, nstart = 25)

# plot the clusters accordingly
fviz_cluster(list(data = vehicle_pca_data, cluster = kc3$cluster), geom = "point", stand = FALSE, ellipse.type = "convex", ggtheme = theme_minimal())

kc3_centers <- kc3$centers
kc3_bss <- kc3$betweenss
kc3_tss <- kc3$tot.withinss
kc3_wss <- kc3$withinss

cat("K-means clustering with k=3:\n")
cat("Cluster centers:\n")
print(kc3_centers)
cat("Cluster membership:\n")
print(kc3$cluster)
cat("Between-cluster sum of squares (BSS): ", kc3_bss, "\n")
cat("Total sum of squares (TSS): ", kc3_tss, "\n")
cat("Ratio of BSS to TSS: ", kc3_bss/kc3_tss, "\n")
cat("Within-cluster sum of squares (WSS): ", kc3_wss, "\n\n")
table(good_vehicle_data$Class,kc3$cluster)#Confusion Matrix

# Perform k-means clustering with k=4

k <- 4
kc4 <- kmeans(vehicle_pca_data, centers = k, nstart = 25)

# plot the clusters accordingly
fviz_cluster(list(data = vehicle_pca_data, cluster = kc4$cluster), geom = "point", stand = FALSE, ellipse.type = "convex", ggtheme = theme_minimal())

kc4_centers <- kc4$centers
kc4_bss <- kc4$betweenss
kc4_tss <- kc4$tot.withinss
kc4_wss <- kc4$withinss

cat("K-means clustering with k=4:\n")
cat("Cluster centers:\n")
print(kc4_centers)
cat("Cluster membership:\n")
print(kc4$cluster)
cat("Between-cluster sum of squares (BSS): ", kc4_bss, "\n")
cat("Total sum of squares (TSS): ", kc4_tss, "\n")
cat("Ratio of BSS to TSS: ", kc4_bss/kc4_tss, "\n")
cat("Within-cluster sum of squares (WSS): ", kc4_wss, "\n\n")
table(good_vehicle_data$Class,kc4$cluster)#Confusion Matrix

# Compute silhouette width for k = 2
sil_width_2 <- silhouette(kc2$cluster, dist(vehicle_pca_data))
avg_sil_width_2 <- mean(sil_width_2[,3])
cat("Average Silhouette Width Score for k = 2 is", avg_sil_width_2, "\n")

# Compute silhouette width for k = 3
sil_width_3 <- silhouette(kc3$cluster, dist(vehicle_pca_data))
avg_sil_width_3 <- mean(sil_width_3[,3])
cat("Average Silhouette Width Score for k = 3 is", avg_sil_width_3, "\n")

# Compute silhouette width for k = 4
sil_width_4 <- silhouette(kc4$cluster, dist(vehicle_pca_data))
avg_sil_width_4 <- mean(sil_width_4[,3])
cat("Average Silhouette Width Score for k = 4 is", avg_sil_width_4, "\n")


# Get cluster membership for each observation
cluster_membership_pca <- kc3$cluster

# Load the clusterCrit package
library(clusterCrit)
library(cluster)
library(fpc)

# Convert PCA data to distance matrix
vehicle_dist <- dist(vehicle_pca_data)

# Calculate Calinski-Harabasz index
ch_index <- round(cluster.stats(vehicle_dist, cluster_membership_pca)$ch, 2)

# Print the index
cat("Calinski-Harabasz Index:", ch_index, "\n")