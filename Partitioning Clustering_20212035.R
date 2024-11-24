library(dplyr)
library(readxl)
library(factoextra)
library(NbClust)
library(caret)
library(e1071)


# Importing the Data set
library(readxl)
whitewine_data <- read_excel("C:/Users/krishan_s/Desktop/ML/Assessmet/Whitewine_v6.xlsx")

# number of data check
dim(whitewine_data)

# Checking for is there are any na values
sum(is.na(whitewine_data))

# a)

# using the box plot method to check outliers
boxplot(whitewine_data[,-12])

# first column outliers removal
IQR_FA <- IQR(whitewine_data$`fixed acidity`)
lower_FA = quantile(whitewine_data$`fixed acidity`, .25) - 1.5*IQR_FA
upper_FA = quantile(whitewine_data$`fixed acidity`, .75) + 1.5*IQR_FA

# second column outliers removal
IQR_VA <- IQR(whitewine_data$`volatile acidity`)
lower_VA = quantile(whitewine_data$`volatile acidity`, .25) - 1.5*IQR_VA
upper_VA = quantile(whitewine_data$`volatile acidity`, .75) + 1.5*IQR_VA

# 3 column outliers removal
IQR_CA <- IQR(whitewine_data$`citric acid`)
lower_CA = quantile(whitewine_data$`citric acid`, .25) - 1.5*IQR_CA
upper_CA = quantile(whitewine_data$`citric acid`, .75) + 1.5*IQR_CA

# 4 column outliers removal
IQR_RS <- IQR(whitewine_data$`residual sugar`)
lower_RS = quantile(whitewine_data$`residual sugar`, .25) - 1.5*IQR_RS
upper_RS = quantile(whitewine_data$`residual sugar`, .75) + 1.5*IQR_RS

# 5 column outliers removal
IQR_CL <- IQR(whitewine_data$chlorides)
lower_CL = quantile(whitewine_data$chlorides, .25) - 1.5*IQR_CL
upper_CL = quantile(whitewine_data$chlorides, .75) + 1.5*IQR_CL

# 6 column outliers removal
IQR_FSD <- IQR(whitewine_data$`free sulfur dioxide`)
lower_FSD = quantile(whitewine_data$`free sulfur dioxide`, .25) - 1.5*IQR_FSD
upper_FSD = quantile(whitewine_data$`free sulfur dioxide`, .75) + 1.5*IQR_FSD

# 7 column outliers removal
IQR_TSD <- IQR(whitewine_data$`total sulfur dioxide`)
lower_TSD = quantile(whitewine_data$`total sulfur dioxide`, .25) - 1.5*IQR_TSD
upper_TSD = quantile(whitewine_data$`total sulfur dioxide`, .75) + 1.5*IQR_TSD

# 8 column outliers removal
IQR_DE <- IQR(whitewine_data$density)
lower_DE = quantile(whitewine_data$density, .25) - 1.5*IQR_DE
upper_DE = quantile(whitewine_data$density, .75) + 1.5*IQR_DE

# 9 column outliers removal
IQR_PH <- IQR(whitewine_data$pH)
lower_PH = quantile(whitewine_data$pH, .25) - 1.5*IQR_PH
upper_PH = quantile(whitewine_data$pH, .75) + 1.5*IQR_PH

# 10 column outliers removal
IQR_SUL <- IQR(whitewine_data$sulphates)
lower_SUL = quantile(whitewine_data$sulphates, .25) - 1.5*IQR_SUL
upper_SUL = quantile(whitewine_data$sulphates, .75) + 1.5*IQR_SUL

# 11 column outliers removal
IQR_AL <- IQR(whitewine_data$alcohol)
lower_AL = quantile(whitewine_data$alcohol, .25) - 1.5*IQR_AL
upper_AL = quantile(whitewine_data$alcohol, .75) + 1.5*IQR_AL

updated_whitewine <- subset(whitewine_data, whitewine_data$`fixed acidity` >= lower_FA 
                            & whitewine_data$`fixed acidity` <= upper_FA 
                            & whitewine_data$`volatile acidity` >= lower_VA 
                            & whitewine_data$`volatile acidity` <= upper_VA 
                            & whitewine_data$`citric acid` >= lower_CA 
                            & whitewine_data$`citric acid` <= upper_CA
                            & whitewine_data$`residual sugar` >= lower_RS 
                            & whitewine_data$`residual sugar` <= upper_RS 
                            & whitewine_data$chlorides >= lower_CL 
                            & whitewine_data$chlorides <= upper_CL 
                            & whitewine_data$`free sulfur dioxide` >= lower_FSD 
                            & whitewine_data$`free sulfur dioxide` <= upper_FSD 
                            & whitewine_data$`total sulfur dioxide` >= lower_TSD 
                            & whitewine_data$`total sulfur dioxide` <= upper_TSD 
                            & whitewine_data$density >= lower_DE 
                            & whitewine_data$density <= upper_DE 
                            & whitewine_data$pH >= lower_PH 
                            & whitewine_data$pH <= upper_PH 
                            & whitewine_data$sulphates >= lower_SUL 
                            & whitewine_data$sulphates <= upper_SUL 
                            & whitewine_data$alcohol >= lower_AL 
                            & whitewine_data$alcohol <= upper_AL)

# Boxplot
boxplot(updated_whitewine)

# Plotting the box plot for before and after removing outliers
boxplot(whitewine_data[,1])
boxplot(updated_whitewine[,1])

boxplot(whitewine_data[,2])
boxplot(updated_whitewine[,2])

boxplot(whitewine_data[,3])
boxplot(updated_whitewine[,3])

boxplot(whitewine_data[,4])
boxplot(updated_whitewine[,4])

boxplot(whitewine_data[,5])
boxplot(updated_whitewine[,5])

boxplot(whitewine_data[,6])
boxplot(updated_whitewine[,6])

boxplot(whitewine_data[,7])
boxplot(updated_whitewine[,7])

boxplot(whitewine_data[,8])
boxplot(updated_whitewine[,8])

boxplot(whitewine_data[,9])
boxplot(updated_whitewine[,9])

boxplot(whitewine_data[,10])
boxplot(updated_whitewine[,10])

boxplot(whitewine_data[,11])
boxplot(updated_whitewine[,11])


# Normalizing the data ( Scaling )
# Creating a new function called normalize
normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}

WhitewineNormalize <- as.data.frame(lapply(updated_whitewine, normalize))
whiteNormUpdated <- WhitewineNormalize[,-12]
boxplot(whiteNormUpdated)

# b)

# Determine the optimal number of clusters using automated methods
# NBclust
library(NbClust)
nb_clusters <- NbClust(data = whiteNormUpdated, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

library(factoextra)

# Elbow method to determine the number of clusters
elbow_plot <- fviz_nbclust(whiteNormUpdated, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +  # Add a vertical line at k = 4
  labs(subtitle = "Elbow method")

print(elbow_plot)

# Gap Statistics
library(cluster)
gap_stat <- clusGap(whiteNormUpdated, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

fviz_gap_stat(gap_stat)

# Silhouette Method
fviz_nbclust(whiteNormUpdated, kmeans,method = "silhouette") + labs(subtitle = "Silhouette method")

# c)

# Find the most favored k from the automated methods
fav_k <- list(NBclust = nb_clusters$Best.nc[1],
              Elbow = which.min(wcss),
              Gap_Statistics = which.max(gap_stat$Tab[, "gap"]),
              Silhouette = which.max(silhouette[2:10]))

print(fav_k)

# Select the most common k among the favored k values
most_common_k <- names(sort(table(unlist(fav_k)), decreasing = TRUE))[1]

print(most_common_k)

# Perform K-means clustering with the most favored k
kmeans_model <- kmeans(whiteNormUpdated, centers = as.numeric(most_common_k))

# Print K-means model summary
print(kmeans_model)

# Extract cluster centers
cluster_centers <- kmeans_model$centers
print(cluster_centers)

# Assign clusters to each data point
cluster_assignments <- kmeans_model$cluster
print(cluster_assignments)

# Compute Within-Cluster Sum of Squares (WSS)
wss <- kmeans_model$tot.withinss
print(paste("Within-Cluster Sum of Squares (WSS):", wss))

# Compute Between-Cluster Sum of Squares (BSS)
tss <- sum(kmeans_model$tot.withinss) + 
  sum((apply(whiteNormUpdated, 2, mean) - cluster_centers)^2) * nrow(whiteNormUpdated)

bss <- tss - wss
print(paste("Between-Cluster Sum of Squares (BSS):", bss))

# Compute Ratio of BSS over TSS
bss_tss_ratio <- bss / tss
print(paste("Ratio of BSS over TSS:", bss_tss_ratio))

########################################################################################################

# e)

# Perform PCA
pca_result <- prcomp(whiteNormUpdated, scale = TRUE)

# Extract eigenvalues and eigenvectors
eigenvalues <- pca_result$sdev^2
eigenvectors <- pca_result$rotation

# Print eigenvalues and eigenvectors
print("Eigenvalues:")
print(eigenvalues)
print("Eigenvectors:")
print(eigenvectors)

# Compute cumulative score per principal component
cumulative_score <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2) * 100)

# Print cumulative score
print("Cumulative Score per Principal Component:")
print(cumulative_score)

# Find the number of principal components with cumulative score > 85%
num_components <- which(cumulative_score > 85)[1]

# Extract the required principal components
transformed_data <- as.data.frame(pca_result$x[, 1:num_components])

# Print the transformed dataset
print("Transformed Dataset with Principal Components:")
print(transformed_data)

# f)

# Perform k-means clustering on PCA-transformed dataset
kmeans_model <- kmeans(transformed_data, centers = 2, nstart = 25)

# Apply NBclust method
nbclust_out <- NbClust(transformed_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
print(nbclust_out)

# Visualize NBclust results
fviz_nbclust(nbclust_out)

# Elbow Method
fviz_nbclust(transformed_data, kmeans, method = "wss")

# Gap Statistics
fviz_nbclust(transformed_data, kmeans, method = "gap_stat")

# Silhouette Method
fviz_nbclust(transformed_data, kmeans, method = "silhouette")

# Visualize clustering results
fviz_cluster(kmeans_model, data = transformed_data)

# g)

# Perform k-means clustering with the most favored k value
kmeans_result <- kmeans(transformed_data, centers = 2)  # Assuming 2 is the most favored k value

# Display k-means output
print("K-Means Output:")
print(kmeans_result)

# Display cluster centers
print("Cluster Centers:")
print(kmeans_result$centers)

# Display clustered results
print("Clustered Results:")
print(kmeans_result$cluster)

# Calculate between_cluster_sums_of_squares (BSS)
BSS <- sum((kmeans_result$centers - colMeans(transformed_data))^2)

# Calculate within_cluster_sums_of_squares (WSS)
WSS <- sum(kmeans_result$withinss)

# Calculate total_sum_of_Squares (TSS)
TSS <- BSS + WSS

# Calculate BSS/TSS ratio
BSS_TSS_ratio <- BSS / TSS

# Display BSS, WSS, and BSS/TSS ratio
print("Between Cluster Sum of Squares (BSS):")
print(BSS)
print("Within Cluster Sum of Squares (WSS):")
print(WSS)
print("Ratio of BSS over TSS:")
print(BSS_TSS_ratio)

# h)

# Generate silhouette plot
library(cluster)
silhouette_plot <- silhouette(kmeans_result$cluster, dist(transformed_data))

# Plot silhouette plot
plot(silhouette_plot, main = "Silhouette Plot for K-Means Clustering")

# Calculate silhouette scores
sil <- silhouette(kmeans_result$cluster, silhouette_plot)

# Average silhouette width score
avg_silhouette_width <- mean(sil$width)

# Print average silhouette width score
cat("Average Silhouette Width Score:", avg_silhouette_width, "\n")