# Hierarchical Clustering
#- There are two types of Hierarchical Clustering;
 
# 1. Agglomerative
# - It is the bottom up approach.
 
# 2. Divisive
 
# ** Steps for Agglomerative HC **
# Step 1 : Make each data point a single point cluster -> That forms N clusters.
# Step 2 : Take the two closest data points and make them one cluster -> That forms (N - 1) clusters.
# Step 3 : Take the two closest clusters and make them one cluster -> That forms (N - 2) clusters.
# Step 4 : Repeat Step 3 until there is only one cluster. Than FIN.

# - **Option to choose Distance between clusters**
# 1. Closest Point
# 2. Furthest Point
# 3. Average Distance
# 4. Distance between Centroids. 

# ----------------------------------------------------- Importing Data ------------------------------------------- #

dataset = read.csv('Mall_Customers.csv')

# Selecting particular columns
dataset = dataset[4:5]

#-----------------------------  Using the Dendogram to find the optimal number of clusters ----------------------- #

dendrogram = hclust(dist(dataset, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = "Dendrogram",
     xlab = "Customer",
     ylab = "Eculidean Distance")

# --------------------------------- Fitting Hierarchical Clustering to the Mall dataset -------------------------- #

hc = hclust(dist(dataset, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5)
y_hc

# -------------------------------------------- Visualising the Cluster ------------------------------------------- #

library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')
