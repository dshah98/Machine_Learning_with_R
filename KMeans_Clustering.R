# K-mean takes out the complexity from the decision making processing which allows you to very easily identify those clusters are actually called cluster of data point.

# Steps to find K-Mean;
 # Step 1 : Chosse the number K of clusters.    
# Step 2 : Select at random K point, the centroid (not necessarily from your dataset)
# Step 3 : Assign each data point to the closest centroid -> That forms K clusters    
# Step 4 : Computer and place the new centroid of each cluster.
# Step 5 : Reaasign each data point to the new closest centroid. If any reassignment took place, go to Step 4, 
# otherwise go to FIN.

# ----------------------------------------------------- Importing Data ------------------------------------------- #

dataset = read.csv('Mall_Customers.csv')

# Selecting particular columns
dataset = dataset[4:5]

# ---------------------------- Using the elbow methos to find the optimal number of clusters --------------------- #

set.seed(6)

# WSCC - Within Cluster Sum of Square, 
# It is defined as the sum of the square distances between each observation pointof the cluster.
wcss <- vector()

for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)

plot(1:10, wcss, type = "b",
     main = paste("Cluster of Customers"),
     xlab = "Nummber of Clusters",
     ylab = "WCSS")

# The optimal number of clusters for our data is 5.

# -------------------------------------- Applying KMeans to the Mall dataset ------------------------------------- #

set.seed(29)
kmeans = kmeans(dataset, 5, iter.max = 300, nstart = 10)

# -------------------------------------------- Visualising the Cluster ------------------------------------------- #

library(cluster)
clusplot(dataset, 
         kmeans$cluster,
         lines = 0, shade = TRUE, color = TRUE, labels = 2, plotchar = FALSE, span = TRUE,
         main = paste("Cluster of Customers"),
         xlab = "Annual Income",
         ylab = "Spending Score")
