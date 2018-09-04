# Final Exam Part 2
# Households

# Before start, set working directory.

# 1 -2 
# Load the dataset.
Households <- read.csv("Households.csv")
str(Households)

table(Households$MorningPct, )

# Of the households that spend more than $150 per transaction on average, 
# what is the minimum average discount per transaction?
min(Households$AvgDiscount[Households$AvgSalesValue >= 150])

# Of the households who have an average discount per transaction greater than 25%, 
# what is the minimum average sales value per transaction?
min(Households$AvgSalesValue[Households$AvgDiscount >= 25])

# In the dataset, what proportion of households visited the retailer 
# at least 300 times?
sum(Households$NumVisits >= 300) / nrow(Households)

# 3
# IMPORTANCE OF NORMALIZING
summary(Households)
# We would expect NumVisits to dominate, because it is on the largest scale

# 4
# Normalizing
library(caret)

preproc = preProcess(Households)
HouseholdsNorm = predict(preproc, Households)
# What is the maximum value of NumVisits in the normalized dataset?
# What is the minimum value of AfternoonPct in the normalized dataset?
summary(HouseholdsNorm)

# 5
# Run the following code to create a dendrogram of your data:
set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
# how many clusters appropriate?
plot(ClusterShoppers, labels = FALSE)
# Four clusters and six clusters have very little "wiggle room", 
# which means that the additional clusters are not very distinct 
# from existing clusters. That is, when moving from 3 clusters 
# to 4 clusters, the additional cluster is very similar to an 
# existing one (as well as when moving from 5 clusters to 6 clusters).

# 6
# Run the k-means clustering algorithm
set.seed(200)
km <- kmeans(HouseholdsNorm, 10)
# How many observations are in the smallest $ largest cluster?
table(km$cluster)

# 7
# Now, use the cluster assignments from k-means clustering together 
# with the cluster centroids to answer the next few questions.

# Extract clusters
HouseholdsClusters = km$cluster
 
kclust1 <- subset(Households, HouseholdsClusters == 1)
kclust2 <- subset(Households, HouseholdsClusters == 2)
kclust3 <- subset(Households, HouseholdsClusters == 3)
kclust4 <- subset(Households, HouseholdsClusters == 4)
kclust5 <- subset(Households, HouseholdsClusters == 5)
kclust6 <- subset(Households, HouseholdsClusters == 6)
kclust7 <- subset(Households, HouseholdsClusters == 7)
kclust8 <- subset(Households, HouseholdsClusters == 8)
kclust9 <- subset(Households, HouseholdsClusters == 9)
kclust10 <- subset(Households, HouseholdsClusters == 10)

sapply(split(Households, HouseholdsClusters), colMeans)

# un the k-means clustering algorithm again, this time selecting 5 clusters. 
# Right before the "kmeans" function, set the random seed to 5000.
set.seed(5000)
km2 <- kmeans(HouseholdsNorm, 5)
table(km2$cluster)
HouseholdsClusters = km2$cluster

k2clust1 <- subset(Households, HouseholdsClusters == 1)
k2clust2 <- subset(Households, HouseholdsClusters == 2)
k2clust3 <- subset(Households, HouseholdsClusters == 3)
k2clust4 <- subset(Households, HouseholdsClusters == 4)
k2clust5 <- subset(Households, HouseholdsClusters == 5)
sapply(split(Households, HouseholdsClusters), colMeans)

# The cluster centroid shows average behavior in a single cluster - 
# it does not describe every single observation in that cluster or 
# tell us how the cluster compares to other clusters.

# A box plot of NumVisits shows the distribution of the number of 
# visits of the households, and we want to subdivide by cluster. 
# Alternatively, ggplot with x as the cluster and y as the number 
# of visits plots the data, but only geom_point is appropriate to 
# show the distribution of the data.
