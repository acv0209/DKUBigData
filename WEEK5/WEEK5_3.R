setwd("c:/MyR")

###################################################################################
# Example-1: Hierarchical Clustering
###################################################################################
# Load the data 
data("USArrests")
head(USArrests)

# Standardize the data 
df <- scale(USArrests)

# Show the first 6 rows 
head(df, nrow = 6)

# Compute the dissimilarity matrix 
# df = the standardized data 
res.dist <- dist(df, method = "euclidean")
res.dist
 
# The R code below displays the first 6 rows and columns of the distance matrix: 
as.matrix(res.dist)[1:6, 1:6]

# hclust() can be used as follow:
# d: a dissimilarity structure as produced by the dist() function. 
# method: Theagglomeration(linkage)methodtobeusedforcomputingdistance between clusters. 
# Allowed values is one of "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".  
res.hc <- hclust(d=res.dist, method = "ward.D2")

# Dendrogram
install.packages("factoextra") 

# cex: label size 
library("factoextra") 
fviz_dend(res.hc, cex = 0.5)

# Verify the cluster tree
# Compute cophentic distance 
res.coph <- cophenetic(res.hc)

# Correlation between cophenetic distance and 
# the original distance 
cor(res.dist, res.coph)

res.hc2 <- hclust(res.dist, method = "average")
cor(res.dist, cophenetic(res.hc2))

res.hc3 <- hclust(res.dist, method = "mcquitty")
cor(res.dist, cophenetic(res.hc3))

res.hc4 <- hclust(res.dist, method = "median")
cor(res.dist, cophenetic(res.hc4))

res.hc5 <- hclust(res.dist, method = "centroid")
cor(res.dist, cophenetic(res.hc5))

res.hc6 <- hclust(res.dist, method = "complete")
cor(res.dist, cophenetic(res.hc6))

# Cut tree into 4 groups 
grp <- cutree(res.hc, k=4) 
head(grp, n=4)

# Number of members in each cluster 
table(grp)

# Get the names for the members of cluster 1 
rownames(df)[grp == 1]

# The result of the cuts can be visualized easily using the function fviz_dend() [in factoextra]
# Cut in 4 groups and color by groups 
fviz_dend(res.hc, k=4, # Cut in four groups 
          cex = 0.5, # label size 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
          color_labels_by_k = TRUE, # color labels by groups 
          rect = TRUE # Add rectangle around groups
)

# Using the function fviz_cluster() [in factoextra], 
# we can also visualize the result in a scatter plot. 
# Observations are represented by points in the plot, using principal components. 
# A frame is drawn around each cluster.          
fviz_cluster(list(data = df, cluster = grp), 
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "convex", # Concentration ellipse 
             repel = TRUE, # Avoid label overplotting (slow) 
             show.clust.cent = FALSE, ggtheme = theme_minimal())
             
###################################################################################
# Example-2: Cluster Analysis
###################################################################################
x=matrix(c(88, 72, 94, 68, 92, 85, 77, 88, 71, 85, 71, 83, 
           73, 63, 79, 90, 80, 70, 87, 74, 74, 88, 65, 77, 
           68, 63, 73, 80, 63, 62, 60, 62, 74, 67, 74), 7, 5, byrow=TRUE) 
colnames(x)=c("안정성", "보건성", "편리성", "쾌적성", "효율성")
rownames(x)=c("서울","부산","대구","대전","광주","인천","울산")
x
(dd=round(dist(x),1))

par(mfrow=c(2,3))
c1=hclust(dd)
plot(c1,hang=1, main="Complete: 최장거리법")
c2=hclust(dd, method="single")
plot(c2,hang=1, main="Single: 최단거리법")
c3=hclust(dd, method="centroid")
plot(c3,hang=1, main="Centroid: 중심법")
c4=hclust(dd, method="ward.D")
plot(c4,hang=1, main="Ward.D: 워드법")
c5=hclust(dd, method="average")
plot(c5,hang=1, main="Average: 평균법")
c6=hclust(dd, method="median")
plot(c6,hang=1, main="Median: 중앙값법")

c1=hclust(dd)
c1$merge
c1$height
c1$order
c1$labels
c1$method
c1$dist.method

c1=hclust(dd)
cutree (c1, k=2) # Cluster Num=2
cutree (c1, k=3) # Cluster Num=3
cophenetic(c1) 
cor (dd, cophenetic (hclust(dd, method="complete")))
cor (dd, cophenetic (hclust(dd, method="single")))
cor (dd, cophenetic (hclust(dd, method="centroid")))
cor (dd, cophenetic (hclust(dd, method="ward.D2")))
cor (dd, cophenetic (hclust(dd, method="average")))
cor (dd, cophenetic (hclust(dd, method="median")))

c1=hclust(dd)
den1=as.dendrogram(c1) 
str(den1, max=2)
par (mfrow=c(2,3))
plot(den1) #normal, triangle
plot(den1, nodePar=list(pch=c(1,NA), cex=0.8, labcex=0.8), type="t", center=T)
plot(den1, nodePar=list(pch=2:1, cex=0.4*2, col=2:3), horiz=T) #horiz
plot(cut (den1, h=35)$upper)
plot(cut (den1, h=35)$lower[[3]], nodePar=list(pch=c(1,7), horiz=T, type="tr"))
plot(den1) 
rect.hclust (c1, k=3, border="red")

###################################################################################
# Example-3: K-Means cluster Analysis 
###################################################################################
# We will use the demo data sets "USArrests".

data("USArrests")      # Loading the data set
df <- scale(USArrests) # Scaling the data
# View the firt 3 rows of the data
head(df, n = 3)

# Required R packages and functions
install.packages("factoextra")
library(factoextra)

# One fundamental question is: How to choose the right number of expected clusters (k)?
library(factoextra)
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# The R code below performs k-means clustering with k = 4:
# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)

# Print the results
print(km.res)

# It is possible to compute the mean of each variables by clusters using the original data:
aggregate(USArrests, by=list(cluster=km.res$cluster), mean)

# If you want to add the point classifications to the original data, use this:
dd <- cbind(USArrests, cluster = km.res$cluster)
head(dd)  

# kmeans() function returns a list of components, including: 
# cluster, centers, totss, withinss, tot.withinss, betweenss, size.

# Cluster number for each of the observations
km.res$cluster
head(km.res$cluster, 4)

# Cluster size
km.res$size

# Cluster means
km.res$centers

# Visualizing k-means clusters
# If we have a multi-dimensional data set, a solution is to perform Principal Component Analysis (PCA) 
# and to plot data points according to the first two principal components coordinates. 
# The function fviz_cluster() [factoextra package] can be used to easily visualize k-means clusters.

fviz_cluster(km.res, data = df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

###################################################################################
# Example-4: K-Means cluster Analysis 
###################################################################################
xx=matrix(c(88, 72, 94, 68, 92, 85, 77, 88, 71, 85, 71, 83, 
            73, 63, 79, 90, 80, 70, 87, 74, 74, 88, 65, 77, 
            68, 63, 73, 80, 63, 62, 60, 62, 74, 67, 74), 7, 5, byrow=TRUE) 
colnames(xx)=c("안정성", "보건성", "편리성", "쾌적성", "효율성")
rownames(xx)=c("서울","부산","대구","대전","광주","인천","울산")

k1=kmeans(xx, center=2, algorithm="Hartigan-Wong")
k2=kmeans(xx, center=3, algorithm="Hartigan-Wong")
k3=kmeans(xx, center=4, algorithm="Hartigan-Wong")
k1$betweenss / k1$totss	 
k2$betweenss / k2$totss	 
k3$betweenss / k3$totss

kk1=kmeans(xx, center=3, algorithm="Hartigan-Wong")
kk2=kmeans(xx, center=3, algorithm="Lloyd")
kk3=kmeans(xx, center=3, algorithm="Forgy")
kk4=kmeans(xx, center=3, algorithm="MacQueen")
kk1$betweenss / kk1$totss	 
kk2$betweenss / kk2$totss	 
kk3$betweenss / kk3$totss
kk4$betweenss / kk4$totss

## Among (kk1, kk2, kk3, kk4), kk1 is the best kk1
## We use Hartigan-Wong algorithm.

kk1

###################################################################################
# Example-5: hclust: binary data 
# 정성적 자료에 의한 계층적 군집분석
# 여가시간의 활용의사를 조사하여 다음과 같은 결과가 나왔다.
###################################################################################
x1=c(1,1,0,0,0,0,0,0,1,0)
x2=c(0,0,1,1,1,0,0,0,0,1)
x3=c(0,0,1,0,1,0,1,1,0,0)
x4=c(0,0,0,1,0,0,0,0,1,0)
x5=c(0,1,1,1,0,0,1,0,0,0)
x6=c(1,1,0,0,0,1,0,1,0,1)
x7=c(1,0,0,0,0,1,0,0,1,1)
x8=c(0,0,0,0,1,1,1,1,0,0)
y=rbind(x1,x2,x3,x4,x5,x6,x7,x8)
rownames(y)=c("운동","TV","인터넷","독서","게임","쇼핑","예술","영화")
(dd=round(dist(y,method="binary")*10,2))

par(mfrow=c(2,3))
c1=hclust(dd)
plot(c1,hang=1, main="Complete")
c2=hclust(dd, method="single")
plot(c2,hang=1, main="Single")
c3=hclust(dd, method="centroid")
plot(c3,hang=1, main="Centroid")
c4=hclust(dd, method="ward.D")
plot(c4,hang=1, main="Ward.D")
c5=hclust(dd, method="average")
plot(c5,hang=1, main="Average")
c6=hclust(dd, method="median")
plot(c6,hang=1, main="Median")

yy=cbind(x1,x2,x3,x4,x5,x6,x7,x8)
ddd=round(dist(yy,method="binary")*10,2)
par(mfrow=c(2,3))
cc1=hclust(dd)
plot(cc1,hang=1, main="Complete: 최장거리법")
cc2=hclust(ddd, method="single")
plot(cc2,hang=1, main="Single: 최단거리법")
cc3=hclust(ddd, method="centroid")
plot(cc3,hang=1, main="Centroid: 중심법")
cc4=hclust(ddd, method="ward.D")
plot(cc4,hang=1, main="Ward.D: 워드법")
cc5=hclust(ddd, method="average")
plot(cc5,hang=1, main="Average: 평균법")
cc6=hclust(ddd, method="median")
plot(cc6,hang=1, main="Median: 중앙값법")


###################################################################################
# Example-6: K-Means 
###################################################################################

x <- rbind(matrix(rnorm(100,sd=0.3), ncol=2), 
           matrix(rnorm(100,mean=1, sd=0.3), ncol=2)) 
colnames(x) <- c("Weight", "Height") 
head(x)
(cl <- kmeans(x,5,nstart=25))
cl$totss
cl$tot.withinss
cl$betweenss
cl$size
cl$totss
cl$tot.withinss
cl$betweenss
cl$size
plot(x, col=cl$cluster, pch=cl$cluster)
points(cl$center, col=1:5, pch=16)
title("K-Means Plot \n Weight - Height : 5 Groups")
legend("bottomright", c("1 Group", "2 Group", "3 Group", 
                        "4 Group", "5 Group"), col=1:5, pch=1:5)

###################################################################################
# Example-7: The Chatterjee - Price Attitude Data
# A data frame with 30 observations on 7 variables. 
# The first column are the short names from the reference, 
# the second one the variable names in the data frame:

#   Y	rating	numeric	Overall rating
# X[1]	complaints	numeric	Handling of employee complaints
# X[2]	privileges	numeric	Does not allow special privileges
# X[3]	learning	numeric	Opportunity to learn
# X[4]	raises	numeric	Raises based on performance
# X[5]	critical	numeric	Too critical
# X[6]	advancel	numeric	Advancement
###################################################################################

library(datasets)
str(attitude)
summary(attitude)

# Subset the attitude data
dat = attitude[,c(3,4)]

# Plot subset data
plot(dat, main = "% of favourable responses to
     Learning and Privilege", pch =20, cex =2)

# Perform K-Means with 2 clusters
set.seed(7)
km1 = kmeans(dat, 2, nstart=100)

# Plot results
plot(dat, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)

# Check for the optimal number of clusters given the data

mydata <- dat
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

# Perform K-Means with the optimal number of clusters identified from the Elbow method
set.seed(7)
km2 = kmeans(dat, 6, nstart=100)

# Examine the result of the clustering algorithm
km2

# Plot results
plot(dat, col =(km2$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)

# Estimating the optimal number of clusters
# One fundamental question is: How to choose the right number of expected clusters (k)?
# Wwe provide a simple solution. 
# The idea is to compute k-means clustering using different values of clusters k. 
# Next, the wss (within sum of square) is drawn according to the number of clusters. 
# The location of a bend (knee) in the plot is generally considered as an indicator 
# of the appropriate number of clusters.  

data("USArrests")      # Loading the data set
df <- scale(USArrests) # Scaling the data
# View the firt 3 rows of the data
head(df, n = 3)

library(factoextra)

# The plot above represents the variance within the clusters. 
# It decreases as k increases, but it can be seen a bend (or ??elbow??) at k = 4. 
# This bend indicates that additional clusters beyond the fourth have little value.
# In the next section, we??ll classify the observations into 4 clusters.

fviz_nbclust(df, kmeans, method = "wss") +  geom_vline(xintercept = 4, linetype = 2)

# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)

print(km.res)

aggregate(USArrests, by=list(cluster=km.res$cluster), mean)

dd <- cbind(USArrests, cluster = km.res$cluster)
head(dd)

# Cluster number for each of the observations
km.res$cluster

head(km.res$cluster, 4)

# Cluster size
km.res$size

# Cluster means
km.res$centers

fviz_cluster(km.res, data = df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse 
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


