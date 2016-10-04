#Import iris
library(datasets)
head(iris)
#Use ggplot to plot the characteristics
library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

#hclust function requires the input to be provided as a distance matrix- ith row jth column is distance between ith and jth rows in dataset
#it is computed by using the specified distance measure to compute the distances between the rows of a data matrix
#"euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski" are the possible measures;"euclidean" used by default
head(dist(iris[,3:4]))

#Use hierarchical clustering; Complete Linkage clustering is used by default if method is unspecified
clusters <- hclust(dist(iris[, 3:4]),method='average')

#Plots the cluster dendrogram (tree used for hierarchical clustering)
plot(clusters)

#puts rectangles around clusters at height h
rect.hclust(hclust(dist(iris[, 3:4]),method='average'),h=3)

#cut the tree at level passed as second argument
clusterCut <- cutree(clusters, 3)

#Check the accuracy of results
table(clusterCut, iris$Species)

