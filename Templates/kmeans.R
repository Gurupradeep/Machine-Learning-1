#Load the datasets library to use iris 
library(datasets)
head(iris)

#Use set.seed to ensure that same set of random k centroids are chosen everytime
#the code is run once the dataset is loaded into the global environment
set.seed(20)

#Use ggplot to plot the characteristics to roughly choose the ideal value of k and 
# nstart
library(ggplot2)
#plot 
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

#Run the kmeans algorithm
# Here, only Petal lengths and widths are chosen 
#nstart determines the number of initial configurations to be attempted; it chooses the best one
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)

#Creates a table with first argument as rows and second argument as columns
table(irisCluster$cluster, iris$Species)
irisCluster$cluster <- as.factor(irisCluster$cluster)
nrow(iris)

#Plot the results of the kmeans algorithm to see the clusters
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()
