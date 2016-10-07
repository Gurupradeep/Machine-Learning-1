#Install "kknn" package required for implementation of KNN
install.packages("kknn")
#Import the installed libraries
library(kknn)
#Import iris dataset
data(iris)
m <- nrow(iris)
#Select random indices from 1:m in 1/3rd of length of iris with equal probability
imp <- sample(1:m, m/3, prob = rep(1/m,m)) 
#Set up the training set
iris.train <- iris[-imp,]
#Make the remaining rows as testing dataset
iris.test <- iris[imp,]
#k is the no. of neighbours, distance mentions the kind of measure(distance=1 specifies Minskowski distance)
iris.knn <- kknn(formula = formula(Species~.), train = iris.train, test = iris.test, k = 7, distance = 1)
#fitted() extracts the classes of objects from the model
fit <- fitted(iris.knn)
table(iris.test$Species, fit)

