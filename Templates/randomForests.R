#Set the current Working Directory
setwd("E:/Computer Engg/Machine learning/RScripts/");
#Load the training data
train<-read.csv("train.csv");
library(randomForest)
#Build the model
forest<-randomForest(as.factor(Output)~.-Id,data=train,importance=TRUE,ntree=500)
plot(forest)
#Load the test data
test<-read.csv("test.csv")
library(RColorBrewer)
library(rpart.plot)
fancyRpartPlot(tree)
varImpPlot(forest)
#Use the model for predicting test data
my_prediction<-predict(forest,test,type="class")
answer<-data.frame(Id=test$Id,Output=my_prediction)
write.csv(answer,file="solution.csv",row.names=FALSE)

#Use rfcv() to obtain cross validation prediction performance with iris example 
#See link: https://www.rdocumentation.org/packages/randomForest/versions/4.6-12/topics/rfcv
set.seed(647)
myiris <- cbind(iris[1:4], matrix(runif(96 * nrow(iris)), nrow(iris), 96))
result <- rfcv(myiris, iris$Species, cv.fold=3)
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))