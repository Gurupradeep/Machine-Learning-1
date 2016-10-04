#Set the current Working Directory
setwd("E:/Computer Engg/Machine learning/RScripts");
#Load the training data
train<-read.csv("train.csv");
library(rpart)
#Build the model
tree<-rpart(as.factor(Output)~.,data=train,method="class",control=rpart.control(minsplit=,cp=))
pfit<- prune(tree, cp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
plot(tree)
text(tree)
plot(pfit)
text(pfit)
library(RColorBrewer)
library(rpart.plot)
fancyRpartPlot(tree)
#Load the test data
test<-read.csv("test.csv")
#Use the model for predicting test data
my_prediction<-predict(pfit,test,type="class")
answer<-data.frame(Id=test$Id,Output=my_prediction)
write.csv(answer,file="solution.csv",row.names=FALSE)

