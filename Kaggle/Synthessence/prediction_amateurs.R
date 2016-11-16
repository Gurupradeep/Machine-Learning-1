library(caret)
library(randomForest)
#Set the current Working Directory
setwd("E:/Computer Engg/Machine learning/RScripts/submission");
#Load the training data
train<-read.csv("training.csv");
test<-read.csv("test.csv");
train
library(rpart)
train$age[train$age=='?']=mean(train$age,na.rm=TRUE)
test$age[test$age=='?']=mean(test$age,na.rm=TRUE)
#table(is.na(train$age))
nrow(train)
mean(train$age,na.rm=TRUE)
levels(train$workclass)
train$workclass[train$workclass=='?']<-' Private'
test$workclass[test$workclass=='?']<-' Private'
table(test$native.country)
train$occupation[train$occupation=='?']<-' Prof-specialty'
test$occupation[test$occupation=='?']<-' Prof-specialty'
train$native.country[train$native.country=='?']<-' United-States'
test$native.country[test$native.country=='?']<-' United-States'
train$profit<-NA
test$profit<-NA
train$profit=train$capital.gain-train$capital.loss
test$profit=test$capital.gain-test$capital.loss
train$capital.gain=NULL
train$capital.loss=NULL
test$capital.gain=NULL
test$capital.loss=NULL

'?' %in% train$education.num
'?' %in% test$final.weight
train$married <- as.integer(train$relationship%in%c(" Husband"," Wife"))
test$married <- as.integer(test$relationship%in%c(" Husband"," Wife"))
for (i in 1:15) {
  if(class(train[,i])=="factor") train[,i] <- as.numeric(train[,i])
  
}
for (i in 1:14) {
  if(class(test[,i])=="factor") test[,i] <- as.numeric(test[,i])
  
}


dmy=dummyVars("~.",data=train)
train.new=data.frame(predict(dmy,newdata=train))
head(train.new,1)
dmy2=dummyVars("~.",data=test)
test.new=data.frame(predict(dmy2,newdata=test))

forest<-randomForest(as.factor(greater)~.,data=train,importance=TRUE,ntree=500)
plot(forest)
library(RColorBrewer)
library(rpart.plot)
fancyRpartPlot(tree)
varImpPlot(forest)
colnames(test.new)
colnames(train.new)
#Use the model for predicting test data
my_prediction<-predict(forest,test,type="class")
answer<-data.frame(greater=my_prediction)
write.csv(answer,file="solution.csv",row.names=FALSE)
hist(train$age)
plot(train$age,train$greater)
