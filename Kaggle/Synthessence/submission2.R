library(caret)
library(randomForest)
set.seed(100)
#Set the current Working Directory
setwd("E:/Computer Engg/Machine learning/RScripts/submission");
#Load the training data
train<-read.csv("training.csv");
test<-read.csv("test.csv");
library(rpart)
library(randomForest)
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


y=NULL
y = train$greater
train$greater = NULL
param0 <- list(
  "objective"  = "binary:logistic",
  "eval_metric" = "auc",
  "eta" = 0.01 
  ,"subsample" = 1
  , "colsample_bytree" = 1
  , "min_child_weight" = 1
  , "max_depth" = 9
) 
library(xgboost)
xgtrain <- xgb.DMatrix(as.matrix(train), label = y)
watchlist <- list('train' = xgtrain)
cv <- xgb.cv(params = param0,data = xgtrain,nrounds = 101,nfold = 3,print.every.n = 20)
xgmod = xgb.train(
  nrounds = 150
  , params = param0
  , data = xgtrain
  , watchlist = watchlist
  , nthread = 8
  ,print.every.n = 20
)
data2 = as.matrix(test)
prediction = predict(xgmod,xgb.DMatrix(data2))
for(i in 1:length(prediction))
{
  if(!is.na(prediction[i]))
  {
    if(prediction[i]>.5 ){
      prediction[i] = 1
    }
    else 
    {
      prediction[i] = 0
    }
  }
  else
  {
    prediction[i] = 0
  }
}
answer<-data.frame(greater=prediction)
answer
write.csv(answer,file="prediction_amateurs.csv",row.names=FALSE)
