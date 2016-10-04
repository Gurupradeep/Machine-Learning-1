setwd("E:/Computer Engg/Machine learning/RScripts/CoverType");
train<-read.csv("train.csv");
test<-read.csv("test.csv");
library(rpart)
library(randomForest)
train$Highwater[train$Vertical_Distance_To_Hydrology < 0]<-0
train$Highwater[train$Vertical_Distance_To_Hydrology > 0]<-1
test$Highwater [test$Vertical_Distance_To_Hydrology < 0]<-0
test$Highwater [test$Vertical_Distance_To_Hydrology > 0]<-1
train$Aspect2[(train$Aspect)+180>360]<-train$Aspect-180
train$Aspect2[(train$Aspect)+180<360]<-train$Aspect+180
str(train)
predicted_hs<-rpart(Hillshade_3pm~Hillshade_Noon+Hillshade_9am,data=train[train$Hillshade_3pm!=0,],method="anova")
train$Hillshade_3pm[train$Hillshade_3pm==0]=predict(predicted_hs,train[train$Hillshade_3pm==0,])
write.csv(train$Hillshade_3pm,file="temp2.csv",row.names=FALSE)
forest<-randomForest(as.factor(Cover_Type)~.-Id,data=train,importance=TRUE,ntree=500)
plot(forest)
#library(RColorBrewer)
#library(rpart.plot)
#fancyRpartPlot(tree)
varImpPlot(forest)
test<-read.csv("test.csv")
my_prediction<-predict(forest,test,type="class")
answer<-data.frame(Id=test$Id,Cover_Type=my_prediction)
write.csv(answer,file="solution1.csv",row.names=FALSE)

