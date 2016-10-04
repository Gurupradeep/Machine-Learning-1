setwd("E:/Computer Engg/Machine learning/RScripts");
train<-read.csv("train.csv");
library(rpart)
tree<-rpart(Cover_Type~.,data=train,method="class")
plot(tree)
text(tree)

my_prediction<-predict(tree,test,type="class")
answer<-data.frame(Id=test$Id,Cover_Type=my_prediction)
write.csv(answer,file="solution.csv",row.names=false)

