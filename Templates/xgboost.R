#Loading libraries
library(xgboost)
library(caret)
library(Metrics)
library(RCurl)

# Setting up the Working directory 
setwd("E:/Computer Engg/Machine learning/RScripts/")

#Loading the test and the train datasets
train = read.csv("train.csv")
test = read.csv("test.csv")

#Extract the target column
outcome = train[, "target"]
outcome<-as.factor(outcome)
levels(outcome)

#Convert the character levels to numeric
num.class = length(levels(outcome))
levels(outcome) = 1:num.class
head(outcome)

#Remove the target column from training set
train$target<-NULL

#If Columns with NA have to be removed
# remove columns with NA, use test data as referal for NA
cols.without.na = colSums(is.na(test)) == 0
train = train[, cols.without.na]
test = test[, cols.without.na]

#Convert data into matrix format compatible for xgboost
train.matrix = as.matrix(train)
test.matrix = as.matrix(test)
# convert outcome from factor to numeric matrix 
# xgboost takes multi-labels in [0, numOfClass)
y = as.matrix(as.integer(outcome)-1)

#Set xgboost parameters
param <- list("objective" = "multi:softprob",    # multiclass classification 
              "num_class" = num.class,    # number of classes 
              "eval_metric" = "merror",    # evaluation metric 
              "nthread" = 8,   # number of threads to be used 
              "max_depth" = 16,    # maximum depth of tree 
              "eta" = 0.3,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 1,    # part of data instances to grow tree 
              "colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12  # minimum sum of instance weight needed in a child 
)
#Perform n-fold Cross Validation while training 
nround.cv = 200
bst.cv <- xgb.cv(param=param, data=train.matrix, label=y, 
                              nfold=4, nrounds=nround.cv, prediction=TRUE, verbose=FALSE) 
tail(bst.cv$dt) 
#Index of minimum merror
min.merror.idx = which.min(bst.cv$dt[, test.merror.mean]) 

#print it
min.merror.idx

#Get CV's prediction decoding
pred.cv = matrix(bst.cv$pred, nrow=length(bst.cv$pred)/num.class, ncol=num.class)
pred.cv = max.col(pred.cv, "last")

#Confusion matrix
confusionMatrix(factor(y+1), factor(pred.cv))

#Real model fit training with full data
bst <- xgboost(param=param, data=train.matrix, label=y, 
                            nrounds=min.merror.idx, verbose=0)

#Xgboost predict test data using the trained model
pred <- predict(bst, test.matrix)

#Decode the prediction
pred = matrix(pred, nrow=num.class, ncol=length(pred)/num.class)
pred = t(pred)
pred = max.col(pred, "last")
print(pred)

#Write it to a .csv file
solution =data.frame(Id = ,Prediction = pred)
write.csv(solution,file = "submission.csv",row.names = FALSE)