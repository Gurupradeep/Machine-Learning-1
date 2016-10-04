#Loading libraries
library(xgboost)
library(caret)
library(Metrics)
library(RCurl)

# Setting up the Working directory 
setwd("E:/Computer Engg/Machine learning/RScripts/CoverType")

#Loading the test and the train datasets
train = read.csv("train.csv")
test = read.csv("test.csv")

train$Highwater[train$Vertical_Distance_To_Hydrology <= 0]<-0
train$Highwater[train$Vertical_Distance_To_Hydrology > 0]<-1
test$Highwater [test$Vertical_Distance_To_Hydrology <= 0]<-0
test$Highwater [test$Vertical_Distance_To_Hydrology > 0]<-1
train$Aspect2[(train$Aspect)+180>=360]<-train$Aspect-180
train$Aspect2[(train$Aspect)+180<360]<-train$Aspect+180

#Extract the target column
outcome = train[, "Cover_Type"]
outcome<-as.factor(outcome)
levels(outcome)

#Convert the character levels to numeric
num.class = length(levels(outcome))
levels(outcome) = 1:num.class
print(levels(outcome))
head(outcome)

#Remove the target column from training set
train$Cover_Type<-NULL
train$Id<-NULL

#If Columns with NA have to be removed
# remove columns with NA, use test data as referal for NA
cols.without.na = colSums(is.na(test)) == 0
train = train[, cols.without.na]
test = test[, cols.without.na]

#Convert REAL to NUMERIC
train[] <- lapply(train, as.numeric)
test[]<-lapply(test, as.numeric)

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
              "max_depth" = 10,    # maximum depth of tree 
              "eta" = 0.03 ,   # step size shrinkage,
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 1,    # part of data instances to grow tree 
              "colsample_bytree" = 0.35,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12 
)
#Perform n-fold Cross Validation while training 
set.seed(1234)
nround.cv = 700
bst.cv <- xgb.cv(param=param, data=train.matrix, label=y, 
                 nfold=4, nrounds=nround.cv, prediction=TRUE, verbose=FALSE) 
tail(bst.cv$dt) 
#Index of minimum merror
min.merror.idx = which.min(bst.cv$dt[, test.merror.mean]) 

#print it
bst.cv$dt[min.merror.idx]

#Get CV's prediction decoding
pred.cv = matrix(bst.cv$pred, nrow=length(bst.cv$pred)/num.class, ncol=num.class)
pred.cv = max.col(pred.cv, "last")

#Confusion matrix
confusionMatrix(factor(y+1), factor(pred.cv))

#Real model fit training with full data
#bst <- xgboost(param=param, data=train.matrix, label=y, 
 #              nrounds=min.merror.idx, verbose=0)
#train<-read.csv("train.csv")
trainM<-data.matrix(train, rownames.force = NA);
#cat("Creating DMarix for xgboost...\n");
dtrain <- xgb.DMatrix(data=trainM, label=y, missing = NaN);

watchlist <- list(trainM=dtrain);
clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = min.merror.idx, 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)
#Xgboost predict test data using the trained model
pred <- predict(clf, test.matrix)

#Decode the prediction
pred = matrix(pred, nrow=num.class, ncol=length(pred)/num.class)
pred = t(pred)
pred = max.col(pred, "last")
print(pred)
unique(pred)

#Plotting importance
# get the trained model
model = xgb.dump(bst, with.stats=TRUE)
# get the feature real names
names = dimnames(train.matrix)[[2]]
# compute feature importance matrix
importance_matrix = xgb.importance(names, model=bst)

# plot
gp = xgb.plot.importance(importance_matrix)
print(gp) 
#Write it to a .csv file
test<-read.csv("test.csv")
typeof(test$Id)
solution =data.frame(Id = test$Id,Cover_Type = pred)
write.csv(solution,file = "submission.csv",row.names = FALSE)
