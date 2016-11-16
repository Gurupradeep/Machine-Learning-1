This is a readme file explaining the method used for prediction of income for the given testing data.


# Data Cleaning

The features were analyzed and the missing values were replaced accordingly. 
For "age", there were no missing values. Nonetheless, it was replaced with the mean of the age values in both the training and testing datasets.
Categorical variables were replaced by most frequently occuring factor levels.
All categorical variables were factored. 



#Feature engineering

Two features - "capital-gain", "capital-loss" were removed and "profit" ,"married" were added. 

1. train$profit=train$capital.gain-train$capital.loss
test$profit=test$capital.gain-test$capital.loss 

2. train$married <- as.integer(train$relationship%in%c(" Husband"," Wife"))
test$married <- as.integer(test$relationship%in%c(" Husband"," Wife"))

#Building the model

Logistic regression was tried as a naive approach and the desired accuracy was not obtained. The training error was high when cross validation was performed. So, randomforest approach was later used. 
XGBoost algorithm was also run. The errors obtained was slightly lesser than that obtained using randomforests. 

Below are the errors obtained as a result of cross validation.
> cv <- xgb.cv(params = param0,data = xgtrain,nrounds = 101,nfold = 3,print.every.n = 20)
[0]	train-auc:0.918311+0.002997	test-auc:0.906279+0.002751
[20]	train-auc:0.922430+0.003381	test-auc:0.909358+0.002642
[40]	train-auc:0.925605+0.002658	test-auc:0.910813+0.003386
[60]	train-auc:0.927785+0.002537	test-auc:0.912024+0.003191
[80]	train-auc:0.929725+0.002504	test-auc:0.912973+0.003508
[100]	train-auc:0.931428+0.002389	test-auc:0.913358+0.003799

> xgmod = xgb.train(
  nrounds = 150
  , params = param0
  , data = xgtrain
  , watchlist = watchlist
  , nthread = 8
  ,print.every.n = 20
)

[0]	train-auc:0.919709
[20]	train-auc:0.922414
[40]	train-auc:0.924347
[60]	train-auc:0.926047
[80]	train-auc:0.927509
[100]	train-auc:0.929309
[120]	train-auc:0.930988
[140]	train-auc:0.932307

