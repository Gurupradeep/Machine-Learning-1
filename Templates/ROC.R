install.packages("ROCR")
library(ROCR)
data(ROCR.simple)
print(ROCR.simple$labels)

#$Predictions has continous values between O and 1 which are predicted by our model and $labels are the actual output values which should be binary.
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf)

#Get optimal threshold by specifying tpr and fpr
str(perf)
cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
head(cutoffs)
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.1))
