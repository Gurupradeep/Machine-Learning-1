install.packages("ROCR")
library(ROCR)
data(ROCR.simple)
print(ROCR.simple$labels)

#$Predictions has continous values between O and 1 which are predicted by our model and $labels are the actual output values which should be binary.
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf)
