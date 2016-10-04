#Copy the address then change back slashes to front slashes
#Setting the working directory
setwd("E:/Computer Engg/Machine learning/RScripts/Titanic")
#Loading the data
train = read.csv("train.csv")
test = read.csv("test.csv")

#understanding the data
head(train)
str(train)
summary(train)
is.na(train$Sex)
table(train$Sex)
#Analysing Role of Sex
table(train$Survived)
prop.table(table(train$Survived,train$Sex),1)
count = table(train$Survived,train$Sex)
print(count)

#Percentage of ppl survived according to Sex
male_survived_percentage = count[4]/(count[3]+count[4]) 
print(male_survived_percentage)
female_survived_percentage = count[2]/(count[1]+count[2])
print(female_survived_percentage)

#Analysisig role of Passenger class
table(train$Pclass)
var =table(train$Survived,train$Pclass)
print(var)
prop.table(table(train$Survived,train$Pclass),1)

#percentage of ppl survived according to class
p1 = (var[2])/(var[1]+var[2])
p2 = (var[4])/(var[4]+var[3])
p3 = (var[6])/(var[6]+var[5])
print(p1)
print(p2)
print(p3)


#Replacing missing values in Age
train$Age[is.na(train$Age)] = mean(train$Age,na.rm = T)
train$child = NA
train$child[train$Age >= 18] = 0
train$child[train$Age < 18] = 1
table(train$child)
age_per=table(train$Survived,train$child)
print(age_per)
#percentage
child_per = age_per[4]/(age_per[3]+age_per[4])
print(child_per)

adult_per = age_per[2]/(age_per[2]+age_per[1])
print(adult_per)

str(train)

#  Adding new column
train$dummy = NULL
print(train$dummy)
train$dummy[train$Sex == "male"] = 0
train$dummy[train$Sex == "female"] = 1
is.na(train$dummy)
print(train$dummy)

#Cleaning data
train$PassengerId = NULL
train$Ticket = NULL
train$Fare = NULL
train$Cabin = NULL
train$Embarked = NULL
train$Name = NULL
train$Sex = NULL
str(train)

#cleaning test data
test$PassengerId = NULL
test$Ticket = NULL
test$Fare = NULL
test$Cabin = NULL
test$Embarked = NULL
test$Name = NULL

test$dummy = NULL
print(train$dummy)
test$dummy[test$Sex == "male"] = 0
test$dummy[test$Sex == "female"] = 1
is.na(test$dummy)
print(test$dummy)
train$Sex = NULL
test$Age[is.na(test$Age)] = mean(test$Age,na.rm = T)
#Adding child column
test$child = NA
test$child[test$Age >= 18] = 0
test$child[test$Age < 18] = 1
is.na(test$child)
str(test)
test$Sex<-NULL
nrow(test)
model = glm(Survived ~.,family = binomial,data = train)
str(model)
str(train)
summary(model)
str(test)
prediction = predict.glm(model,test,type = "response")
str(prediction)
length(prediction)
print(prediction)
summary(prediction)
head(prediction)
length(prediction)

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
str(prediction)
head(prediction)
table(prediction)

#making a dataframe
test = read.csv("test.csv")
solution = data.frame(Id = test$PassengerId, Survived = prediction)
str(test)

