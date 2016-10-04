setwd("E:/Computer Engg/Machine learning/RScripts");
train<-read.csv("data.csv");
train_data<-train[!is.na(train$shot_made_flag),]
test_data<-train[is.na(train$shot_made_flag),]
#head(train_data)
train_data$game_event_id<-NULL
train_data$game_id<-NULL
train_data$team_id<-NULL
train_data$team_name<-NULL
train_data$game_date<-NULL
train_data$matchup<-NULL
train_data$shot_id<-NULL
#test_data$game_event_id<-NULL
#test_data$game_id<-NULL
#test_data$team_id<-NULL
#test_data$team_name<-NULL
#test_data$game_date<-NULL
#test_data$matchup<-NULL
#test_data$shot_id<-NULL
#test_data$shot_made_flag<-NULL

test_data$action_type = as.numeric(factor(test_data$action_type))
test_data$combined_shot_type = as.numeric(factor(test_data$combined_shot_type))
test_data$season = as.numeric(factor(test_data$season))
test_data$shot_type = as.numeric(factor(test_data$shot_type))
test_data$shot_zone_area = as.numeric(factor(test_data$shot_zone_area))
test_data$shot_zone_basic = as.numeric(factor(test_data$shot_zone_basic))
test_data$shot_zone_range = as.numeric(factor(test_data$shot_zone_range))
test_data$opponent = as.numeric(factor(test_data$opponent))
train_data$action_type = as.numeric(factor(train_data$action_type))
train_data$combined_shot_type = as.numeric(factor(train_data$combined_shot_type))
train_data$season = as.numeric(factor(train_data$season))
train_data$shot_type = as.numeric(factor(train_data$shot_type))
train_data$shot_zone_area = as.numeric(factor(train_data$shot_zone_area))
train_data$shot_zone_basic = as.numeric(factor(train_data$shot_zone_basic))
train_data$shot_zone_range = as.numeric(factor(train_data$shot_zone_range))
train_data$opponent = as.numeric(factor(train_data$opponent))
library(randomForest)

train_data$dist<-(train_data$loc_x**2+train_data$loc_y**2)**0.5
test_data$dist<-(test_data$loc_x**2+test_data$loc_y**2)**0.5
train_data$timeleft<-train_data$minutes_remaining*60+train_data$seconds_remaining
test_data$timeleft<-test_data$minutes_remaining*60+test_data$seconds_remaining
train_data$seconds_remaining<-NULL
test_data$seconds_remaining<-NULL
train_data$minutes_remaining<-NULL
test_data$minutes_remaining<-NULL
forest<-randomForest(shot_made_flag~.,data=train_data,importance=TRUE,ntree=800)
my_prediction<-predict(forest,test_data)
answer<-data.frame(shot_id=test_data$shot_id,shot_made_flag=my_prediction)
write.csv(answer,file="check.csv",row.names=FALSE)
