library(caret)
library(e1071)
library(class)
library(cluster)
library(fpc)
library(ggplot2) 
library(factoextra)

library(rpart)
library(rpart.plot)
library(dplyr)
library(caret)
library(caretEnsemble) # training multiple models, custom ensembles
library(parallel)
library(ggscatter)


data <- read.csv("community_health_all_time.csv",stringsAsFactors = FALSE)

score <- read.csv("point_settings.csv",stringsAsFactors = FALSE)
dim(data)
summary(data)
summary (score)

#data preprocessing

comm <- data[,-c(18:44)]
comm[mapply(is.infinite, comm)] <- NA
comm[is.na(cluster)] <- 0
comm$total_health_score[comm$total_health_score < 100] <- 0
comm$total_health_score[comm$total_health_score >= 100] <- 1
score[is.na(score)] <- 0

score[is.na(score)] <- 0
#df = data[,-c(1)]

## general Plot
b <- ggplot(cluster, aes(x = total_health_score, y = avg_total_word_count))
# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 



ggplot(cluster, aes(total_health_score)) + 
  geom_histogram(binwidth=50, fill = 'blue')
  
ggplot(cluster, aes(listening_score)) + 
  geom_histogram(binwidth=20, fill = 'purple')
ggplot(cluster, aes(sharing_score)) + 
  geom_histogram(binwidth=20, fill = 'orange')
ggplot(cluster, aes(interacting_score)) + 
  geom_histogram(binwidth=20, fill = 'steelblue')

library(ggplot2)




#join both dataset based on board_id. left outer join
join_data <- merge( x = comm,  y = score, by ='board_id', all.x = FALSE)
join_data$total_health_score[join_data$total_health_score < 100] <- 0
join_data$total_health_score[join_data$total_health_score >= 100] <- 1

# set Default variable
join_data$total_health_score <- factor(join_data$total_health_score)

# to create a barplot
plot(join_data$total_health_score,
     main = "Classes by Enagement Level", col = c("red","steelblue"),
     ylab = "Classes", pch = 19)

#join_data <- na.omit(join_data) 
summary (cluster)
noms <- c("board_title", "creation_date", "X.network.name",'board_id','end_date')
nums <- names(cluster)[!names(cluster) %in% c( "total_health_score", noms)]

#regression 
model=glm(total_health_score~X.board.computed.fields.hyperlink.click.count+X.board.computed.fields.orphaned.posts.count+
                  X.board.computed.fields.days.since.start+X.board.computed.fields.multimedia.count
          +X.board.computed.fields.connected.users.count+X.board.computed.fields.reactions.count+
                  X.board.computed.fields.post.views.count+X.board.computed.fields.hyperlink.count
          + X.board.computed.fields.follower.count+X.board.computed.fields.comment.count+X.board.computed.fields.word.count
          +X.board.computed.fields.post.count+X.board.computed.fields.mentions.count+post_value+post_word_min
          +total_points_awarded_via_accolades
          ,family="binomial",data=join_data)
summary(model)

summary
vars <- names(join_data)[!names(join_data) %in% c( "periodic_target", "periodic_max","comment_received_value",'post_word_min','post_value',
                                                 'comment_word_min', 'avg_special_reaction_value','number_of_accolades',
                                                 'number_of_grading_periods','total_participation_goal','end_date','comment_value','total_health_score',
                                                 'X.board.computed.fields.days.since.start', noms)]


#install.packages('pROC')

# Initialize seed 
set.seed(232) 

# Create list of training indices
sub <- createDataPartition(y = join_data$total_health_score, 
                           p = 0.80, 
                           list = FALSE)  # splitting traning and testing dataset
# create train dataframe
train <- join_data[sub, ] 

# create test dataframe
test <-  join_data[-sub, ]
summary(train)
library(rpart)
library(rpart.plot)

##
cc.rpart <- rpart(formula = total_health_score ~ ., 
                  data = train[ ,c(vars, "total_health_score")], 
                  method = "class",
                  control = rpart.control(minsplit=30, cp=0.005))
cc.rpart

# We can see the basic output of our
# Decision Tree model
cc.rpart

# obtain variable importance
cc.rpart$variable.importance


## Tree Plots
rpart.plot(cc.rpart, extra = 9)



## Training Performance
#generate class predictions for our training set
base.trpreds <- predict(object = cc.rpart, 
                        newdata = train,
                        type = "class")
summary(cc.rpart)
confusionMatrix(base.trpreds, as.factor(train$total_health_score))
# Obtain a confusion matrix and obtain performance
# measures for our model applied to the
#training dataset (train).
DT_train_conf <- confusionMatrix(data = base.trpreds, # predictions
                                 reference = as.factor(train$total_health_score), # actual
                                 positive = "1",
                                 mode = "everything")
DT_train_conf
#confusion <- table(train$total_health_score,base.testpreds)
## Testing Performance
#generate class predictions for our testing set
base.testpreds <- predict(object = cc.rpart, 
                          newdata = test, 
                          type = "class")


# Obtain a confusion matrix and obtain performance 
# measures for our model applied to the
# testing dataset (test).
DT_test_conf <- confusionMatrix(data = base.testpreds, # predictions
                                reference = as.factor(test$total_health_score), # actual
                                positive = "1",
                                mode = "everything")
DT_train_conf

## Goodness of Fit
# Overall
round(cbind(Training = DT_train_conf$overall,
      Testing = DT_test_conf$overall),2)


### 2. Hyperparameter Tuning Model
#perform a grid search for the 
# optimal cp value.
# choose the cp that is associated with the smallest 
# cross-validated error (highest accuracy)
grids <- expand.grid(cp = seq(from = 0,
                              to = 0.05,
                              by = 0.005))
grids

# Cross validation using repeated method
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3,
                     search = "grid")
# Initialize a random seed for cross validation
set.seed(232)

# train the DT model using 10-Fold Cross 
# Validation (repeated 3 times).
DTFit <- train(form = total_health_score ~ ., 
               data = train[ ,c(vars, "total_health_score")], 
               method = "rpart", 
               trControl = ctrl, 
               tuneGrid = grids) 

# View the results of our
# cross validation
DTFit

# Plot the cp value vs. Accuracy
plot(DTFit)
