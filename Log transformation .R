#packages we use 
install.packages("readxl")
library("readxl")
install.packages("writexl")
library("writexl")
install.packages("openxlsx")
library("openxlsx")
install.packages("rio")
library("rio")
install.packages("tidyverse")
library("tidyverse")

# Load data
data <- read.csv("logreg4.csv") 

data=data[,-1]

# Log transformation 
data$X.board.computed.fields.hyperlink.click.count = log(data$X.board.computed.fields.hyperlink.click.count)
data$X.board.computed.fields.days.since.start = log(data$X.board.computed.fields.days.since.start)
data$X.board.computed.fields.multimedia.count = log(data$X.board.computed.fields.multimedia.count)
data$X.board.computed.fields.reactions.count = log(data$X.board.computed.fields.reactions.count)
data$X.board.computed.fields.hyperlink.count = log(data$X.board.computed.fields.hyperlink.count)
data$X.board.computed.fields.follower.count = log(data$X.board.computed.fields.follower.count)
data$X.board.computed.fields.comment.count = log(data$X.board.computed.fields.comment.count)
data$X.board.computed.fields.word.count = log(data$X.board.computed.fields.word.count)
data$X.board.computed.fields.post.count = log(data$X.board.computed.fields.post.count)
data$X.board.computed.fields.mentions.count = log(data$X.board.computed.fields.mentions.count)
data$post_value = log(data$post_value)
data$post_word_min = log(data$post_word_min)
data$comment_value = log(data$comment_value)
data$comment_word_min = log(data$comment_word_min)
data$number_of_accolades = log(data$number_of_accolades)
data$number_of_grading_periods = log(data$number_of_grading_periods)
data$periodic_max = log(data$periodic_max)
data$total_participation_goal = log(data$total_participation_goal)
data$periodic_target = log(data$periodic_target)

#Repleace inf in data by NA
data <- do.call(data.frame,                      # Replace Inf in data by NA
              lapply(data,
                     function(x) replace(x, is.infinite(x), NA)))
sum(is.na(data))
data<- drop_na(data)

install.packages("psych")
library(psych)
describe(data)


scaled.dat <- scale(data)


write.csv(scaled.dat, "scaled7.csv")
