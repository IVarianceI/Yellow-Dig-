library(caret)
library(e1071)
library(class)
library(cluster)
library(fpc)
library(ggplot2) 
library(factoextra)
#library(Rtsne)
library(rpart)
library(rpart.plot)
library(dplyr)
library(caret)
library(caretEnsemble) # training multiple models, custom ensembles
library(parallel)
library(ggscatter)


data <- read.csv("community_health_all_time.csv",stringsAsFactors = FALSE) #community health 
dim(data)
summary(data)

cluster <- data[,-c(3:18)]
cluster[mapply(is.infinite, cluster)] <- NA
cluster[is.na(cluster)] <- 0

score[is.na(score)] <- 0



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

# set Default variable
cluster$total_health_score <- factor(cluster$total_health_score)


summary (cluster)
noms <- c("board_title", "creation_date", "X.network.name",'board_id','end_date')
nums <- names(cluster)[!names(cluster) %in% c( "total_health_score", noms)]

vars <- c( nums)
summary(cluster[,c(vars,"total_health_score")])
cor_vars <- cor(x = cluster[ ,nums])

symnum(x = cor_vars,
       corr = TRUE)
high_corrs <- findCorrelation(x = cor_vars, 
                              cutoff = .75, 
                              names = TRUE)

high_corrs
varsC <- vars[!vars %in% high_corrs] # remove high correlation varaibles
varsC


#------------------------------------------
######### Clustering #########
#------------------------------------------


## Standardization
# A transformation should be completed to make
# numerical variables comparable and similarly 
# scaled
summary(join_data)

# Identifying Outliers
# Z-Score Method
join_data[abs(scale(join_data$X.board.computed.fields.hyperlink.click.count)) > 200, ]
join_data[abs(scale(join_data$ X.board.computed.fields.orphaned.posts.count)) > 200, ]
join_data[abs(scale(join_data$ X.board.computed.fields.days.since.start)) > 100, ]
join_data[abs(scale(join_data$X.board.computed.fields.multimedia.count)) > 200, ]
join_data[abs(scale(join_data$X.board.computed.fields.connected.users.count)) > 200, ]
join_data[abs(scale(join_data$X.board.computed.fields.reactions.count)) > 2000, ]
join_data[abs(scale(join_data$X.board.computed.fields.post.views.count)) > 5000, ]
join_data[abs(scale(join_data$X.board.computed.fields.hyperlink.count)) > 200, ]
join_data[abs(scale(join_data$X.board.computed.fields.follower.count)) > 100, ]
join_data[abs(scale(join_data$X.board.computed.fields.comment.count)) > 2000, ]
join_data[abs(scale(join_data$ X.board.computed.fields.word.coun)) > 200000, ]
join_data[abs(scale(join_data$ X.board.computed.fields.post.count)) > 500, ]
join_data[abs(scale(join_data$ X.board.computed.fields.mentions.count)) > 100, ]

#standardization 
cen_sc <- preProcess(x = cluster[vars],
                     method = c("BoxCox", "center", "scale"))
CC_Cluster_sc <- predict(object = cen_sc,
                         newdata = cluster)


# Look for an elbow point
set.seed(232)
library(factoextra)

load("Clustering.RData")
args(wss_plot)
wss_plot(scaled_data = CC_Cluster_sc[ ,varsC], 
         method = "kmeans", 
         max.k = 15, 
         seed_no = 232) 

# We choose k=4
set.seed(232)

kmeans4 <- kmeans(x = CC_Cluster_sc[ ,vars], # data
                  centers = 3, # # of clusters
                  trace = FALSE, 
                  nstart = 25)

fviz_cluster(object = kmeans4, 
             data = CC_Cluster_sc[,vars])

kmeans4

cluster_result <- data.frame(cluster,
                             cluster = as.factor(kmeans4$cluster))

## Describe the Cluster Solution

clus_means_kMC <- aggregate(x = CC_Cluster_sc[ ,vars], 
                            by = list(kmeans4$cluster), 
                            FUN = mean)
clus_means_kMC


# Use the matplot() function to
# visualize the (scaled) cluster centers
# to observe differences
# Use the matplot() function to
# visualize the (scaled) cluster centers
# to observe differences
# Use the matplot() function to
# visualize the (scaled) cluster centers
# to observe differences
matplot(t(clus_means_kMC[ ,-1]), 
        type = "l", 
        ylab = "", 
        xlim = c(0, 16), 
        xaxt = "n", 
        col = 1:6, 
        lty = 1:6, 
        main = "Cluster Centers") 

# Add custom x-axis labels
axis(side = 1, 
     at = 1:16, 
     labels = vars, 
     las = 2) 
legend("left", 
       legend = 1:3, 
       col = 1:3, 
       lty = 1:3, 
       cex = 0.6) 



library(ggplot2)
#install.packages("ggdendro")
library(ggdendro)
# Ward Hierarchical Clustering
d <- dist(join_data, method = "ward.D2") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit, hang = -1) # display dendogram
ggdendrogram(fit, rotate = TRUE, size = 4, theme_dendro = FALSE)
groups <- cutree(fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=3, border="red")

## list of cluster assignments
o=order(kmeans4$cluster)

final <- data.frame(data$board_title[o],kmeans4$cluster[o],data$X.network.name[o])
cluster_1 <- final[final$kmeans4.cluster.o. == '1',]
cluster_1
final %>% 
  select(data.X.network.name.o.,data.board_title.o., kmeans4.cluster.o.) %>% 
  filter(kmeans4.cluster.o. == 1)

final %>% 
  select(data.X.network.name.o.,data.board_title.o., kmeans4.cluster.o.) %>% 
  filter(kmeans4.cluster.o. == 2)

final %>% 
  select(data.X.network.name.o.,data.board_title.o., kmeans4.cluster.o.) %>% 
  filter(kmeans4.cluster.o. == 1)
cluster_2 <- final[final$kmeans4.cluster.o. == 2,]
cluster_2

cluster_3 <- final[final$kmeans4.cluster.o. == 3,]
cluster_3
summary(final)
