setwd("~/Desktop/Desktop/YellowDig")
## Load libraries
library(ggplot2) 
library(caret) 
library(cluster) # clustering
#install.packages('factoextra')
library(factoextra) # cluster validation, plots
#install.packages('fpc')
library(fpc) # cluster validation
#install.packages('Rtsne')
library(Rtsne) # dimension reduction
#df4 Closed Won
#df5 Total Pipeline
df4<-read.csv(file = "combine1.csv",stringsAsFactors = TRUE)
df5<-read.csv('combine2.csv',stringsAsFactors = TRUE)
df4<- subset(df4, select = -c(X, Account.Name, Last.Activity, Opportunity.Name, Created.Date))
df5<-subset(df5, select = -c(X,Account.Name, Opportunity.Name,Close.Date))

df4_factors<-c('Account.Owner','Account.Size','Account.Type','Billing,State.Province','Sales.Territory','Account.Status','Opportunity.Owner',
               'Lead.Source','License.Type','Billing.Model','Stage','Fiscal.Period','Type')
df4_ords <- c('Account.Size')
df4$Account.Size <- factor(x = df4$Account.Size, 
                       levels = c("SMB", "MM", "ENT"),
                       ordered = TRUE)
table(df4$Billing.State.Province)
library(stringr)
df4$Billing.State.Province=str_replace(df4$Billing.State.Province,'Arizona','AZ')
df4$Billing.State.Province=str_replace(df4$Billing.State.Province,'Massachusetts','MA')
df4$Billing.State.Province=str_replace(df4$Billing.State.Province,'Pennsylvania','PA')
df4$Billing.State.Province=str_replace(df4$Billing.State.Province,'Vermont','VT')
table(df4$Billing.State.Province)
df4$Billing.State.Province<-as.factor((df4$Billing.State.Province))

df5$Account.Size <- factor(x = df5$Account.Size, 
                           levels = c("SMB", "MM", "ENT"),
                           ordered = TRUE)
table(df5$Billing.State.Province)
df5$Billing.State.Province=str_replace(df5$Billing.State.Province,'Arizona','AZ')
df5$Billing.State.Province=str_replace(df5$Billing.State.Province,'Massachusetts','MA')
df5$Billing.State.Province=str_replace(df5$Billing.State.Province,'Pennsylvania','PA')
df5$Billing.State.Province=str_replace(df5$Billing.State.Province,'Vermont','VT')
df5$Billing.State.Province<-as.factor((df5$Billing.State.Province))

is.na(df5)
is.na(df4)

#cen <- preProcess(x = df4,method = "medianImpute")
#df4_nn <- predict(object = cen,newdata = df4) #NA replaced之后的dataset

#cen <- preProcess(x = df5,method = "medianImpute")
#df5_nn <- predict(object = cen,newdata = df5) #NA replaced之后的dataset


df4_nn<-df4
df5_nn<-df5


cen_sc_df5 <- preProcess(x = df5_nn,
                     method = c("center", "scale"))
df5_sc <- predict(object = cen_sc_df5,
                 newdata = df5_nn)

cen_sc_df4 <- preProcess(x = df4_nn,
                         method = c("center", "scale"))
df4_sc <- predict(object = cen_sc_df4,
                  newdata = df4_nn)

cen_yj_df4 <- preProcess(x = df4_nn,
                     method = "YeoJohnson")
df4_yj <- predict(object = cen_yj_df4,
                 newdata = df4_nn)

cen_yj_df5 <- preProcess(x = df5_nn,
                         method = "YeoJohnson")
df5_yj <- predict(object = cen_yj_df5,
                  newdata = df5_nn)

#Replace NA with Unknown






hdist_df4 <- daisy(x = df4_yj, 
               metric = "gower")
summary(hdist_df4)
hdist_df5 <- daisy(x = df5_yj, 
                   metric = "gower")
summary(hdist_df5)

sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(hdist_df4, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)  #k=3




sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(hdist_df5, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)   #k=3




set.seed(831)
pam1 <- pam(x = hdist_df4,
            diss = TRUE, #use dissimilarity， not similarity
            k = 3) # 3 cluster solution
pam1

df4[pam1$medoids, ]

head(pam1$clustering)
pam1$clustering

df4$Clustering<-pam1$clustering
set.seed(832)
pam2 <- pam(x = hdist_df5,
            diss = TRUE, #use dissimilarity， not similarity
            k = 3) # 3 cluster solution
pam2

df5[pam2$medoids, ]

head(pam2$clustering)
pam2$clustering
df5$Clustering<-pam2$clustering




ld_dist_df4 <- Rtsne(X = hdist_df4, 
                 is_distance = TRUE)
lddf_dist_df4<- data.frame(ld_dist_df4$Y)



ggplot(data = lddf_dist_df4, 
       mapping = aes(x = X1, y = X2)) +
  geom_point(aes(color = factor(pam1$clustering))) +
  labs(color = "Cluster")


ld_dist_df5 <- Rtsne(X = hdist_df5, 
                     is_distance = TRUE)
lddf_dist_df5<- data.frame(ld_dist_df5$Y)



ggplot(data = lddf_dist_df5, 
       mapping = aes(x = X1, y = X2)) +
  geom_point(aes(color = factor(pam2$clustering))) +
  labs(color = "Cluster")

table(df4$Clustering)
table(df5$Clustering)

write.csv(df5,'df5.csv')
write.csv(df4,'df4.csv')

df4_new<-read.csv(file = "df4.csv",stringsAsFactors = TRUE)
df5_new<-read.csv('df5.csv',stringsAsFactors = TRUE)
df4_new<-subset(df4_new, select = -c(X))
df5_new<-subset(df5_new, select = -c(X))




















df4_cluster1<-df4_new[which(df4_new$Clustering==1),]
table(df4_cluster1$Account.Size)
df4_cluster2<-df4_new[which(df4_new$Clustering==2),]
df4_cluster3<-df4_new[which(df4_new$Clustering==3),]

df5_cluster1<-df5_new[which(df5_new$Clustering==1),]
df5_cluster2<-df5_new[which(df5_new$Clustering==2),]
df5_cluster3<-df5_new[which(df5_new$Clustering==3),]

table(df5_cluster1$Account.Status)
table(df5_cluster2$Account.Status)
table(df5_cluster3$Account.Status)

table(df5_cluster1$Account.Size)
table(df5_cluster2$Account.Size)
table(df5_cluster3$Account.Size)




ggplot(df4_cluster1,aes(Sales.Territory,fill=Account.Owner))+geom_bar()
ggplot(df4_cluster2,aes(Sales.Territory,fill=Account.Owner))+geom_bar()
ggplot(df4_cluster3,aes(Sales.Territory,fill=Account.Owner))+geom_bar()
ggplot(df5_cluster1,aes(Sales.Territory,fill=Account.Owner))+geom_bar()
ggplot(df5_cluster2,aes(Sales.Territory,fill=Account.Owner))+geom_bar()
ggplot(df5_cluster3,aes(Sales.Territory,fill=Account.Owner))+geom_bar()

ggplot(df4_cluster1,aes(Account.Type,fill=Account.Owner))+geom_bar()
ggplot(df4_cluster2,aes(Account.Type,fill=Account.Owner))+geom_bar()
ggplot(df4_cluster3,aes(Account.Type,fill=Account.Owner))+geom_bar()
ggplot(df5_cluster1,aes(Account.Type,fill=Account.Owner))+geom_bar()
ggplot(df5_cluster2,aes(Account.Type,fill=Account.Owner))+geom_bar()
ggplot(df5_cluster3,aes(Account.Type,fill=Account.Owner))+geom_bar()

ggplot(df4_cluster1,aes(Account.Type,fill=Sales.Territory))+geom_bar()
ggplot(df4_cluster2,aes(Account.Type,fill=Sales.Territory))+geom_bar()
ggplot(df4_cluster3,aes(Account.Type,fill=Sales.Territory))+geom_bar()
ggplot(df5_cluster1,aes(Account.Type,fill=Sales.Territory))+geom_bar()
ggplot(df5_cluster2,aes(Account.Type,fill=Sales.Territory))+geom_bar()
ggplot(df5_cluster3,aes(Account.Type,fill=Sales.Territory))+geom_bar()

ggplot(df4_cluster1,aes(Sales.Territory,fill=Account.Size))+geom_bar()
ggplot(df4_cluster2,aes(Sales.Territory,fill=Account.Size))+geom_bar()
ggplot(df4_cluster3,aes(Sales.Territory,fill=Account.Size))+geom_bar()
ggplot(df5_cluster1,aes(Sales.Territory,fill=Account.Size))+geom_bar()
ggplot(df5_cluster2,aes(Sales.Territory,fill=Account.Size))+geom_bar()
ggplot(df5_cluster3,aes(Sales.Territory,fill=Account.Size))+geom_bar()

ggplot(df4_cluster1,aes(Stage,fill=Opportunity.Owner))+geom_bar()
ggplot(df4_cluster2,aes(Stage,fill=Opportunity.Owner))+geom_bar()
ggplot(df4_cluster3,aes(Stage,fill=Opportunity.Owner))+geom_bar()
ggplot(df5_cluster1,aes(Stage,fill=Opportunity.Owner))+geom_bar()
ggplot(df5_cluster2,aes(Stage,fill=Opportunity.Owner))+geom_bar()
ggplot(df5_cluster3,aes(Stage,fill=Opportunity.Owner))+geom_bar()

table(df5_cluster1$Opportunity.Owner)


ggplot(df4_cluster1,aes(Account.Type,fill=Account.Owner))+geom_bar()
ggplot(df4_cluster2,aes(Sales.Territory,fill=Account.Type))+geom_bar()

ggplot(df4_cluster2,aes(Sales.Territory,fill=Account.Owner))+geom_bar()
ggplot(df4_cluster1,aes(Sales.Territory,fill=Account.Type))+geom_bar()
ggplot(df5_cluster1,aes(Sales.Territory,fill=Account.Owner))+geom_bar()
ggplot(df5_cluster2,aes(Sales.Territory,fill=Account.Owner))+geom_bar()
ggplot(df5_cluster3,aes(Sales.Territory,fill=Account.Owner))+geom_bar()
ggplot(df5_cluster1,aes(Account.Owner,fill=Account.Type))+geom_bar()
ggplot(df5_cluster2,aes(Account.Owner,fill=Account.Type))+geom_bar()
ggplot(df5_cluster3,aes(Account.Owner,fill=Account.Type))+geom_bar()

