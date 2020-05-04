### WINE DATA 

##############################
#Run all of this first.
library(readr)

#Model 1
data <- na.omit(read.csv("wine_quality.csv"))


#Normalization function
nor <-function(x) {
  (x -min(x))/(max(x)-min(x))
}                 

#Accuracy function
accuracy <- function(x){
  sum(diag(x)/(sum(rowSums(x)))) * 100
}   

#Precision function
precision <- function(x){
  x[2,2]/(x[2,2]+x[1,2])
}                

###############################



#Normalize the data
type<-data$type
data <- as.data.frame(lapply(data[,c(2:13)], nor))
data <- cbind(data,type)
data$type<-as.factor(data$type)


#Model 2+3
data2 <- data #Make sure the data is normalized before you proceed.
data2$quality_6 <-"bad"
data2$quality_6[data2$quality>=6] <- "good"




##Model 1: Classifying Type of Wine on All Data
data


#Make Train and Test

##K-Means
library(factoextra)
library(tidyverse)


data <- data %>% 
  select(-type)

data2 <- data %>% 
  scale()

#We know it's 2
fviz_nbclust(data2,FUNcluster = kmeans)
library(gt)

set.seed(123)
km.res <- kmeans(data2, 4, nstart = 25)

gt(aggregate(data, by=list(cluster=km.res$cluster), mean))


fviz_cluster(km.res,data2) 


##Model 2: Predicting Quality (Good/Bad) For Red Wine

red_wine <- subset(data2,type=="red")


#We know it's 2
fviz_nbclust(red_wine,FUNcluster = kmeans)


set.seed(123)
km.res <- kmeans(red_wine, 2, nstart = 25)

gt(aggregate(red_wine, by=list(cluster=km.res$cluster), mean))

dd <- cbind(red_wine, cluster = km.res$cluster)

fviz_cluster(km.res,red_wine) 




##Model 3: Predicting Quality (Good/Bad) For White Wine

white_wine <- subset(data2,type=="white")


#We know it's 2
fviz_nbclust(white_wine,FUNcluster = kmeans)


set.seed(123)
km.res <- kmeans(white_wine, 2, nstart = 25)

gt(aggregate(white_wine, by=list(cluster=km.res$cluster), mean))

dd <- cbind(white_wine, cluster = km.res$cluster)

fviz_cluster(km.res,white_wine) 


