### WINE DATA 

##############################
#Run all of this first.
library(readr)

#Model 1
data <- na.omit(read.csv("wine_quality.csv"))

#Normalize the data
type<-data$type
data <- as.data.frame(lapply(data[,c(2:13)], nor))
data <- cbind(data,type)
data$type<-as.factor(data$type)


#Model 2+3
data2 <- data #Make sure the data is normalized before you proceed.
data2$quality_6 <-"bad"
data2$quality_6[data2$quality>=6] <- "good"



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



##Model 1: Classifying Type of Wine on All Data
data


#Make Train and Test
set.seed(500)
data$label <- sample(2, nrow(data), replace=TRUE, prob=c(0.80, 0.20))

test_data <- subset(data,label==2)
train_data <- subset(data,label==1)





##Model 2: Predicting Quality (Good/Bad) For Red Wine

red_wine <- subset(data2,type=="red")

#Make Train and Test
set.seed(500)
red_wine$label <- sample(2, nrow(red_wine), replace=TRUE, prob=c(0.80, 0.20))

test_red_wine <- subset(red_wine,label==2)
train_red_wine <- subset(red_wine,label==1)





##Model 3: Predicting Quality (Good/Bad) For White Wine

white_wine <- subset(data2,type=="white")

#Make Train and Test
set.seed(500)
white_wine$label <- sample(2, nrow(white_wine), replace=TRUE, prob=c(0.80, 0.20))

test_white_wine <- subset(white_wine,label==2)
train_white_wine <- subset(white_wine,label==1)

