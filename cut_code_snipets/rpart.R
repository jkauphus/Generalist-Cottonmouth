#Example of rpart
library(rpart)
library(rpart.plot)
library(dplyr)

#Data
study<-read.csv("data/Cottonmouth-Datasheet.csv")
glimpse(study)
s<-study[,c(3:24)]
s<-na.omit(s)
glimpse(s)

#rpart data
s1<-s[,c(1:20)]
glimpse(s1)

#Character Trees
ttree<-rpart(Type~., data = s1, method = "class") #TRAINING DATASET
rpart.plot(ttree, type = 3, extra = 8,digits = 3, fallen.leaves = TRUE)
