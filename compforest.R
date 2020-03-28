library(readr)
compny<-read.csv(file.choose())
View(compny)
summary(compny)
attach(compny)
compny$segment<-ifelse(Sales<=4.8,"Strong","Weak")
compny$segment<-as.factor(compny$segment)
str(compny)
comp_rand<-compny[order(runif(400)),]
comp_train<-comp_rand[1:300,]
comp_test<-comp_rand[301:400,]
prop.table(table(compny$segment))
prop.table(table(comp_train$segment))
prop.table(table(comp_test$segment))
library(randomForest)
comp_forest<-randomForest(segment~.,data=comp_train)
comp_forest
importance(comp_forest)
varImpPlot(comp_forest)
comp_pred<-predict(comp_forest,newdata = comp_test, type='class')
comp_pred
library(caTools)
library(caret)

CrossTable(table(comp_pred,comp_test$segment))
