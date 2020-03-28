library(readr)
forest<-read.csv(file.choose())
table(forest$size_category)
attach(forest)

forest$size_category<-ifelse(size_category=="small",0,1)
str(forest)
summary(forest)
normalize<-function(x){
  temp<-(x-min(x))/(max(x)-min(x))
}
forest_nt<-as.data.frame(lapply(forest[,3:30], normalize))
forest_norm<-cbind(forest[,c(1:2,31)],forest_nt)
str(forest_norm)

forest_train<-forest_norm[1:417,]
forest_test<-forest_norm[418:517,]
prop.table(table(forest_norm$size_category))
prop.table(table(forest_train$size_category))
prop.table(table(forest_test$size_category))
library(kernlab)
attach(forest_norm)
model<-ksvm(size_category~.,data=forest_train, kernel="vanilla")
model_pred<-predict(model, newdata=forest_test)
library(gmodels)
library(caret)
confusionMatrix(table(model_pred, forest_test$size_category))

