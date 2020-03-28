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

forest_norm1<-cbind(forest[,c(31)], forest_nt)
forest_train1<-forest_norm1[1:417,]
forest_test1<-forest_norm1[418,517,]

forest_train<-forest_norm[1:417,]
forest_test<-forest_norm[418:517,]
prop.table(table(forest_norm$size_category))
prop.table(table(forest_train$size_category))
prop.table(table(forest_test$size_category))
install.packages("neuralnet")
library(neuralnet)
library(nnet)
install.packages("nnet")
attach(forest_norm1)
model<-neuralnet(formula = forest_norm1$`forest[, c(31)]`~., data = forest_train1)
