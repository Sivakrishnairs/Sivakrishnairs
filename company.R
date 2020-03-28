library(readr)
compny<-read.csv(file.choose())
View(compny)
summary(compny)
str(compny)
hist(compny$Sales)
boxplot(compny$Sales)
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
library(rpart)
model<-rpart(segment~.,data = comp_train)
plot(model,margin = 0.1)


text(model,use.n = TRUE,pretty = TRUE,cex=0.8)
pred<-predict(model,newdata = comp_test,type="class")
head(pred,n=15)
