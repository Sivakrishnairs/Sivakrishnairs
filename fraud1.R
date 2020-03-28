library(readr)
fraud<-read.csv(file.choose())
str(fraud)# to find datatype for variable
fraud$fd<-ifelse(fraud$Taxable.Income<=30000,"Risk","Good")# to find whether it is risky or good 
fraud$fd<-as.factor(fraud$fd)#converting data type to factor
str(fraud)
summary(fraud)
fraud_rand<-fraud[order(runif(600)),]#ordering random unifrom numbers to fraud data set
fraud_train<-fraud_rand[1:500,]#spliting data to train
fraud_test<-fraud_rand[501:600,]#spliting data to test
prop.table(table(fraud_rand$fd))# finding proportion of risk & good
prop.table(table(fraud_train$fd))
prop.table(table(fraud_test$fd))
install.packages("rpart")#package for decission tree
attach(fraud)
library(rpart)
model<-rpart(fraud$fd~fraud$Undergrad+fraud$Marital.Status+fraud$Taxable.Income+fraud$City.Population+fraud$Work.Experience+fraud$Urban, data=fraud_train)# building the model
plot(model,margin = 0.1)
text(model,use.n = TRUE,pretty = TRUE,cex=0.8)#ploting the tree with text
pred<-predict(model,newdata = fraud_test,type="class")# predicting no test data
head(pred,n=20)# to check the model build

