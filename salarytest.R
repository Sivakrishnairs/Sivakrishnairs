library(readr)
sal<-read.csv(file.choose())#data set
sal_mod<-ifelse(sal$Salary==" >50K","High","low")#catorizing to high & low
sal<-data.frame(sal,sal_mod)# adding column sal_mod to sal
sal<-sal[,-14]# removing 14 column

library(e1071)# library for navie bayes
colnames(sal)
attach(sal)
tain<-sal[1:14000,]# spliting the data to tran
test<-sal[14001:15060,]# soliting data to test
attach(sal)
# bulding model
model<-naiveBayes(sal_mod~age+education+occupation+hoursperweek,data = tain )
# preding the model for test data
pred1<-predict(model, newdata=test, type="class")

pred1
table(pred1)
library(caret)# library for confusion matrix
confusionMatrix(table(pred1,test$sal_mod))# confusion matrix
tp<-116
tn<-720
fn<-124
fp<-100
pression<-tp/(tp+fp)
rec<-tp/(tp+fn)
specificity=tn/(tn+fp)
f1<-2*(pression*rec)/(pression+rec)
print(c(pression,rec,specificity,f1))