library(readr)
glass<-read.csv(file.choose())

str(glass)
table(glass$Type)
round(prop.table(table(glass$type))*100, digits=1)
library(gmodels)
glass_n<-scale(glass)
glass_train<-glass_n[1:114,]
glass_test<-glass_n[115:214,]
glass_train_lable<-glass[1:114,10]
glass_test_lable<-glass[115:214,10]
library(gmodels)
library(class)

install.packages("gmodels")
glass_test_pred<-knn(train=glass_train,test=glass_test,cl=glass_train_lable,k=1)
CrossTable(x=glass_test_lable,y=glass_test_pred,prop.chisq = FALSE)
glass_test_pred<-knn(train=glass_train,test=glass_test,cl=glass_train_lable,k=25)
CrossTable(x=glass_test_lable,y=glass_test_pred,prop.chisq = FALSE)

glass_test_pred<-knn(train=glass_train,test=glass_test,cl=glass_train_lable,k=5)
CrossTable(x=glass_test_lable,y=glass_test_pred,prop.chisq = FALSE)
final<-knn(train=glass_n,test = glass_n,cl=glass$Type,k=1)
mean(final==glass$Type)
table(final,glass$Type)
