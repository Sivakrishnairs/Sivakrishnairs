library(readr)
wbcd<-read.csv(file.choose())
wbcd1<-wbcd[-1]
table(wbcd1$diagnosis)
str(wbcd1)
summary(wbcd)
wbcd1$diagnosis<-factor(wbcd$diagnosis,levels  =c("B","M"),labels =c("Benign","Malignant"))
View(wbcd)
round(prop.table(table(wbcd1$diagnosis))*100, digits = 1)
normalize<-function(x){
  return((x-min(x)))/(max(x)-min(x))
}
normalize(wbcd1$radius_mean)
View(wbcd1)
wbcd_n<-as.data.frame(lapply(wbcd1[2:31],normalize))
wbcd_norm<-scale(wbcd1[2:31])
summary(wbcd_norm)
wbcd_train<-wbcd_norm[1:469,]
wbcd_test<-wbcd_norm[470:569,]
wbcd_train_labels<-wbcd1[1:469,1]#creating as X
wbcd_test_labels<-wbcd1[470:569,1]#creating as Y as knn will take only x,y
install.packages("class")
library(class)
wbcd_test_pred<-knn(train = wbcd_train,test=wbcd_test,cl=wbcd_train_labels,k=1)
library(gmodel)
install.packages("gmodels")
library(gmodels)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq = FALSE)
final<-knn(train=wbcd_norm,test=wbcd_norm,cl=wbcd1$diagnosis,k=1)
mean(final==wbcd1$diagnosis)
table(final,wbcd1$diagnosis)
