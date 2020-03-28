library(readr)
crime<-read.csv(file.choose())
attach(crime)
colnames(crime)
sum(is.na(crime))
norm_data<-scale(crime[,2:5])
?dist
a<-dist(norm_data,method = "euclidean")
a
?hclust
dlf<-hclust(a,method ="complete")
str(dlf)
plot(dlf)
plot(dlf,hang=-1)
rect.hclust(dlf,k=4, border="blue")
group<-cutree(dlf,k=4)
group
clustername<-as.matrix(group)
final<-data.frame(crime,clustername)
View(final)
final1<-final[,c(6,1:5)]
write.csv(final1,file="cr.csv", row.names=F)
getwd()
aggregate(crime[,-1],list(final$clustername),mean)
