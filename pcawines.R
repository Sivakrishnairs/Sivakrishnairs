library(readr)
wines<-read.csv(file.choose())
sum(is.na(wines))
View(wines)
cor(wines)
pcaobj<-princomp(wines,cor=TRUE,scores = TRUE,covmat = NULL)
summary(pcaobj)
str(pcaobj)
pcaobj$loadings
pcaobj$scores
plt(pcaobj)
biplot(pcaobj)
pcaobj$scores[,1:12]
mydata<-cbind(wines,pcaobj$scores[,1:2])
norm_clus<-scale(pcaobj$scores[,1:12])
dist1<-dist(norm_clus,method = "euclidean")
?hclust

fit1<-hclust(dist1,method = "complete")
plot(fit1,hang=-1)
group<-cutree(fit1,6)
mem<-as.matrix(group)
View(mem)
fina1<-cbind(mem,wines)
View(aggregate(fina1[,-1],by=list(mem),FUN=mean))

k_3<-kmeans(norm_clus,4)
str(k_3)
wines$cluster<-as.matrix(k_3$cluster)

aggregate(wines[,c(2:15)],by=list(wines$cluster),mean)
twss<-NULL
for(i in 2:15){
  twss<-c(twss,kmeans(wines,i)$tot.withinss)
}
twss
plot(2:15,twss,type = "o")

