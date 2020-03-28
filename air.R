library(readxl)
air<-read_excel(file.choose())
#customized normalization function
normalize<-function(x){
  temp<-(x-min(x))/(max(x)-min(x))
  return(temp)
}
# normalizing the data using lapply fuction removing the 1 and 12 coloumn
air_norm<-as.data.frame(lapply(air[,2:11],normalize))
# making the final data set to apply model
air_final<-cbind(air_norm,air$`Award?`)
k_3<-kmeans(air_final,4)
str(k_3)
#adding cluster column to main data set air
air$cluster<-as.matrix(k_3$cluster)
#applying aggregate function for clustring
aggregate(air[,c(2:13)],by=list(air$cluster),mean)
twss<-NULL
for(i in 2:15){
  twss<-c(twss,kmeans(air_norm,i)$tot.withinss)
}
twss
plot(2:15,twss,type = "o")
