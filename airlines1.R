library(readxl)
airlines<-read_excel(file.choose())
attach(airlines)
summary(airlines)
normal<-function(x){
  temp<-(x-min(x))/(max(x)-min(x))
  return(temp)
}
f1<-normal(airlines$Balance)

f2<-normal(airlines$Qual_miles)
f3<-normal(airlines$cc1_miles)
f4<-normal(airlines$cc3_miles)
f5<-normal(airlines$Bonus_miles)
f6<-normal(airlines$Bonus_trans)
f7<-normal(airlines$Flight_miles_12mo)
f8<-normal(airlines$Flight_trans_12)
f9<-normal(airlines$Days_since_enroll)
f10<-normal((airlines$cc2_miles))
names(f1)<c()
final1<-cbind(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,airlines$`Award?`)
names(final1)<-c("balance","Qual_miles","cc1_miles")

names(final1)<-c("balance","Qual_miles","cc1_miles","cc3_miles","Bonus_miles","Bonus_trans","Flight_miles_12mo","Flight_trans_12","Days_since_enroll","cc3_miles","`Award?`")
View(final1)

k_3<-kmeans(final1,4)
str(k_3)
airlines$cluster<-as.matrix(k_3$cluster)

aggregate(airlines[,c(2:13)],by=list(airlines$cluster),mean)
twss<-NULL
for(i in 2:15){
  twss<-c(twss,kmeans(airlines,i)$tot.withinss)
}
twss
plot(2:15,twss,type = "o")
