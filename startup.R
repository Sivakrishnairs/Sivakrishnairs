library(readr)
start<-read.csv(file.choose())
str(start)
# normalization function
normalize<-function(x){
  temp<-(x-min(x))/(max(x)-min(x))
}
sart_m<-start[,-(4)]# removing the coloum "state"
start_n<-as.data.frame(lapply(start[,-(4)], normalize))#applying normlaze function by removeing column state

start_norm<-cbind(start_n, start$State)# combining column state 
attach(start_norm)
start_train<-start_norm[1:40,]# spliting train data
start_test<-start_norm[41:50,]# spliting test data

library(neuralnet)
library(nnet)
colnames(start_norm)
# building model
model<-neuralnet(formula = Profit~R.D.Spend+Administration+Marketing.Spend,data = start_train)
str(model)
plot(model)# ploting the network
model2<-compute(model,start_train[,-(4)])
model2
pred_prof<-model2$net.result
cor(pred_prof,start_test$Profit)
model3<-neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend,data = start_train, hidden = 5)
plot(model3)
pred_prof1<-model3$net.result
model4<-compute(model3,start_train)
