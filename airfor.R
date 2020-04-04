library(readxl)
air<-read_excel(file.choose())
windows()
plot(air$Passengers,type="o")# plot to view
x<-data.frame(outer(rep(month.abb,length=96), month.abb,"==")+0)
colnames(x)<-month.abb
View(x)
data<-cbind(air,x)
data["t"]<-1:96
data["t_sq"]<-data["t"]*data["t"]
data["log_passenger"]<-log(data["Passengers"])
attach(data)
train<-data[1:84,]
test<-data[85:96,]
linear_model<-lm(Passengers~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model, interval = "predict",newdata = test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear
expo_model<-lm(log_passenger~t,data=train)
expo_pred<-data.frame(predict(expo_model, interval = "predict", newdata = test))
rmse_expo<-sqrt(mean((test$Passengers-expo_pred$fit)^2,na.rm = T))
rmse_expo
Quad_model<-lm(Passengers~t+t_sq, data = train)
Quad_model_pred<-data.frame(predict(Quad_model,interval = "predict",newdata=test))
rmse_model<-sqrt(mean((test$Passengers-Quad_model_pred$fit)^2,na.rm=T))
rmse_model

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear

Add_sea_Quad_model<-lm(Passengers~t+t_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad 

multi_add_sea_model<-lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_model","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_model,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

new_model <- lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = data)


resid <- residuals(new_model)
resid[1:10]
windows()
acf(resid,lag.max = 10)


k <- arima(resid, order=c(1,0,0))
str(k)

View(data.frame(res=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 15)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
acf(k$residuals)
write.csv(data,file="trakdata.csv",col.names = F,row.names = F)


library(readxl)
test_data<-read_excel(file.choose(),1)
View(test_data)
pred_new<-data.frame(predict(new_model,newdata=test_data,interval = 'predict'))
View(pred_new)
pred_new$fit <- pred_new$fit+pred_res$pred
View(pred_new)