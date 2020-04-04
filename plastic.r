library(readr)
plastic<-read.csv(file.choose())
plot(plastic$Sales,type="o")
x<-data.frame(outer(rep(month.abb,length=60), month.abb,"==")+0)
colnames(x)<-month.abb
trakdata<-cbind(plastic,x)
trakdata["t"]<-1:60
trakdata["t_sq"]<-trakdata["t"]*trakdata["t"]
attach(trakdata)
trakdata["log_sales"]<-log(trakdata["Sales"])
train<-trakdata[1:48,]
test<-trakdata[48:60,]
#linear model
linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model, interval = "predict",newdata = test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear
#expoential model
expo_model<-lm(log_sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo
#quadratic model
Quad_model<-lm(Sales~t+t_sq,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad
#Additive seasonality linear model
Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear
#Additive seasonality quadratic model
Add_sea_Quad_model<-lm(Sales~t+t_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad
#multiple additive seasonality
multi_add_sea_model<-lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_Add_sea_Linear","rmse_Add_sea_Quad","rmse_multi_add_sea"),c(rmse_linear, rmse_expo, rmse_Quad, rmse_Add_sea_Linear, rmse_Add_sea_Quad, rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

newmodel<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trakdata)
resid <- residuals(newmodel)
resid[1:10]
windows()
acf(resid,lag.max = 12)

sheet<-write.csv(row.names = month.abb,length=11)

library(readxl)
new_data<-read_excel(file.choose(),1)
view(new_data)
pred_new<-data.frame(predict(newmodel,newdata=new_data,interval="predict"))
View(pred_new)
#Applying ARIMA
newmodel<-lm(Sales~t+t_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trakdata)
resid <- residuals(newmodel)
resid[1:10]
windows()
acf(resid,lag.max = 12)
k<-arima(resid,order=c(1,0,0))
str(k)
summary(k)
View(data.frame(res=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 12)
predires<-predict(arima(resid,order=c(1,0,0)),n.ahead=12)
str(predires)
predires$pred
write.csv(trakdata,file="trakdata.csv",col.names = F, row.names = F)
getwd()
pred_new$fit<-pred_new$fit+predires$pred
