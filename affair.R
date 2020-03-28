library(readr)
af<-read.csv(file.choose())
attach(af)
af$affairs<-replace(af$affairs, af$affairs>0, 1)
table(af$affairs)
af$children<-ifelse(af$children=='yes',1,0)
af$gender<-ifelse(af$gender=='yes',1,0)
model<-glm(affairs~.,data=af,family = 'binomial')
summary(model)
model2<-glm(affairs~age+yearsmarried+religiousness+rating+factor(children),data=af,family = 'binomial')
summary(model2)
predict(model2,af)
prob<-predict(model2, af ,type="response")
table(af$affairs)
confusion<-table(prob>0.6,af$affairs)
confusion
tp<-144
tn<-446
fn<-5
fp<-6
pression<-tp/(tp+fp)
rec<-tp/(tp+fn)
specificity=tn/(tn+fp)
f1<-2*(pression*rec)/(pression+rec)
print(c(pression,rec,specificity,f1))
library(ROCR)
rocrpred<-prediction(prob,af$affairs)
rocrpref<-performance(rocrpred,'tpr','fpr')
str(rocrpref)
plot(rocrpref)
library(pROC)
auc<-performance(rocrpred, measure = "auc")
auc<-auc@y.name[(1)]
auc













