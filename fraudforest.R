library(readr)
fraud<-read.csv(file.choose())
fraud_seg<-ifelse(fraud$Taxable.Income<=30000,"Risk","Good")
fraud1<-cbind(fraud,fraud_seg)
attach(fraud)
attach(fraud1)
fraud1$fraud_seg<-as.factor(fraud1$fraud_seg)
fraud1_round<-fraud1[order(runif(600)),]
fraud_train<-fraud1_round[1:500,]
fraud_test<-fraud1_round[501:600,]
prop.table(table(fraud1_round$fraud_seg))
prop.table(table(fraud_train$fraud_seg))
prop.table(table(fraud_test$fraud_seg))
install.packages("randomForest")
library(randomForest)
attach(fraud1)
fraud_forest<-randomForest(fraud_seg~.,data=fraud_train)
fraud_forest
importance(fraud_forest)
varImpPlot(fraud_forest)
plot(fraud_forest)
fraud_pred<-predict(fraud_forest,newdata = fraud_test, type = "class")
fraud_pred

library(caret)

CrossTable(table(fraud_pred,fraud_test$fraud_seg))
