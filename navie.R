library(readr)
sms<-read.csv(file.choose(), stringsAsFactors = F)
str(sms)
class(sms)
sms$type<-factor(sms$type)
str(sms)
table(sms$type)
library(tm)
sms_corpus<-Corpus(VectorSource(sms$text))
corpus_clean<-tm_map(sms_corpus,tolower)
corpus_clean<-tm_map(corpus_clean, removeNumbers)
corpus_clean<-tm_map(corpus_clean, removeWords, stopwords())
corpus_clean<-tm_map(corpus_clean,removePunctuation)
removeNumpunt<-function(x)gsub("[^[:alpha:][:space:]]*","",x)
corpus_clean<-tm_map(corpus_clean,content_transformer(removeNumpunt))
class(corpus_clean)
sms_dtm<-DocumentTermMatrix(corpus_clean)
class(sms_dtm)
sms_train<-sms[1:4169,]
sms_test<-sms[4170:5559,]
sms_dm_train<-sms_dtm[1:4169,]
sms_dm_test<-sms_dtm[4170:5559,]
sms_corpus_train<-corpus_clean[1:4169]
sms_corpus_test<-corpus_clean[4170:5559]
prop.table(table(sms_train$type))
prop.table(table(sms_test$type))
sms_dic<-findFreqTerms(sms_dm_train,5)
sms_train1<-DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dic))
sms_test1<-DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dic))
sms_dic[1:10]
inspect(sms_corpus_train[1:6])
list(sms_dic[1:100])
conv_count<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x, levels = c(0,1), labels = c("No","Yes"))
}
sms_train1<-apply(sms_train1, MARGIN = 2,conv_count)
sms_test1<-apply(sms_test1, MARGIN = 2, conv_count)
install.packages("e1071")
library(e1071)
sms_classifier<-naiveBayes(sms_train1,sms_train$type)
sms_test_pred<-predict(sms_classifier, sms_test1)
library(gmodels)
CrossTable(sms_test_pred, sms_test$type, prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,dnn=c("predicted","actual"))
Accuracy<-mean(sms_test_pred==sms_test$type)
Accuracy
