library(e1071)
dataset<-read.csv("datasets/lyrics_dataset_test2.csv")
dim(dataset)
str(dataset)
#View(summary(dataset))
nrow(dataset)

test <- read.csv("nepali_test.csv", stringsAsFactors = FALSE)
test <- rbind(test,dataset)

test <- test[1:107,]


#separate train data and test data
set.seed(1000)
train<-sample(1:150,108,replace=FALSE)
traindata<-dataset[train,]
testdata<-dataset[-train,]

#train data according to C_SVM model

#tune svm model for better accuracy
obj<-tune.svm(sentiment~.,data=traindata,gamma=seq(0.01,1,by=0.25),cost=seq(1,50,10))
summary(obj)

#new tuned model 
fit_new_radial<-svm(sentiment~.,data=traindata,cost=1,gamma=0.01,cross=10)
summary(fit_new_radial)

#prediction on test data
pred_new_radial<-predict(fit_new_radial,testdata)

View(testdata)
testdata_radial<-testdata

View(pred_new_radial)
testdata_radial$sentiment<-pred_new_radial
View(testdata_radial)

#comparing accuracy
table_new_radial<-table(pred_new_radial,testdata$sentiment)
"accuracy"
sum(diag(table_new_radial))/sum(table_new_radial)*100

#train data according to ni-SVM model
fit_default_sigmoid_nu<-svm(sentiment~.,data=traindata,type="nu-classification",kernel="sigmoid",nu=0.05)
fit_default_linear_nu<-svm(sentiment~.,data=traindata,type="nu-classification",kernel="linear",nu=0.05)

summary(fit_default_linear_nu)
summary(fit_default_sigmoid_nu)


#prediction on test data (svm-nu)

pred_default_linear_nu<-predict(fit_default_linear_nu,testdata)
pred_default_sigmoid_nu<-predict(fit_default_sigmoid_nu,testdata)

testdata_linear<-testdata
testdata_sigmoid<-testdata

testdata_linear$sentiment<-pred_default_linear_nu
View(testdata_linear)

testdata_sigmoid$sentiment<-pred_default_sigmoid_nu
View(testdata_sigmoid)

#comparing accuracy

table_default_linear_nu<-table(pred_default_linear_nu,testdata$sentiment)
"accuracy"
sum(diag(table_default_linear_nu))/sum(table_default_linear_nu)*100

table_default_sigmoid_nu<-table(pred_default_sigmoid_nu,testdata$sentiment)
"accuracy"
sum(diag(table_default_sigmoid_nu))/sum(table_default_sigmoid_nu)*100


