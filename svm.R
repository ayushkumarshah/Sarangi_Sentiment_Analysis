library(e1071)


dataset<-read.csv("datasets/lyrics_dataset_test.csv")
dim(dataset)
str(dataset)
#View(summary(dataset))
nrow(dataset)

#separate train data and test data
set.seed(1000)
train<-sample(1:150,108,replace=FALSE)
traindata<-dataset[train,]
testdata<-dataset[-train,]


#train data according to C_SVM model
fit_default_radial<-svm(sentiment~.,data=traindata)
fit_default_poly<-svm(sentiment~.,data=traindata,kernel="polynomial")
fit_default_linear<-svm(sentiment~.,data=traindata,kernel="linear")
fit_default_sigmoid<-svm(sentiment~.,data=traindata,kernel="sigmoid")

fit_default_radial
summary(fit_default_radial)
summary(fit_default_poly)
summary(fit_default_linear)
summary(fit_default_sigmoid)

#tune svm model for better accuracy
obj<-tune.svm(sentiment~.,data=traindata,gamma=seq(0.01,1,by=0.25),cost=seq(1,50,10))
summary(obj)
#new tuned model 
fit_new_radial<-svm(sentiment~.,data=traindata,cost=1,gamma=0.01,cross=10)
summary(fit_new_radial)

#prediction on test data
pred_new_radial<-predict(fit_new_radial,testdata)
pred_default_radial<-predict(fit_default_radial,testdata)
pred_default_linear<-predict(fit_default_linear,testdata)
pred_default_poly<-predict(fit_default_poly,testdata)
pred_default_sigmoid<-predict(fit_default_sigmoid,testdata)

View(testdata)
testdata1<-testdata
testdata2<-testdata
testdata3<-testdata
testdata4<-testdata
testdata5<-testdata
testdata6<-testdata
testdata7<-testdata

View(pred_new_radial)
testdata1$sentiment<-pred_new_radial
View(testdata1)

View(pred_default_radial)
testdata2$sentiment<-pred_default_radial
View(testdata2)

View(pred_default_linear)
testdata3$sentiment<-pred_default_linear
View(testdata3) 

View(pred_default_poly)
testdata4$sentiment<-pred_default_poly

View(testdata4) 

View(pred_default_sigmoid)
testdata5$sentiment<-pred_default_sigmoid
View(testdata5) 

#comparing accuracy
table_new_radial<-table(pred_new_radial,testdata$sentiment)
"accuracy"
sum(diag(table_new_radial))/sum(table_new_radial)*100

table_default_radial<-table(pred_default_radial,testdata$sentiment)
"accuracy"
sum(diag(table_default_radial))/sum(table_default_radial)*100

table_default_linear<-table(pred_default_linear,testdata$sentiment)
"accuracy"
sum(diag(table_default_linear))/sum(table_default_linear)*100

table_default_poly<-table(pred_default_poly,testdata$sentiment)
"accuracy"
sum(diag(table_default_poly))/sum(table_default_poly)*100

table_default_sigmoid<-table(pred_default_sigmoid,testdata$sentiment)
"accuracy"
sum(diag(table_default_sigmoid))/sum(table_default_sigmoid)*100

#train data according to ni-SVM model
fit_default_sigmoid_nu<-svm(sentiment~.,data=traindata,type="nu-classification",kernel="sigmoid",nu=0.05)
fit_default_poly_nu<-svm(sentiment~.,data=traindata,type="nu-classification",kernel="polynomial",nu=0.05)
fit_default_radial_nu<-svm(sentiment~.,data=traindata,type="nu-classification",kernel="radial",nu=0.05)

fit_default_linear_nu<-svm(sentiment~.,data=traindata,type="nu-classification",kernel="linear",nu=0.05)


#summary(fit_default_radial_nu)
summary(fit_default_poly_nu)
#summary(fit_default_linear_nu)
summary(fit_default_sigmoid_nu)


#prediction on test data (svm-nu)
pred_default_radial_nu<-predict(fit_default_radial_nu,testdata)
pred_default_linear_nu<-predict(fit_default_linear_nu,testdata)
pred_default_poly_nu<-predict(fit_default_poly_nu,testdata)
pred_default_sigmoid_nu<-predict(fit_default_sigmoid_nu,testdata)

View(pred_default_poly_nu)
testdata6$sentiment<-pred_default_poly_nu
View(testdata6)

View(pred_default_sigmoid_nu)
testdata7$sentiment<-pred_default_sigmoid_nu
View(testdata7)

View(pred_default_radial_nu)
testdata6$sentiment<-pred_default_poly_nu
View(testdata6)

View(pred_default_linear_nu)
testdata7$sentiment<-pred_default_sigmoid_nu
View(testdata7)

#comparing accuracy


table_default_poly_nu<-table(pred_default_poly_nu,testdata$sentiment)
"accuracy"
sum(diag(table_default_poly_nu))/sum(table_default_poly_nu)*100

table_default_sigmoid_nu<-table(pred_default_sigmoid_nu,testdata$sentiment)
"accuracy"
sum(diag(table_default_sigmoid_nu))/sum(table_default_sigmoid_nu)*100

table_default_radial_nu<-table(pred_default_radial_nu,testdata$sentiment)
"accuracy"
sum(diag(table_default_radial_nu))/sum(table_default_radial_nu)*100

table_default_linear_nu<-table(pred_default_linear_nu,testdata$sentiment)
"accuracy"
sum(diag(table_default_linear_nu))/sum(table_default_linear_nu)*100
# library(kernlab)
# #train data according to C-bsvc model
# fit_default_linear_cbsvc<-ksvm(sentiment~.,data=traindata,type="C-bsvc",kernel="vanilladot",cros=10)
# print(fit_default_linear_cbsvc)
# 
# fit_default_poly_cbsvc<-ksvm(sentiment~.,data=traindata,type="C-bsvc",kernel="polydot",cros=10)
# print(fit_default_poly_cbsvc)
# 
# fit_default_radial_cbsvc<-ksvm(sentiment~.,data=traindata,type="C-bsvc",kernel="rbfdot",cros=10)
# print(fit_default_radial_cbsvc)
# 
# fit_default_hyptan_cbsvc<-ksvm(sentiment~.,data=traindata,type="C-bsvc",kernel="tanhdot",cros=10)
# print(fit_default_hyptan_cbsvc)

