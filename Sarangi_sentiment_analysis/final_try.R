source("loadlibrary.R")
dataset<-read.csv("datasets/lyrics_dataset_test.csv")
View(dataset)

test <- read.csv("nepali.csv", stringsAsFactors = FALSE)
test <- rbind(test,dataset)

View(test)
test <- test[1:150,]
View(test)
write.csv(test,file="datasets/predict.csv")
dataset<-read.csv("datasets/predict.csv")

dataset<-dataset %>% 
  select(lyrics, title,artist,sentiment)
View(dataset)

#separate train data and test data
set.seed(1000)
train<-sample(1:150,100,replace=FALSE)
traindata<-dataset[train,]
testdata <- dataset[1:80,]
# testdata<-dataset[sample(1:150,80,replace=FALSE),]
View(testdata)
#train data according to C_SVM model
#tune svm model for better accuracy
#obj<-tune.svm(sentiment~.,data=traindata,gamma=seq(0.01,1,by=0.25),cost=seq(1,50,10))
#summary(obj)

#new tuned model 
fit_new_radial<-svm(sentiment~.,data=traindata,cost=1,gamma=0.01,cross=10)
summary(fit_new_radial)

#prediction on test data
pred_new_radial<-predict(fit_new_radial,testdata)

# View(testdata)
testdata_radial<-testdata

# View(pred_new_radial)
testdata_radial$sentiment<-pred_new_radial
 View(testdata_radial)


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
# View(testdata_sigmoid)

table_default_linear_nu<-table(testdata$sentiment,pred_default_linear_nu)
"accuracy"
sum(diag(table_default_linear_nu))/sum(table_default_linear)*100
table_default_linear_nu

Recall(testdata[, 4], pred_default_linear_nu)
Precision(testdata[, 4], pred_default_linear_nu)
F1_Score(testdata[, 4], pred_default_linear_nu)
Accuracy(testdata[, 4], pred_default_linear_nu)


table_default_sigmoid_nu<-table(testdata$sentiment,pred_default_sigmoid_nu)
"accuracy"
sum(diag(table_default_sigmoid_nu))/sum(table_default_sigmoid_nu)*100
table_default_sigmoid_nu

Recall(testdata[, 4], pred_default_sigmoid_nu)
Precision(testdata[, 4], pred_default_sigmoid_nu)
F1_Score(testdata[, 4], pred_default_sigmoid_nu)
Accuracy(testdata[, 4], pred_default_sigmoid_nu)

table_new_radial<-table(testdata$sentiment,pred_new_radial)
"accuracy"
sum(diag(table_new_radial))/sum(table_new_radial)*100
table_new_radial

Recall(testdata[, 4],pred_new_radial)
Precision(testdata[, 4], pred_new_radial)
F1_Score(testdata[, 4], pred_new_radial)
Accuracy(testdata[, 4], pred_new_radial)


newdata1 <- testdata[ which(testdata$sentiment=='देश्भक्ती' ),]  
View(newdata1)
pred_default_linear_nudb<-predict(fit_default_linear_nu,newdata1)


Recall(newdata1[, 4],pred_default_linear_nudb)
Precision(newdata1[, 4], pred_default_linear_nudb)
F1_Score(newdata1[, 4], pred_default_linear_nudb)
Accuracy(newdata1[, 4], pred_default_linear_nudb)


pred_default_sigmoid_nudb<-predict(fit_default_sigmoid_nu,newdata1)

Recall(newdata1[, 4],pred_default_sigmoid_nudb)
Precision(newdata1[, 4], pred_default_sigmoid_nudb)
F1_Score(newdata1[, 4], pred_default_sigmoid_nudb)
Accuracy(newdata1[, 4], pred_default_sigmoid_nudb)

pred_new_radialdb<-predict(fit_new_radial,newdata1)

Recall(newdata1[, 4],pred_new_radialdb)
Precision(newdata1[, 4], pred_new_radialdb)
F1_Score(newdata1[, 4], pred_new_radialdb)
Accuracy(newdata1[, 4], pred_new_radialdb)

newdata2 <- testdata[ which(testdata$sentiment=='हर्ष' ),]  
View(newdata2)
pred_default_linear_nuh<-predict(fit_default_linear_nu,newdata2)


Recall(newdata2[, 4],pred_default_linear_nuh)
Precision(newdata2[, 4], pred_default_linear_nuh)
F1_Score(newdata2[, 4], pred_default_linear_nuh)
Accuracy(newdata2[, 4], pred_default_linear_nuh)


pred_default_sigmoid_nuh<-predict(fit_default_sigmoid_nu,newdata2)

Recall(newdata2[, 4],pred_default_sigmoid_nuh)
Precision(newdata2[, 4], pred_default_sigmoid_nuh)
F1_Score(newdata2[, 4], pred_default_sigmoid_nuh)
Accuracy(newdata2[, 4], pred_default_sigmoid_nuh)

pred_new_radialh<-predict(fit_new_radial,newdata2)

Recall(newdata2[, 4],pred_new_radialh)
Precision(newdata2[, 4], pred_new_radialh)
F1_Score(newdata2[, 4], pred_new_radialh)
Accuracy(newdata2[, 4], pred_new_radialh)



newdata3 <- testdata[ which(testdata$sentiment=='दुःखी' ),]  
View(newdata3)
pred_default_linear_nud<-predict(fit_default_linear_nu,newdata3)


Recall(newdata3[, 4],pred_default_linear_nud)
Precision(newdata3[, 4], pred_default_linear_nud)
F1_Score(newdata3[, 4], pred_default_linear_nud)
Accuracy(newdata3[, 4], pred_default_linear_nud)


pred_default_sigmoid_nud<-predict(fit_default_sigmoid_nu,newdata3)

Recall(newdata3[, 4],pred_default_sigmoid_nud)
Precision(newdata3[, 4], pred_default_sigmoid_nud)
F1_Score(newdata3[, 4], pred_default_sigmoid_nud)
Accuracy(newdata3[, 4], pred_default_sigmoid_nud)

pred_new_radiald<-predict(fit_new_radial,newdata3)

Recall(newdata3[, 4],pred_new_radiald)
Precision(newdata3[, 4], pred_new_radiald)
F1_Score(newdata3[, 4], pred_new_radiald)
Accuracy(newdata3[, 4], pred_new_radiald)

newdata4<- testdata[ which(testdata$sentiment=='प्रेम' ),]  
View(newdata4)
pred_default_linear_nup<-predict(fit_default_linear_nu,newdata4)


Recall(newdata4[, 4],pred_default_linear_nup)
Precision(newdata4[, 4], pred_default_linear_nup)
F1_Score(newdata4[, 4], pred_default_linear_nup)
Accuracy(newdata4[, 4], pred_default_linear_nup)


pred_default_sigmoid_nup<-predict(fit_default_sigmoid_nu,newdata4)

Recall(newdata4[, 4],pred_default_sigmoid_nup)
Precision(newdata4[, 4], pred_default_sigmoid_nup)
F1_Score(newdata4[, 4], pred_default_sigmoid_nup)
Accuracy(newdata4[, 4], pred_default_sigmoid_nup)

pred_new_radialp<-predict(fit_new_radial,newdata4)

Recall(newdata4[, 4],pred_new_radialp)
Precision(newdata4[, 4], pred_new_radialp)
F1_Score(newdata4[, 4], pred_new_radialp)
Accuracy(newdata4[, 4], pred_new_radialp)
