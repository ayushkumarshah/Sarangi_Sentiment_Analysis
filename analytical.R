library(e1071)
dataset<-read.csv("datasets/lyrics_dataset_test.csv")
View(dataset)
dim(dataset)
str(dataset)
#View(summary(dataset))
nrow(dataset)
source("multipot.R")

#separate train data and test data
set.seed(1000)
train<-sample(1:150,100,replace=FALSE)
traindata<-dataset[train,]
testdata<-dataset[sample(1:150,80,replace=FALSE),]

#train data according to C_SVM model

#tune svm model for better accuracy
#obj<-tune.svm(sentiment~.,data=traindata,gamma=seq(0.01,1,by=0.25),cost=seq(1,50,10))
#summary(obj)

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


Recall(testdata[, 4], pred_default_linear_nu)
Precision(testdata[, 4], pred_default_linear_nu)
F1_Score(testdata[, 4], pred_default_linear_nu)
Accuracy(testdata[, 4], pred_default_linear_nu)


Recall(testdata[, 4], pred_default_sigmoid_nu)
Precision(testdata[, 4], pred_default_sigmoid_nu)
F1_Score(testdata[, 4], pred_default_sigmoid_nu)
Accuracy(testdata[, 4], pred_default_sigmoid_nu)


Recall(testdata[, 4],pred_new_radial)
Precision(testdata[, 4], pred_new_radial)
F1_Score(testdata[, 4], pred_new_radial)
Accuracy(testdata[, 4], pred_new_radial)

newdata1 <- testdata[ which(testdata$sentiment=='देश्भक्ती' ),]  
View(newdata1)
pred_default_linear_nudb<-predict(fit_default_linear_nu,newdata1)
pred_default_sigmoid_nudb<-predict(fit_default_sigmoid_nu,newdata1)
pred_new_radialdb<-predict(fit_new_radial,newdata1)
newdata2 <- testdata[ which(testdata$sentiment=='हर्ष' ),]  
pred_default_linear_nuh<-predict(fit_default_linear_nu,newdata2)
pred_default_sigmoid_nuh<-predict(fit_default_sigmoid_nu,newdata2)
pred_new_radialh<-predict(fit_new_radial,newdata2)
newdata3 <- testdata[ which(testdata$sentiment=='दुःखी' ),]  
pred_default_linear_nud<-predict(fit_default_linear_nu,newdata3)
pred_default_sigmoid_nud<-predict(fit_default_sigmoid_nu,newdata3)
pred_new_radiald<-predict(fit_new_radial,newdata3)
newdata4<- testdata[ which(testdata$sentiment=='प्रेम' ),]  
pred_default_sigmoid_nup<-predict(fit_default_sigmoid_nu,newdata4)
pred_new_radialp<-predict(fit_new_radial,newdata4)

emotions <- c("देश्भक्ती","हर्ष","दुःखी","प्रेम")

r_linear <- c(Recall(newdata1[, 4],pred_default_linear_nudb),
              Recall(newdata2[, 4],pred_default_linear_nuh),
              Recall(newdata3[, 4],pred_default_linear_nud),
              Recall(newdata4[, 4],pred_default_linear_nup))
r_sigmoid <- c(Recall(newdata1[, 4],pred_default_sigmoid_nudb),
               Recall(newdata2[, 4],pred_default_sigmoid_nuh),
               Recall(newdata3[, 4],pred_default_sigmoid_nud),
               Recall(newdata4[, 4],pred_default_sigmoid_nup))
r_radial <- c(Recall(newdata1[, 4],pred_new_radialdb),
              Recall(newdata2[, 4],pred_new_radialh),
              Recall(newdata3[, 4],pred_new_radiald),
              Recall(newdata4[, 4],pred_new_radialp))
g.recall <- melt(data.frame(emotions,r_linear,r_sigmoid,r_radial), id='emotions')
names(g.recall) <- c('x', 'func', 'value')

p_linear <- c(Precision(newdata1[, 4],pred_default_linear_nudb),
              Precision(newdata2[, 4],pred_default_linear_nuh),
              Precision(newdata3[, 4],pred_default_linear_nud),
              Precision(newdata4[, 4],pred_default_linear_nup))
p_sigmoid <- c(Precision(newdata1[, 4],pred_default_sigmoid_nudb),
               Precision(newdata2[, 4],pred_default_sigmoid_nuh),
               Precision(newdata3[, 4],pred_default_sigmoid_nud),
               Precision(newdata4[, 4],pred_default_sigmoid_nup))
p_radial <- c(Precision(newdata1[, 4],pred_new_radialdb),
              Precision(newdata2[, 4],pred_new_radialh),
              Precision(newdata3[, 4],pred_new_radiald),
              Precision(newdata4[, 4],pred_new_radialp))
g.precision <- melt(data.frame(emotions,p_linear,p_sigmoid,p_radial), id='emotions')
names(g.precision) <- c('x', 'func', 'value')


f_linear <- c(F1_Score(newdata1[, 4],pred_default_linear_nudb),
              F1_Score(newdata2[, 4],pred_default_linear_nuh),
              F1_Score(newdata3[, 4],pred_default_linear_nud),
              F1_Score(newdata4[, 4],pred_default_linear_nup))
f_sigmoid <- c(F1_Score(newdata1[, 4],pred_default_sigmoid_nudb),
               F1_Score(newdata2[, 4],pred_default_sigmoid_nuh),
               F1_Score(newdata3[, 4],pred_default_sigmoid_nud),
               F1_Score(newdata4[, 4],pred_default_sigmoid_nup))
f_radial <- c(F1_Score(newdata1[, 4],pred_new_radialdb),
              F1_Score(newdata2[, 4],pred_new_radialh),
              F1_Score(newdata3[, 4],pred_new_radiald),
              F1_Score(newdata4[, 4],pred_new_radialp))
g.f1 <- melt(data.frame(emotions,f_linear,f_sigmoid,f_radial), id='emotions')
names(g.f1) <- c('x', 'func', 'value')


a_linear <- c(Accuracy(newdata3[, 4],pred_default_linear_nud),
              Accuracy(newdata1[, 4],pred_default_linear_nudb),
              Accuracy(newdata4[, 4],pred_default_linear_nup),
              Accuracy(newdata2[, 4],pred_default_linear_nuh))
a_sigmoid <- c(Accuracy(newdata3[, 4],pred_default_sigmoid_nud),
               Accuracy(newdata1[, 4],pred_default_sigmoid_nudb),
               Accuracy(newdata4[, 4],pred_default_sigmoid_nup),
               Accuracy(newdata2[, 4],pred_default_sigmoid_nuh))
a_radial <- c(Accuracy(newdata3[, 4],pred_new_radiald),
              Accuracy(newdata1[, 4],pred_new_radialdb),
              Accuracy(newdata4[, 4],pred_new_radialp),
              Accuracy(newdata2[, 4],pred_new_radialh))
g.accuracy.l <- melt(data.frame(emotions,a_linear), id='emotions')
g.accuracy.s <- melt(data.frame(emotions,a_sigmoid), id='emotions')
g.accuracy.r <- melt(data.frame(emotions,a_radial), id='emotions')

names(g.accuracy.l) <- c('x', 'func', 'value')
names(g.accuracy.s) <- c('x', 'func', 'value')
names(g.accuracy.r) <- c('x', 'func', 'value')





