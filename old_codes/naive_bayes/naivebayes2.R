library(RTextTools)
library(e1071)
library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)

dataset <- read.csv("lyrics_dataset_test.csv",stringsAsFactors = FALSE)
View(dataset)


data<-dataset[c(-2,-3)]

View(data)

matrix= create_matrix(data[,1], 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) 


corpus <- Corpus(VectorSource(data$lyrics))
# Inspect the corpus
corpus
dtm <- DocumentTermMatrix(corpus)

View(dtm)
dtm

a=as.matrix(dtm)
a
View(a)

classifier = naiveBayes(a[1:100,], as.factor(data[1:100,2]) )

# test the validity
predicted = predict(classifier, a[101:150,])
predicted
na.omit(predicted)

table(data[101:150, 2], predicted)
  recall_accuracy(tweets[21:25, 2], predicted)

# build the data to specify response variable, training set, testing set.
container = create_container(matrix, as.numeric(as.factor(tweets[,2])),
                             trainSize=1:10, testSize=11:15,virgin=FALSE)

models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))

results = classify_models(container, models)
results

# accuracy table
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])
table(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"])

# recall accuracy
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"FORESTS_LABEL"])

recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[11:15, 2])), results[,"SVM_LABEL"])

# model summary
analytics = create_analytics(container, results)
summary(analytics)
head(analytics@document_summary)
analytics@ensemble_summar



N=4
set.seed(2014)
cross_validate(container,N,"MAXENT")
cross_validate(container,N,"TREE")
cross_validate(container,N,"SVM")
cross_validate(container,N,"RF")

