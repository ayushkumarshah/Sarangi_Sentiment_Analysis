library(RTextTools)
library(e1071)

pos_tweets =  rbind(
  c('I love this car', 'positive'),
  c('This view is amazing', 'positive'),
  c('I feel great this morning', 'positive'),
  c('I am so excited about the concert', 'positive'),
  c('He is my best friend', 'positive'),
  c('I am happy today','positive'),
  c('Today is the best day of my life','positive')
)

neg_tweets = rbind(
  c('I do not like this car', 'negative'),
  c('This view is horrible', 'negative'),
  c('I feel tired this morning', 'negative'),
  c('I am not looking forward to the concert', 'negative'),
  c('He is my enemy', 'negative'),
    c('He is not a good person.','negative'),
    c('I dont like you','negative')
)

sad_tweets=rbind(c('im sad','sad'),
                 c('i want to cry','sad'), 
                 c('i am lonely','sad'),
                 c('im depressed','sad'), 
                 c('i want to die','sad'),
                 c('i have nowhere to go','sad'),
                 c('i miss my family','sad'))

test_tweets = rbind ( c('feel happy this morning', 'positive'),
  c('larry friend', 'positive'),
  c('not like that man', 'negative'),
  c('house not great', 'negative'),
  c('your song annoying', 'negative'),
  c('life is not good','sad'),
  c('i hate my life','sad')
  )

View(test_tweets)



tweets = rbind(pos_tweets, neg_tweets,sad_tweets, test_tweets)
View(tweets)

matrix= create_matrix(tweets[,1], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) 


# train the model
mat = as.matrix(matrix)
View(mat)
classifier = naiveBayes(mat[1:15,], as.factor(tweets[1:15,2]) )

# test the validity
predicted = predict(classifier, mat[18:25,])
predicted
na.omit(predicted)

table(tweets[18:25, 2], predicted)
recall_accuracy(tweets[21:25, 2], predicted)

# build the data to specify response variable, training set, testing set.
container = create_container(matrix, as.numeric(as.factor(tweets[,2])),
                             trainSize=1:10, testSize=11:15,virgin=FALSE)

models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))

results = classify_models(container, models)

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

