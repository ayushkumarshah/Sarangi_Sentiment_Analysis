library(data.table) 
library(jsonlite)
library(purrr)
library(RecordLinkage) 
library(stringr)
library(tm)

#load data
traind <- fromJSON("datasets/traindata.json")
test <- fromJSON("datasets/testdata.json")

#convert json to data table
vars <- setdiff(names(traind),c("photos","features"))
train <- map_at(traind, vars, unlist) %>% as.data.table()
test <- map_at(test,vars,unlist) %>% as.data.table()

train <- train[,.(listing_id,features,title,artist,interest_level)]
test <- test[,.(listing_id,features,title,artist,interest_level)]

dim(train)
dim(test)
head(train)
head(test)
sapply(train,class)
sapply(test,class)


#join data
test[,interest_level := "None"]
tdata <- rbindlist(list(train,test))

#fill empty values in the list
tdata[,features := ifelse(map(features, is_empty),"empty",features)]

#count number of features per listing
tdata[,feature_count := unlist(lapply(features, length))]

#count number of words in description
tdata[,desc_word_count := str_count(features,pattern = "\\w+")]

#count total length of description
tdata[,desc_len := str_count(features)]

#similarity between address
#tdata[,lev_sim := levenshteinDist(street_address,display_address)]

dim(tdata)

#extract variables from features
fdata <- data.table(listing_id = rep(unlist(tdata$listing_id), lapply(tdata$features, length)), features = unlist(tdata$features))
head(fdata)

#convert features to lower
fdata[,features := unlist(lapply(features, tolower))]

#calculate count for every feature
fdata[,count := .N, features]
fdata[order(count)][1:20]

#keep features which occur 100 or more times
fdata <- fdata[count >= 100]

#convert columns into table`<br/>
fdata <- dcast(data = fdata, formula = listing_id ~ features, fun.aggregate = length, value.var = "features")

#create a corpus of descriptions
text_corpus <- Corpus(VectorSource(tdata$features))
#check first 4 documents
inspect(text_corpus[1:4])

#the corpus is a list object in R of type CORPUS
print(lapply(text_corpus[1:2], as.character))


#let's clean the data
dropword <- "br"

#remove br
#text_corpus <- tm_map(text_corpus,removeWords,dropword)
#print(as.character(text_corpus[[1]]))

#tolower
#text_corpus <- tm_map(text_corpus, tolower)
#print(as.character(text_corpus[[1]]))

#remove punctuation
##text_corpus <- tm_map(text_corpus, removePunctuation)
#print(as.character(text_corpus[[1]]))

#remove number
text_corpus <- tm_map(text_corpus, removeNumbers)
print(as.character(text_corpus[[1]]))

#remove whitespaces
#text_corpus <- tm_map(text_corpus, stripWhitespace,lazy = T)
#print(as.character(text_corpus[[1]]))

#remove stopwords
#text_corpus <- tm_map(text_corpus, removeWords, c(stopwords('english')))
#print(as.character(text_corpus[[1]]))

#convert to text document
text_corpus <- tm_map(text_corpus, PlainTextDocument)

#perform stemming - this should always be performed after text doc conversion
text_corpus <- tm_map(text_corpus, stemDocument,language = "nepali")
print(as.character(text_corpus[[1]]))
text_corpus[[1]]$content
#After every cleaning step, we've printed the resultant corpus to help you understand the effect of each step on the corpus. Now, our corpus is ready to get converted into a matrix.

#convert to document term matrix
docterm_corpus <- DocumentTermMatrix(text_corpus)
dim(docterm_corpus)
dim(fdata)