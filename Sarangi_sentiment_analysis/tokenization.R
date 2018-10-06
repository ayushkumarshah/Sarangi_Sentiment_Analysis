source("loadlibrary.R")
dataset<-read.csv("datasets/lyrics_dataset_test.csv")
 
suffixes=readLines('datasets/suffix.txt') 

stopwords = readLines('datasets/stopwords.txt')     #Your stop words file


dataset[26,] 

for (i in 1:length(suffixes)) {
  dataset$lyrics <- gsub(paste(suffixes[i],"\\b")," ", dataset$lyrics)
}
# (dataset$lyrics)

write.csv(dataset,file="datasets/changed_dataset.csv")

for (i in 1:length(stopwords)) {
  dataset$lyrics <- gsub(stopwords[i],"", dataset$lyrics)
}
write.csv(dataset,file="datasets/changed_dataset.csv")

# (dataset$lyrics)


dataset[26,] 
# View(dataset)
