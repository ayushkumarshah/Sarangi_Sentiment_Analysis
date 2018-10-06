dataset <- read.csv("nepali_words_final.csv",stringsAsFactors = FALSE)
View(dataset)
dataset <-distinct(dataset,words)
View(dataset)
#save the new dataset to .csv for use in later tutorials
write.csv(nepali_words_filtered, file = "nepali_words_joined.csv")