dataset <- read.csv("lyrics_dataset.csv",stringsAsFactors = FALSE)
nepali_words_filtered <- dataset %>%
  unnest_tokens(words, lyrics) %>%
  distinct(words)
nepali_words<-nepali_words_filtered %>%
  select(words)
View(nepali_words)
#save the new dataset to .csv for use in later tutorials
write.csv(nepali_words, file = "nepali_words_final.csv")