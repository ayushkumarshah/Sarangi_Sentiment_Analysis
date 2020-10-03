dataset <- read.csv("nepali_words.csv",stringsAsFactors = FALSE)


nepali_words_filtered <- dataset %>%
  unnest_tokens(words, words) %>%
  distinct()

View(nepali_words_filtered)
#save the new dataset to .csv for use in later tutorials
write.csv(nepali_words_filtered, file = "nepali_words_joined.csv")