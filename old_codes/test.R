dataset <- read.csv("dataset_final.csv",stringsAsFactors = FALSE)
nepali_words_filtered <- dataset[3,] %>%
  unnest_tokens(word, lyrics) %>%
  distinct(word)

View(nepali_words_filtered)



nepali_nrc<- nepali_words_filtered %>%  
  inner_join(get_sentiments("nrc"))

nrc_plot <- prince_nrc %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count))
