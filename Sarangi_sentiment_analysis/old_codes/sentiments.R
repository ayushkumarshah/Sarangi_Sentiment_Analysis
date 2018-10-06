dataset <- read.csv("sentiments.csv",stringsAsFactors = FALSE)

nrc<- dataset %>%  
  left_join(get_sentiments("nrc"))

View(nrc)

write.csv(nrc, file = "nepali_words_final.csv")


