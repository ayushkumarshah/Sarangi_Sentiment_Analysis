source("loadlibrary.R")
prince <- read.csv("datasets/lyrics_dataset.csv", stringsAsFactors = FALSE)

names(prince)

View(prince)


prince_words_filtered <- prince %>%
  unnest_tokens(word, lyrics) %>%
  distinct()

class(prince_words_filtered)

dim(prince_words_filtered)

full_word_count <- prince %>%
  unnest_tokens(word, lyrics) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words)) 



my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")
prince_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in Nepali lyrics") +
  coord_flip()

prince_words_counts <- prince_words_filtered %>%
  count(word, sort = TRUE) 



wordcloud2(prince_words_counts[1:300, ], size = 1)

prince_word_lengths <- prince %>%
  unnest_tokens(word, lyrics) %>%
  distinct() %>%
  mutate(word_length = nchar(word)) 

prince_word_lengths %>%
  count(word_length, sort = TRUE) %>%
  ggplot(aes(word_length), 
         binwidth = 10) + 
  geom_histogram(aes(fill = ..count..),
                 breaks = seq(1,25, by = 2), 
                 show.legend = FALSE) + 
  xlab("Word Length") + 
  ylab("Word Count") +
  ggtitle("Word Length Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())


wc <- prince_word_lengths %>%
  ungroup() %>%
  select(word, word_length) %>%
  distinct() %>%
  arrange(desc(word_length))

wordcloud2(wc[1:300, ], 
           size = 1,
           minSize = .0005,
           ellipticity = .3, 
           rotateRatio = 1, 
           fontWeight = "bold")







