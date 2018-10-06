lyrics_dataset <- read.csv("datasets/lyrics_dataset.csv", stringsAsFactors = FALSE, encoding="UTF-8")
getsentiments<-read.csv("datasets/sentiments_dataset_test.csv", stringsAsFactors = FALSE, encoding="UTF-8")

#View(lyrics_dataset)

# names(prince)
# 
# View(prince)
# 
# 


my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

undesirable_words <- c("prince", "chorus", "repeat", "lyrics", 
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "2", 
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")

theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.ticks = aticks, #Set axis ticks to on or off
        panel.grid.minor = pgminor, #Turn the minor grid lines on or off
        legend.title = lt, #Turn the legend title on or off
        legend.position = lp) #Turn the legend on or off
}

#Customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

# 
# class(prince_words_filtered)
# 
# dim(prince_words_filtered)
# 
full_word_count <- lyrics_dataset %>%
  unnest_tokens(word, lyrics) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

lyrics_filtered <- lyrics_dataset %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

lyrics_words_counts <- lyrics_filtered %>%
  count(word, sort = TRUE)


lyrics_word_lengths <- lyrics_dataset %>%
  unnest_tokens(word, lyrics) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  mutate(word_length = nchar(word)) 

lyrics_word_lengths %>%
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

wc <- lyrics_word_lengths %>%
  ungroup() %>%
  select(word, word_length) %>%
  distinct() %>%
  arrange(desc(word_length))

lyrics_tidy <- lyrics_dataset %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

lyrics_nrc <- lyrics_tidy %>%
  inner_join(getsentiments)

nrc_plot <- lyrics_nrc %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 1500)) + #Hard code the axis limit
  ggtitle("Nepali NRC Sentiment") +
  coord_flip()

img <- "www/neetesh123.jpg" #Load the background image
lab <- ""  #Turn off the label
#Overlay the plot on the image and create the meme file
meme(img, lab, "meme_nrc_nepali.jpg", inset = nrc_plot,width=500,height=500)
#Read the file back in and display it!
nrc_meme <- image_read("meme_nrc_nepali.jpg")

plot_words_1998 <- lyrics_nrc %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(seq_len(10)) %>%
  ungroup()
