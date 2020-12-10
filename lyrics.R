library(tidytext)
library(geniusr)
library(tidyverse)
library(wesanderson) # color palettes

songs <- tibble(song_id = c(4573553, 4271785, 4186971, 4271772, 3001600, 3319759, 3038886, 4266880, 
                            4105029, 3143254, 5013279, 4271789, 3143259, 4186978, 3143252, 4271788, 
                            3237820, 3041924, 5798642, 4271787, 5173169, 2403346, 3143257, 2800298, 
                            6151003, 3138202, 4213796, 3484874, 3256574, 4271764, 3844633), 
                year = c(2017, 2019, 2019, 2019, 2017, 2018, 2017, 2019, 2018, 2017, 2019, 2019, 
                         2017, 2019, 2017, 2020, 2019, 2017, 2020, 2019, 2020, 2016, 2018, 2016, 
                         2020, 2017, 2019, 2019, 2019, 2019, 2018))

billie <- get_artist_songs_df("615550") %>%
  inner_join(songs) %>%
  select(song_name, song_lyrics_url, year)

############################################################################################################

# get lyrics for every song
billie <- billie %>%
  rowwise() %>%
  mutate(lyrics = paste(get_lyrics_url(song_lyrics_url)$line, collapse = ' ')) %>%
  select(-song_lyrics_url)

lovely <- tibble(song_name = "lovely", year = 2018,
                 lyrics = paste(get_lyrics_search("Billie Eilish & Khalid", "lovely")$line, collapse = ' '))
billie <- rbind(billie, lovely)

# perform sentiment analysis on song lyrics
get_sentim <- function(x) {
  text_df <- tibble(text = x)
  words <- text_df %>% 
    unnest_tokens(word, text)
  polarity <- words %>%
    inner_join(get_sentiments("afinn"))
  
  if (sum(polarity$value) > 0) {
    sentiment <- "positive"
  } else if (sum(polarity$value) < 0) {
    sentiment <- "negative"
  } else {
    sentiment <- "neutral"
  }
  tibble(sentiment = sentiment, polarity = sum(polarity$value))
}

billie <- billie %>%
  rowwise() %>%
  mutate(sentiment = get_sentim(lyrics)$sentiment, polarity = get_sentim(lyrics)$polarity)

############################################################################################################

# negative songs
billie %>%
  filter(sentiment == "negative") %>%
  arrange(polarity) %>%
  select(song_name, year, polarity)

# positive songs
billie %>%
  filter(sentiment == "positive") %>%
  arrange(desc(polarity)) %>%
  select(song_name, year, polarity)

# negative vs positive words used most
text_df <- tibble(line = 1:nrow(billie), text = billie$lyrics)
words <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

sent.labs <- c("Negative", "Positive")
names(sent.labs) <- c("negative", "positive")

top10_negpos <- words %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y",
             labeller = labeller(sentiment = sent.labs)) +
  labs(title = "Top 10 Negative and Positive Words",
       x = "Contribution to Sentiment",
       y = NULL) +
  scale_fill_manual(values = wes_palette(n = 2, name = "GrandBudapest2")) +
  theme_bw()

ggsave("images/top10_negpos.png", plot = top10_negpos)


### wordclouds

devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)

words.df <- words %>% 
  select(-sentiment)
wordcloud2(words.df, size = 1.5, 
           color = wes_palette(n = 20, name = "GrandBudapest2", type = "continuous"),
           minRotation = -pi/2, maxRotation = -pi/2)

# comparison wordcloud positive vs. negative words
words.df2 <- words %>%
  mutate(color = ifelse(sentiment == "positive", "#de8bb7", "#bac0f5")) %>%
  select(-sentiment)
wordcloud2(words.df2, size = 1.5, color = words.df2$color,
           minRotation = -pi/2, maxRotation = -pi/2)

billie <- billie %>% 
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative", "neutral")))

num_songs_sent <- ggplot(billie, aes(x = year, fill = sentiment)) +
  geom_bar() +
  scale_fill_manual(values = wes_palette(n = 3, name = "GrandBudapest2"),
                    labels = c("Positive", "Negative", "Neutral")) +
  labs(title = "Number of Positive/Negative/Neutral Songs", 
       x = "Year", y = "Count", fill = NULL) +
  theme_bw()

prop_songs_sent <- ggplot(billie, aes(x = year, fill = sentiment)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = wes_palette(n = 3, name = "GrandBudapest2"),
                    labels = c("Positive", "Negative", "Neutral")) +
  labs(title = "Proportion of Positive/Negative/Neutral Songs", 
       x = "Year", y = "Proportion", fill = NULL) +
  theme_bw()

polarity_songs <- ggplot(billie, aes(x = year, y = polarity, color = sentiment)) +
  geom_point(size = 5, shape = 18) +
  scale_color_manual(values = wes_palette(n = 3, name = "GrandBudapest2"),
                     labels = c("Positive", "Negative", "Neutral")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Polarity of Every Billie Eilish Song", x = "Year", y = "Polarity", color = NULL) +
  theme_bw()

ggsave("images/num_songs_sent.png", plot = num_songs_sent)
ggsave("images/prop_songs_sent.png", plot = prop_songs_sent)
ggsave("images/polarity_songs.png", plot = polarity_songs)

############################################################################################################

#like/love filtered out | my boy + when the party's over
get_sentim_filtered <- function(x) {
  text_df <- tibble(text = x)
  words <- text_df %>% 
    unnest_tokens(word, text)
  polarity <- words %>%
    inner_join(get_sentiments("afinn")) %>%
    filter(!(word %in% c("like", "love")))
  
  if (sum(polarity$value) > 0) {
    sentiment <- "positive"
  } else if (sum(polarity$value) < 0) {
    sentiment <- "negative"
  } else {
    sentiment <- "neutral"
  }
  tibble(sentiment = sentiment, polarity = sum(polarity$value))
}

billie2 <- billie
# my boy
billie2[18, "sentiment"] <- get_sentim_filtered(billie2[18,]$lyrics)$sentiment
billie2[18, "polarity"] <- get_sentim_filtered(billie2[18,]$lyrics)$polarity
# when the party's over
billie2[28, "sentiment"] <- get_sentim_filtered(billie2[28,]$lyrics)$sentiment
billie2[28, "polarity"] <- get_sentim_filtered(billie2[28,]$lyrics)$polarity

# redo polarity plot
polarity_songs2 <- ggplot(billie2, aes(x = year, y = polarity, color = sentiment)) +
  geom_point(size = 5, shape = 18) +
  scale_color_manual(values = wes_palette(n = 3, name = "GrandBudapest2"),
                     labels = c("Positive", "Negative", "Neutral")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Polarity of Every Billie Eilish Song", x = "Year", y = "Polarity", color = NULL) +
  theme_bw()

ggsave("images/polarity_songs2.png", plot = polarity_songs2)

# average polarity scores
billie2 %>%
  group_by(sentiment) %>%
  summarize(avg_score = mean(polarity))

############################################################################################################

# Billboard Year-End number-one POP singles, 2000-2019
billboard_topsongs <- tibble(song = c("Breathe", "Hanging by a Moment", "How You Remind Me", "In Da Club", "Yeah!", 
                                      "We Belong Together", "Bad Day", "Irreplaceable", "Low", "Boom Boom Pow", "Tik Tok",
                                      "Rolling in the Deep", "Somebody That I Used to Know", "Thrift Shop", "Happy",
                                      "Uptown Funk", "Love Yourself", "Shape of You", "God's Plan", "Old Town Road"), 
                             artist = c("Faith Hill", "Lifehouse", "Nickelback", "50 Cent", "Usher", "Mariah Carey",
                                        "Daniel Powter", "Beyonce", "Flo Rida", "The Black Eyed Peas", "Kesha",
                                        "Adele", "Gotye", "Macklemore & Ryan Lewis", "Pharrell Williams",
                                        "Mark Ronson", "Justin Bieber", "Ed Sheeran", "Drake", "Lil Nas X"),
                             year = 2000:2019)

billboard_topsongs <- billboard_topsongs %>%
  rowwise() %>%
  mutate(lyrics = paste(get_lyrics_search(artist, song)$line, collapse = ' '),
         sentiment = get_sentim(lyrics)$sentiment, polarity = get_sentim(lyrics)$polarity)

billboard_topsongs <- billboard_topsongs %>% 
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative")))

## plots
billboard_polarity <- ggplot(billboard_topsongs, aes(x = year, y = polarity, fill = sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = wes_palette(n = 2, name = "GrandBudapest2"),
                    labels = c("Positive", "Negative")) +
  labs(title = "Polarity of Every Billboard Year-End Number One Pop Single", 
       x = "Year", y = "Polarity", fill = NULL) +
  theme_bw()

ggsave("images/billboard_polarity.png", plot = billboard_polarity)

billboard_topsongs %>%
  group_by(sentiment) %>%
  summarize(overall_polarity = sum(polarity))
