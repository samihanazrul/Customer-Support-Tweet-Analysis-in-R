
install.packages("jsonlite")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("stringr")
install.packages("tidytext")
install.packages("textdata")
install.packages("sentimentr")
install.packages("plotly")
install.packages("htmlwidgets")

library(jsonlite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidytext)
library(textdata)
library(sentimentr)
library(htmlwidgets)
library(plotly)
library(lexicon)

tweet_df <- read.csv("cs_data2")

# Create custom data frame
custom_tweet_df <- data.frame( id = tweet_df$tweet_id,
                               created_at = tweet_df$created_at,
                               text = tweet_df$text,
                               author_id = tweet_df$author_id,
                               response_tweet_id = tweet_df$response_tweet_id,
                               in_response_to_tweet_id = tweet_df$in_response_to_tweet_id,
                               inbound = tweet_df$inbound
)

#Reformatting created_at field
clean_tweets_df <- custom_tweet_df %>% 
  mutate(created_at = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S +0000 %Y"))

# Remove URLs from the text column
clean_tweets_df <- clean_tweets_df %>%
  mutate(text = gsub("http[s]?://\\S+\\s*", "", text))

# Remove remaining leading or trailing whitespaces
clean_tweets_df$text <- trimws(clean_tweets_df$text) 

#removing emojis
clean_tweets_df <- clean_tweets_df %>% 
  mutate(text = gsub("[\\x{1F600}-\\x{1F64F}]|[\\x{1F300}-\\x{1F5FF}]|[\\x{1F680}-\\x{1F6FF}]|[\\x{2600}-\\x{26FF}]|[\\x{2700}-\\x{27BF}]", "", text, perl = TRUE))

# Create a combined list of profanity words
profanity_words <- unique(tolower(c(lexicon::profanity_alvarez, lexicon::profanity_banned, lexicon::profanity_arr_bad)))

# Remove profanity from the text column
for (word in profanity_words) {
  clean_tweets_df$text <- gsub(word, "", clean_tweets_df$text, fixed = TRUE)
}

# task 4: Add columns for year, month, day, hour, date, and weekday
clean_tweets_df <- clean_tweets_df %>%
  mutate(year = year(created_at),
         month = month(created_at, label = TRUE),
         day = day(created_at),
         hour = hour(created_at),
         date = as.Date(created_at),
         weekday = weekdays(created_at))

# Create dataframe containing non-stop words
words_in_tweets_df <- clean_tweets_df %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  filter(!word %in% c("amp", "ist"))

# Task 6.a: Find and visualize top 20 unique words
words_in_tweets_df %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  labs(x = "Count", y = "Words",
       title = "Top 20 Unique Words in Tweets") +
  coord_flip()

words_in_tweets_df

# Save plot as .png and .pdf files
ggsave("top 20 unique words.png", plot = words_in_tweets_df, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)
ggsave("top 20 unique words.pdf", plot = words_in_tweets_df, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)

# Task 6.b: Create a new data frame by filtering out all non-ASCII rows
non_ascii_df <- words_in_tweets_df %>%
  filter(str_detect(word, "[ -~]+"))

# Task 6.c: Rerun the code from Step 6.a on the newly created data frame
top_words_non_ascii_plot <- non_ascii_df %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(x = "Words", y = "Count",
       title = "Top 20 Unique Words in Tweets (Non-ASCII)")
top_words_non_ascii_plot

# Save plot as .png and .pdf files
ggsave("top_words_non_ascii_plot.png", plot = top_words_non_ascii_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)
ggsave("top_words_non_ascii_plot.pdf", plot = top_words_non_ascii_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)


# Task 7.a: Top 20 unique words for inbound tweets
# Filter the dataset to include only rows where 'inbound' is "True"
inbound_words_df <- clean_tweets_df %>%
  filter(inbound == "True") %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  filter(!word %in% c("amp", "ist", "it’s", "i’m", "115858"))

# Create plot for inbound tweets
inbound_words_plot <- inbound_words_df %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(x = "Words", y = "Count",
       title = "Top 20 Unique Words in Inbound Tweets")

# Save plot as .png and .pdf files
ggsave("inbound_words_plot.png", plot = inbound_words_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)
ggsave("inbound_words_plot.pdf", plot = inbound_words_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)

# Display the plot
print(inbound_words_plot)


# Task 7.b: Top 20 unique words for outbound tweets
outbound_words_df <- clean_tweets_df %>%
  filter(inbound == "False") %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  filter(!word %in% c("amp", "ist"))

# Create plot for outbound tweets
outbound_words_plot <- outbound_words_df %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "maroon") +
  coord_flip() +
  labs(x = "Words", y = "Count",
       title = "Top 20 Unique Words in Outbound Tweets")

# Save plot as .png and .pdf files
ggsave("outbound_words_plot.png", plot = outbound_words_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)
ggsave("outbound_words_plot.pdf", plot = outbound_words_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)

# Display the plot
print(outbound_words_plot)

# Task 8.a: AFINN Sentiment analysis to find top 15 positive and negative words

afinn_word_counts <- words_in_tweets_df %>% 
  inner_join(get_sentiments("afinn")) %>%
  mutate(sentiment = if_else(value>0, 'positive', 'negative')) %>% 
  count(word, value, sentiment, sort = T) %>% 
  ungroup()

afinn_word_plot<-afinn_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n, fill = sentiment))+
  geom_col(show.legend = F)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(x ="Sentiment", y ="Words",
       title = "Afinn Sentiment Analysis on Tweets")+
  coord_flip()

# Save plot as .png and .pdf files
ggsave("afinn_word_plot.png", plot = afinn_word_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)
ggsave("afinn_word_plot.pdf", plot = afinn_word_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)


# Task 8.b: AFINN Sentiment analysis to find top 10 positive and negative words in the inbound tweets

afinn_inbound_counts <- inbound_words_df %>% 
  inner_join(get_sentiments("afinn")) %>%
  mutate(sentiment = if_else(value>0, 'positive', 'negative')) %>% 
  count(word, value, sentiment, sort = T) %>% 
  ungroup()

afinn_inbound_plot<-afinn_inbound_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n, fill = sentiment))+
  geom_col(show.legend = F)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(x ="Sentiment", y ="Words",
       title = "Afinn Sentiment Analysis on Inbound Tweets")+
  coord_flip()

# Save plot as .png and .pdf files
ggsave("afinn_inbound_plot.png", plot = afinn_inbound_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)
ggsave("afinn_inbound_plot.pdf", plot = afinn_inbound_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)


# Task 8.c: AFINN Sentiment analysis to find top 10 positive and negative words in the outbound tweets

afinn_outbound_counts <- outbound_words_df %>% 
  inner_join(get_sentiments("afinn")) %>%
  mutate(sentiment = if_else(value>0, 'positive', 'negative')) %>% 
  count(word, value, sentiment, sort = T) %>% 
  ungroup()

afinn_outbound_plot<-afinn_outbound_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n, fill = sentiment))+
  geom_col(show.legend = F)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(x ="Sentiment", y ="Words",
       title = "Afinn Sentiment Analysis on Outbound Tweets")+
  coord_flip()

# Save plot as .png and .pdf files
ggsave("afinn_outbound_plot.png", plot = afinn_outbound_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)
ggsave("afinn_outbound_plot.pdf", plot = afinn_outbound_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)


# Task 9.a: Bing Sentiment analysis to find top 15 positive and negative words
get_sentiments(("bing"))
bing_word_counts <- words_in_tweets_df %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  ungroup()

bing_word_plot<-bing_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n, fill = sentiment))+
  geom_col(show.legend = F)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(x ="Sentiment", y ="Words",
       title = "Bing Sentiment Analysis on Tweets")+
  coord_flip()

# Save plot as .png and .pdf files
ggsave("bing_word_plot.png", plot = bing_word_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)
ggsave("bing_word_plot.pdf", plot = bing_word_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)

# Task 9.b: Bing Sentiment analysis to find top 10 positive and negative words in the inbound tweets
bing_inbound_word_counts <- inbound_words_df %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  ungroup()

Bing_inbound_plot<-bing_inbound_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n, fill = sentiment))+
  geom_col(show.legend = F)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(x ="Sentiment", y ="Words",
       title = "Bing Sentiment Analysis on Inbound Tweets")+
  coord_flip()

# Save plot as .png and .pdf files
ggsave("Bing_inbound_plot.png", plot = Bing_inbound_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)
ggsave("Bing_inbound_plot.pdf", plot = Bing_inbound_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)


# Task 9.c: Bing Sentiment analysis to find top 10 positive and negative words in the outbound tweets
bing_outbound_word_counts <- outbound_words_df %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  ungroup()

bing_outbound_plot<-bing_outbound_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n, fill = sentiment))+
  geom_col(show.legend = F)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(x ="Sentiment", y ="Words",
       title = "Bing Sentiment Analysis on Outbound Tweets")+
  coord_flip()

# Save plot as .png and .pdf files
ggsave("bing_outbound_plot.png", plot = bing_outbound_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)
ggsave("bing_outbound_plot.pdf", plot = bing_outbound_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)


# Task 10.a: NRC Sentiment analysis to find top 10 positive and negative words

get_sentiments("nrc")
nrc_word_counts <- words_in_tweets_df %>%
  inner_join(get_sentiments(("nrc"))) %>%
  count(word, sentiment, sort = T) %>%
  ungroup()

nrc_word_count_plot<-nrc_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n, fill = sentiment))+
  geom_col(show.legend = F)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(x ="Sentiment", y ="Words",
       title = "NRC Sentiment Analysis on Inbound Tweets")+
  coord_flip()

# Save plot as .png and .pdf files
ggsave("nrc_word_count_plot.png", plot = nrc_word_count_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)
ggsave("nrc_word_count_plot.pdf", plot = nrc_word_count_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)


# Task 10.b: NRC Sentiment analysis to find top 10 positive and negative words in the inbound tweets
nrc_inbound_word_counts <- inbound_words_df %>%
  inner_join(get_sentiments(("nrc"))) %>%
  count(word, sentiment, sort = T) %>%
  ungroup()

nrc_inbound_plot<-nrc_inbound_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n, fill = sentiment))+
  geom_col(show.legend = F)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(x ="Sentiment", y ="Words",
       title = "NRC Sentiment Analysis on Tweets")+
  coord_flip()

# Save plot as .png and .pdf files
ggsave("nrc_inbound_plot.png", plot = nrc_inbound_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)
ggsave("nrc_inbound_plot.pdf", plot = nrc_inbound_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)


# Task 10.c: NRC Sentiment analysis to find top 10 positive and negative words in the outbound tweets
nrc_outbound_word_counts <- outbound_words_df %>%
  inner_join(get_sentiments(("nrc"))) %>%
  count(word, sentiment, sort = T) %>%
  ungroup()

nrc_outbound_plot<-nrc_outbound_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n, fill = sentiment))+
  geom_col(show.legend = F)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(x ="Sentiment", y ="Words",
       title = "NRC Sentiment Analysis on Outbound Tweets")+
  coord_flip()

# Save plot as .png and .pdf files
ggsave("nrc_outbound_plot.png", plot = nrc_outbound_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)
ggsave("nrc_outbound_plot.pdf", plot = nrc_outbound_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)


# Task 11: Number of tweets by day of week
day_of_week_plot <- ggplot(clean_tweets_df, aes(x = weekday))+
  geom_bar(aes(fill =  ..count..))+
  theme(legend.position = "none")+
  xlab("Day of the Week")+
  ylab("Number of Tweets")+
  scale_fill_gradient(high = "lightpink" , low = "lightpink4")
day_of_week_plot

# Save plot as .png and .pdf files
ggsave("day_of_week_plot.png", plot = day_of_week_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)
ggsave("day_of_week_plot.pdf", plot = day_of_week_plot, width = 2048 / 150, height= 1502/ 150, units = "in", dpi = 150)


# Convert ggplot to plotly object
plotly_plot <- ggplotly(day_of_week_plot)

# Save the HTML file
saveWidget(as_widget(plotly_plot), "day_of_week_plot.html")
