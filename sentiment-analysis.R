library(tidyverse)
library(lubridate)
library(scales)
library(dslabs)
library(tidytext)

# using nrc lexicon from tidytext for sentiment analysis
nrc <- get_sentiments("nrc")

# count and compare the frequencies of each sentiment appearing in each device
sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

# for each sentiment, compute the odds of being in the device
sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))