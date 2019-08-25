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

# for each sentiment, compute an odds ratio and a confidence interval
library(broom)
log_or <- sentiment_counts %>%
  mutate(log_or = log((Android / (sum(Android) - Android)) / 
                        (iPhone / (sum(iPhone) - iPhone))),
         se = sqrt(1/Android + 1/(sum(Android) - Android) + 
                     1/iPhone + 1/(sum(iPhone) - iPhone)),
         conf.low = log_or - qnorm(0.975)*se,
         conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

# A graphical visualization shows some sentiments that are clearly overrepresented
log_or %>%
  mutate(sentiment = reorder(sentiment, log_or)) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip()
log_or

ggsave('figs/confidence-intervals.png')

# visualization of specific words that are driving these differences
android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave('figs/faceted_sentiments.png')
