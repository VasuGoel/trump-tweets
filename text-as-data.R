library(tidyverse)
library(lubridate)
library(scales)
library(dslabs)
library(tidytext)

# pattern that starts with @, # or neither, and is followed by any combination of letters or digits
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# unnest_tokens function with the regex option to appropriately extract the hashtags and mentions along with words and remove links to pictures
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern)

# remove stop words (in text mining) using tidytext provided database of stop words
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word )

# generate final table using remaining data wrangling techniques
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

# save tweet_words object to .rda file
save(tweet_words, file = 'rdas/tweet_words.rda')

# compute the odds ratio
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))

# save android_iphone_or object to .rda file
save(android_iphone_or, file = 'rdas/android_iphone_or.rda')

# highest odds ratio for Andoid
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

# tops for iPhone
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

# filter based on the total frequency
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)
