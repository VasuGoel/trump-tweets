library(tidyverse)
library(lubridate)
library(scales)

# getting data from JSON API by http://www.trumptwitterarchive.com
url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, 
                                      orders = "a b! d! H!:M!:S! z!* Y!",
                                      tz="EST"))

# save raw trump_tweets fetched to a .csv file
write.csv(trump_tweets, 'data/trump_tweets.csv')

# save raw trump_tweets object to rdas/raw_trump_tweets.rda
save(trump_tweets, file = 'rdas/trump_tweets.rda')