library(tidyverse)
library(lubridate)
library(scales)
library(dslabs)

head(trump_tweets)

# tweets between the day Trump announced his campaign and election day
# use 'extract' to remove the 'Twitter for ' part of the source and filter out retweets
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

# save campaign_tweets object to .rda file
save(campaign_tweets, file = 'rdas/campaign_tweets.rda')

# Plot between '% of tweets' and 'Hour of day (EST)' for 2 source devices Android and iPhone
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)", y = "% of tweets", color = "")

ggsave('figs/line-plot.png')