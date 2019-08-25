library(tidyverse)
library(lubridate)
library(scales)
library(dslabs)
library(tidytext)

# using nrc lexicon from tidytext for sentiment analysis
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  select(word, sentiment)