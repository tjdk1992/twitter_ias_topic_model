#-----------------------------------------------------------------------------#
# Script Name: 04-tweet-filter.R                                              #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script screens tweets for LDA by their attributes and cleanse tweet. #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Package
pacman::p_load(tidyverse, # for data manipulation
               magrittr,  # to overwrite data
               lubridate, # to handle date class
               stringi,   # to handle strings
               zipangu,
               RMeCab
               )

# Data
tweet_filtering <- read_csv("data/tweet-01_binded.csv")
dat_ias_ja <- read_csv("data/basic-ias-info.csv")

# Prepare raw data ------------------------------------------------------------

# Data structure
str(tweet_filtering)

# Merge IAS species information
tweet_filtering %<>% 
  mutate(code_ias = str_replace_all(MyFile, 
                                    "data-raw/doc-tweet-ias/doc-tweet-", 
                                    "")) %>% 
  left_join(dat_ias_ja %>% 
              dplyr::select(code_ias,
                            name_ja,
                            name_sp), 
            by = "code_ias")
# Check the result of merging
filter(tweet_filtering, is.na(name_sp)) # OK

# Select necessary columns
tweet_filtering %<>% 
  transmute(id_raw,
            tweet_id = id, 
            author_id, 
            type,
            lang,
            code_ias,
            name_ja,
            name_sp,
            date = date(created_at), 
            year = year(date), 
            text)

# Screen the tweet ------------------------------------------------------------

# Tweetの確認
tweet_filtering 

# 侵入はgeneralな侵入（例：子猫が家に侵入）に使われすぎるので含めない
tweet_filtering %<>% 
  filter(str_detect(text, "外来|移入|帰化"))

# Remove duplicate within individual species
tweet_filtering %<>% 
  mutate(rm_dup_sp = str_c(tweet_id, "_", name_sp)) %>% 
  distinct(rm_dup_sp, .keep_all = TRUE) %>% 
  dplyr::select(-rm_dup_sp) 

# Screen by language
## 言語の集計
table(tweet_filtering$lang)
## Remove tweets written in non-Japanese language
tweet_filtering %<>% dplyr::select(-lang) 

# Screen by the Type
## Check the type
table(tweet_filtering$type, useNA = "always")

## Replace NA
tweet_filtering %<>% mutate(type = replace_na(type, "normal"))

## Remove retweeted tweet
tweet_filtering %<>% filter(type != "retweeted") 

#-----------------------------------------------------------------------------#
# 1つのuser accountから多数の同一tweetがされている場合は宣伝であったりbotである
# 可能性が高い。この場合、その複数のtweetに起因する過大評価を避けるために、
# その一番最初のtweet（年単位）のみを解析に含めることにする。
#-----------------------------------------------------------------------------#

# userとtextの集計
tweet_filtering %>% 
  mutate(tweet_rm = str_c(author_id, "_", name_sp, "_", text)) %>% 
  group_by(tweet_rm) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(20)

# 重複を削除
tweet_filtering %<>% 
  mutate(tweet_rm = str_c(author_id, "_", name_sp, "_", text)) %>% 
  group_by(tweet_rm, year) %>%
  mutate(id_rm = row_number()) %>% 
  ungroup() %>% 
  filter(id_rm == 1) %>% 
  dplyr::select(-id_rm) 

# Export data
tweet_filtering %>% 
  mutate(id_filtered = row_number()) %>% 
  write_csv("data/tweet-02_filtered.csv")
