#-----------------------------------------------------------------------------#
# Script Name: 04-tweet-screen.R                                              #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script screens tweets for LDA by their attributes and cleanse tweet. #
#-----------------------------------------------------------------------------#

###############################################################################
# 注意: Rの有効桁数は16程度なので、諸々のidは十進数かcharacter型で扱う！
###############################################################################

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Package
pacman::p_load(tidyverse, # for data manipulation
               magrittr,  # to overwrite data
               lubridate, # to handle date class
               stringi    # to handle strings
               )

# Data
tweet_screening <- read_csv("data/tweet-01_binded.csv")
dat_ias_ja <- read_csv("data/basic-ias-info.csv")

# Prepare raw data ------------------------------------------------------------

# Data structure
str(tweet_screening)

# Merge IAS species information
tweet_screening %<>% 
  mutate(code_ias = str_replace_all(MyFile, 
                                    "data-raw/doc-tweet-ias/doc-tweet-", 
                                    "")) %>% 
  left_join(dat_ias_ja %>% 
              dplyr::select(code_ias,
                            name_ja,
                            name_sp), 
            by = "code_ias")
# Check the result of merging
filter(tweet_screening, is.na(name_sp)) # OK

# Select necessary columns
tweet_screening %<>% 
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
tweet_screening # n = 1,721,394

# Remove duplicate within individual species
tweet_screening %<>% 
  mutate(rm_dup_sp = str_c(tweet_id, "_", name_sp)) %>% 
  distinct(rm_dup_sp, .keep_all = TRUE) %>% 
  dplyr::select(-rm_dup_sp) # n = 1,650,296

# Screen by searched query
# 侵入だけを含むものは除外（目視確認すると関係ないのが多い）
tweet_screening %<>% 
  filter(str_detect(text, "外来") |
           str_detect(text, "移入") |
           str_detect(text, "帰化")) # n = 1,253,094

# Screen by language
## 言語の集計
table(tweet_screening$lang)
## Remove tweets written in non-Japanese language
tweet_screening %<>% dplyr::select(-lang) # n = 1,253,093

# Screen by the Type
## Check the type
table(tweet_screening$type, useNA = "always")

## Replace NA
tweet_screening %<>% mutate(type = replace_na(type, "normal"))

## Remove retweeted tweet
tweet_screening %<>% filter(type != "retweeted") # n = 364,418

#-----------------------------------------------------------------------------#
# 1つのuser accountから多数の同一tweetがされている場合は宣伝であったりbotである
# 可能性が高い。この場合、その複数のtweetに起因する過大評価を避けるために、
# その一番最初のtweet（年単位）のみを解析に含めることにする。
#-----------------------------------------------------------------------------#

# userとtextの集計
tweet_screening %>% 
  mutate(tweet_rm = str_c(author_id, "_", name_sp, "_", text)) %>% 
  group_by(tweet_rm) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(20)

# Histogram
tweet_screening %>% 
  mutate(tweet_rm = str_c(author_id, "_", name_sp, "_", text)) %>% 
  group_by(tweet_rm, year) %>% 
  summarise(n = n()) %>% 
  filter(n > 5) %>% 
  arrange(desc(n)) %>% 
  ggplot() +
  geom_histogram(aes(x = log(n))) # 少しだけあるかな…

# 重複を削除
tweet_screening %<>% 
  mutate(tweet_rm = str_c(author_id, "_", name_sp, "_", text)) %>% 
  group_by(tweet_rm, year) %>%
  mutate(id_rm = row_number()) %>% 
  ungroup() %>% 
  filter(id_rm == 1) %>% 
  dplyr::select(-id_rm) # n = 299,601

# Export data
tweet_screening %>% 
  mutate(id_orig = row_number()) %>% 
  write_csv("data/tweet-02_screened.csv")
