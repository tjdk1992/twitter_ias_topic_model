#-----------------------------------------------------------------------------#
# Script Name: 02-tweet-retrieval.R
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script screening and cleanse tweet                                   #
#                                                                             #
#-----------------------------------------------------------------------------#


# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Packages
library(tidyverse)
library(magrittr)
library(lubridate)
library(stringi)

# Data
tweet_df_binded <- read_csv("data/tweet_01-binded.csv")
dat_ias_ja <- read_csv("data/basic-ias-info.csv")

###############################################################################
# 注意: Rの有効桁数は16程度なので、諸々のidは十進数かcharacter型で扱う！
###############################################################################

# Preparation -----------------------------------------------------------------

# Extract file name
tweet_df_binded %<>% 
  mutate(code_ias = str_replace_all(MyFile, 
                                    "data-raw/doc-tweet-ias/doc-tweet-",
                                    "")) %>% 
  dplyr::select(-MyFile)

# Dateを整理してYearを作成
tweet_df_binded %<>% mutate(date = date(created_at), 
                            year = year(created_at))

# Screen data -----------------------------------------------------------------

# Limit tweet type
## Count the type category
tweet_df_binded %>% 
  group_by(type) %>% 
  summarise(n = n())
## Check the all type
tweet_df_binded %>% 
  # check "quoted", "replied_to", "quoted-replied_to", and "retweeted"
  filter(type == "retweeted") %>% 
  dplyr::select(text)
## Check the retweeted contents
tweet_df_binded %>% 
  filter(type == "retweeted") %>% 
  dplyr::select(text) %>% 
  filter(!(str_detect(text, pattern = "^RT"))) %>% 
  head(10) %>% 
  unlist(use.name = FALSE)
## Remove tweeted tweet
tweet_df_binded %<>% 
  mutate(type = replace_na(type, "original")) %>% 
  filter(!(type == "retweeted"))

# Merge the ias information
## Merge
tweet_df_binded %<>% 
  left_join(dat_ias_ja %>% 
              dplyr::select(code_ias, name_ja, name_sp), 
            by = "code_ias") %>% 
  dplyr::select(-code_ias)
## Check whether the merging was correctly done
filter(tweet_df_binded, is.na(name_ja))

# Limit Language to Japanese
table(tweet_df_binded$lang)
tweet_df_binded <- tweet_df_binded %>% 
  filter(lang == "ja") %>% 
  dplyr::select(-lang)

# Check validity of the searched query
## Check "外来", "移入", "帰化", "侵入"
tweet_df_binded %>% 
  filter(str_detect(text, pattern = "侵入")) %>% 
  filter(!(str_detect(text, pattern = "外来") |
             str_detect(text, pattern = "移入") |
             str_detect(text, pattern = "帰化"))) %>% 
  sample_n(size = 1000) %>% 
  dplyr::select(name_ja, text) %>% 
  View()
## 侵入はほとんどgeneralな使われ方をしているので除外する。
## 例：「家に侵入してきた」
## 侵入とその他3つのqueryが同時に含まれる場合は除外しない。
tweet_df_binded %<>% 
  filter(str_detect(text, pattern = "外来") |
           str_detect(text, pattern = "移入") |
           str_detect(text, pattern = "帰化"))

# Remove duplicates within IAS species
## Check duplicated data
tweet_df_binded %>% 
  group_by(id, name_sp) %>% 
  summarise(n = n()) %>% 
  filter(n >= 2) %>% 
  arrange(desc(n))
## Remove duplicate (複数の名称で個別に検索をかけているので重複が生じうる)
## 例えば、キョンとタイワンキョンなど
tweet_df_binded %<>% distinct(.keep_all = TRUE)

# Add original id
tweet_df_binded %<>% mutate(id_orig = row_number())

#------------------------------------------------------------------------------

# Select variables for analysis

# Check the structure again
glimpse(tweet_df_binded)

# Extract necessary variables
tweet_tidy <- transmute(tweet_df_binded,
                        id_orig,
                        date, year,
                        ct_rt = retweet_count, 
                        ct_rep = reply_count, 
                        ct_qt = quote_count,
                        ct_imp = impression_count,
                        name_ja, name_sp,
                        text)

# Remove binded data frame
rm(tweet_df_binded)

# Write data
write_csv(tweet_tidy, "data/basic-ias-tweet.csv")
