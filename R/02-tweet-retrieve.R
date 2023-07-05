#-----------------------------------------------------------------------------#
# Script Name: 02-tweet-retrieve                                              #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script retrieve tweet data via twitter academic API v2.              #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Package
pacman::p_load(tidyverse,      # for data manipulation
               academictwitteR # for handling twitter
               )

# Data
dat_ias_ja <- read_csv("data/basic-ias-info.csv") # 探索する外来生物のリスト

# Retrieve tweets -------------------------------------------------------------

# Check the list of IAS
str(dat_ias_ja) # KATAKANAが検索対象のTermである。

# Prepare bearer token
bearer_token <- "<ADD MY BEARER TOKEN>" # This is a secret sequence.

# Prepare search query
ias_ja <- dat_ias_ja$KATAKANA
query_common <- "(移入 OR 帰化 OR 外来 OR 侵入)"
list_ias_tweet <- list()
for (i in 1:length(ias_ja)) {
  ias_all_ja <- unlist(dat_ias_ja$KATAKANA, use.name = FALSE)
  ias_all_code <- unlist(dat_ias_ja$code_ias, use.name = FALSE)
  name_ias <- ias_all_ja[i]
  code_ias <- ias_all_code[i]
  t_query <- str_c(query_common, " ", name_ias)
  t_path <- str_c("data-raw/doc-tweet-ias/doc-tweet-", code_ias)
  tweet_ias <- 
    get_all_tweets(query = t_query,
                   start_tweets = "2008-01-01T00:00:00Z",
                   end_tweets = "2023-05-01T00:00:00Z",
                   bearer_token = bearer_token,
                   data_path = t_path,
                   bind_tweets = TRUE,
                   n = 1000000) # 本番は10,000,000
  list_ias_tweet[[i]] <- tweet_ias
}

# Check the last 5 IAS in the looped query
tail(ias_ja) # ブラックバスで終わっているので最後まで取得できていると判断

## 以下のWarningが出ていた
# Warning message:
# Directory already exists. Existing JSON files may be parsed and returned, 
# choose a new path if this is not intended. 