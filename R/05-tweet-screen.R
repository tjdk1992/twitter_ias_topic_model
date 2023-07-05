#-----------------------------------------------------------------------------#
# Script Name: 04-tweet-screen.R                                              #
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
               zipangu    # to handle japanese characters
               )

# Data
tweet_screening <- read_csv("data/tweet-02_filtered.csv")
dat_ias_ja <- read_csv("data/basic-ias-info.csv")

# 検索ワード-------------------------------------------------------------------

# 外来生物名
ias_ja <- dat_ias_ja %>% 
  mutate(n_chr = nchar(KATAKANA)) %>% 
  arrange(desc(n_chr)) %>% 
  pull(KATAKANA)

term_extract_A <- c(str_c("外来", ias_ja),
                    str_c("移入", ias_ja),
                    str_c("帰化", ias_ja))

# 外来種を指す単語
taxon_ja <- c("種", "生物", "動物", "植物", "動植物",
              "哺乳", "鳥", "爬虫", "両生", "魚", "昆虫")

term_extract_B <- c(str_c("外来", taxon_ja),
                    str_c("移入", taxon_ja),
                    str_c("帰化", taxon_ja))

term_extract <- c(term_extract_A, term_extract_B)

tweet_searched <- data.frame()
for (i in 1:length(term_extract)) {
  term_search <- term_extract[i]
  t_tweet_searched <- filter(tweet_screening, 
                             str_detect(text, term_search))
  tweet_searched <- rbind(tweet_searched, t_tweet_searched)
}

# Remove duplicate
tweet_searched %<>% 
  distinct(id_filtered, .keep_all = TRUE)

# チェック
tweet_searched %>% 
  filter(str_detect(text, "在日")) %>% 
  pull(text)

# Export data
tweet_searched %>% 
  mutate(id_screened = row_number()) %>% 
  write_csv("data/tweet-03_screened.csv")
