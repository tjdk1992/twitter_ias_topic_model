#-----------------------------------------------------------------------------#
# Script Name: 07-tweet-tokenize.R                                            #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script tokenizes tweet text into unified form of tokens.             #
#-----------------------------------------------------------------------------#


# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Packages
pacman::p_load(tidyverse, # for data manipulation
               magrittr,  # to overwrite data
               RMeCab)    # for tokenization

# Data
tweet_tokenizing <- read_csv("data/tweet-04_cleansed.csv")

# Tokenization-----------------------------------------------------------------

# Convert tibble into data frame (RMeCab only handle data frame)
tweet_tokenizing %<>% 
  dplyr::select(id_cleansed, text) %>% 
  as.data.frame()

# Tokenize text by RMeCab
list_token_ias <- RMeCabDF(tweet_tokenizing, "text", 1)

# Convert list of token into data frame
token_ias <- purrr::pmap_df(list(nv = list_token_ias,
                                 title = tweet_tokenizing$id),
                            function(nv, title){
                              tibble(id_cleansed = title,
                                     term = nv,
                                     hinshi = names(nv))
                            })

# 形態素解析から得られたトークンの書き出し
write_csv(token_ias, "data/tokens-01_original.csv")
