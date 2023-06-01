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

## Packages
library(readxl)
library(tidyverse)
library(lubridate)

## Data
tweet_tidy <- read_csv("data/basic-ias-tweet.csv")
dat_ias_ja <- read_csv("data/basic-ias-info.csv")

#------------------------------------------------------------------------------

# Count tweets

## Count occurrence of IAS
tweet_count <- tweet_tidy %>% 
  group_by(name_sp) %>% 
  summarise(count = n())

## Re-add the IAS information
glimpse(dat_ias_ja)
tweet_count <- tweet_count %>% 
  right_join(dat_ias_ja, by = "name_sp") %>% 
  dplyr::select(-c(code_ias, KATAKANA, katakana)) %>% 
  dplyr::select(name_ja, name_sp, count, group_biol, taxon, reg1, reg2) %>% 
  distinct(.keep_all = TRUE)
tweet_count$count[is.na(tweet_count$count)] <- 0

## Write data
write_csv(tweet_count, "data/basic-ias-count.csv")
