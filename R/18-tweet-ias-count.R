#-----------------------------------------------------------------------------#
# Script Name: 18-tweet-ias-count.R                                           #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script calculates the occurrence of each IAS in the tweets.          #
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
tweet_finalized <- read_csv("data/tweet-04_finalized.csv")
dat_ias_ja <- read_csv("data/basic-ias-info.csv")

#------------------------------------------------------------------------------

# Count tweets

## Count occurrence of IAS
tweet_count_total <- tweet_finalized %>% 
  group_by(name_sp) %>% 
  summarise(count = n())

tweet_count_annual <- tweet_finalized %>% 
  group_by(name_sp, year) %>% 
  summarise(count = n()) %>% 
  group_by(name_sp) %>% 
  summarise(count = mean(count))

## Re-add the IAS information
glimpse(dat_ias_ja)
tweet_count_total <- tweet_count_total %>% 
  right_join(dat_ias_ja, by = "name_sp") %>% 
  dplyr::select(-c(code_ias, KATAKANA, katakana)) %>% 
  dplyr::select(name_ja, name_sp, count, group_biol, taxon, reg1, reg2) %>% 
  distinct(.keep_all = TRUE)
tweet_count_total$count[is.na(tweet_count_total$count)] <- 0

tweet_count_annual <- tweet_count_annual %>% 
  right_join(dat_ias_ja, by = "name_sp") %>% 
  dplyr::select(-c(code_ias, KATAKANA, katakana)) %>% 
  dplyr::select(name_ja, name_sp, count, group_biol, taxon, reg1, reg2) %>% 
  distinct(.keep_all = TRUE)
tweet_count_annual$count[is.na(tweet_count_annual$count)] <- 0

## Write data
write_csv(tweet_count_total, "data/ias-count_total.csv")
write_csv(tweet_count_annual, "data/ias-count_annual.csv")
