#-----------------------------------------------------------------------------#
# Script Name: 19-tweet-ias-count.R                                           #
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

# Packages
pacman::p_load(tidyverse,
               readxl,
               lubridate
               )

# Data
tweet_finalized <- read_csv("data/tweet-05_finalized.csv")
dat_ias_ja <- read_csv("data/basic-ias-info.csv")

# Count tweets-----------------------------------------------------------------

# Count occurrence of IAS
tweet_count_total <- tweet_finalized %>% 
  group_by(name_sp) %>% 
  summarise(count = n())

tweet_count_annual <- tweet_finalized %>% 
  group_by(name_sp, year) %>% 
  summarise(count = n())

# Re-add the IAS information
glimpse(dat_ias_ja)
tweet_count_total <- tweet_count_total %>% 
  right_join(dat_ias_ja, by = "name_sp") %>% 
  dplyr::select(-c(code_ias, KATAKANA, katakana)) %>% 
  dplyr::select(name_ja, name_sp, group_biol, taxon, reg1, reg2, 
                count) %>% 
  distinct(.keep_all = TRUE)
tweet_count_total$count[is.na(tweet_count_total$count)] <- 0

tweet_count_annual <- tweet_count_annual %>% 
  right_join(dat_ias_ja, by = "name_sp") %>% 
  dplyr::select(-c(code_ias, KATAKANA, katakana)) %>% 
  dplyr::select(name_ja, name_sp, year, group_biol, taxon, reg1, reg2, 
                count) %>% 
  distinct(.keep_all = TRUE)
tweet_count_annual$count[is.na(tweet_count_annual$count)] <- 0

# Visualize--------------------------------------------------------------------

# The number of tweets in total
tweet_count_annual %>% 
  group_by(year) %>% 
  summarise(n = sum(count)) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = "identity")

# The number of tweets of each IAS
tweet_count_annual %>% 
  ggplot(aes(x = year, y = count)) +
  geom_line(aes(colour = name_ja), 
            linewidth = 1.0,
            show.legend = FALSE) +
  theme_ipsum(base_family = "HiraKakuPro-W3")

# Write data-------------------------------------------------------------------
write_csv(tweet_count_total, "data/ias-count_total.csv") # For main analysis
write_csv(tweet_count_annual, "data/ias-count_annual.csv")
