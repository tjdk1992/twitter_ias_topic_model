#-----------------------------------------------------------------------------#
# Script Name: 19-lda-topic-trend.R                                           #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script analyzes IAS occurrence trends over time.                     #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Package
pacman::p_load(tidyverse, # for data manipulation
               readxl,
               magrittr, # for data manipulation
               hrbrthemes, # for nice visualization
               ggpubr, # to use ggarrange(, function
               pals,
               rstatix
               )

# Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

# Data
tweet_count_total <- read_csv("data/ias-count_total.csv")
tweet_count_annual <- read_csv("data/ias-count_annual.csv")

# Annual summary --------------------------------------------------------------

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
