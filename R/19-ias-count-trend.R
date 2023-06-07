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

## Package
library(tidyverse) # for data manipulation
library(readxl)
library(magrittr) # for data manipulation
library(hrbrthemes) # for nice visualization
library(ggpubr) # to use ggarrange() function
library(pals)
library(rstatix)

## Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

## Data
tweet_ias_counting <- read_csv("data/tweet-04_finalized.csv")

# Annual summary --------------------------------------------------------------

# The number of tweets in total
tweet_ias_counting%>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = "identity")

# The number of tweets of each IAS
tweet_ias_counting %>% 
  group_by(name_ja, year) %>% 
  summarise(count_year = n()) %>% 
  ggplot(aes(x = year, y = count_year)) +
  geom_line(aes(colour = name_ja), 
            linewidth = 1.5,
            show.legend = FALSE) +
  theme_ipsum(base_family = "HiraKakuPro-W3")
