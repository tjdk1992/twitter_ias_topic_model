#-----------------------------------------------------------------------------#
# Script Name: 18-lda-topic-trend.R                                           #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script analyzes temporal topic trends.                               #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Package
pacman::p_load(tidyverse,
               magrittr,
               pals,
               hrbrthemes)

# Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

# Data
theta <- read_csv("data/lda-output-03_doc-topic-tweet.csv")

#------------------------------------------------------------------------------

# Topic distribution in whole text data
theta %>% 
  pivot_longer(cols = TP01:TP25,
               names_to = "no_topic",
               values_to = "prob") %>% 
  group_by(no_topic) %>% 
  summarise(sum = sum(prob)) %>% 
  ggplot(aes(x = reorder(no_topic, as.numeric(no_topic)), y = sum)) +
  geom_bar(stat = "identity", fill = pal_orig[1]) +
  labs(x = "Topic", y = "Posterior probability") +
  theme_ipsum(axis_text_size = 12,
              axis_title_size = 12,
              axis_title_just = "center",
              base_family = "Helvetica")

# Time series trends ----------------------------------------------------------

## 年で集計する。
# 推定後そのまま解析続ける場合は以下でもOK
# topic_proportion_per_year <- aggregate(theta, 
#                                        by = list(year = tweet_finalizing$year), 
#                                        mean)
topic_proportion_per_year <- theta %>% 
  pivot_longer(cols = TP01:TP25,
               names_to = "no_topic",
               values_to = "prob") %>% 
  group_by(no_topic, year) %>% 
  summarise(prob = mean(prob)) %>% 
  ungroup()

# そのままのトピックでやる場合
topic_proportion_per_year %>% 
  ggplot(aes(x = year, y = prob, 
             group = no_topic, colour = as.factor(no_topic))) + 
  geom_line(linewidth = 1, show.legend = FALSE) + 
  scale_color_manual(values = as.vector(pal_orig)) + 
  facet_wrap(. ~ no_topic, ncol = 6, nrow = 5) +
  labs(x = "Year", y = "Probability") +
  theme_ipsum(base_family = "Helvetica", 
              base_size = 8, 
              axis_text_size = 8, 
              axis_title_size = 8, 
              axis_title_just = "mc") +
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"), 
        axis.text.x = element_text(angle = 90)) +
  facet_wrap(. ~ no_topic, ncol = 5)

## Save the visualized result
ggsave("fig/Fig_temporal-topic-trend.png",
       units = "mm", width = 174, height = 150)
ggsave("fig/Fig_temporal-topic-trend.eps", 
       units = "mm", width = 174, height = 150, device = cairo_ps)

# 1doc-1topicでやる場合
N_doc_year <- theta %>% 
  group_by(year) %>% 
  summarise(n_doc = n())
theta %>% 
  group_by(topic, year) %>% 
  summarise(n = n()) %>% 
  left_join(N_doc_year, by = "year") %>% 
  mutate(freq = n / n_doc) %>% 
  ggplot(aes(x = year, y = freq, 
             group = topic, colour = as.factor(topic))) + 
  geom_line(linewidth = 1, show.legend = FALSE) + 
  scale_color_manual(values = as.vector(pal_orig)) + 
  facet_wrap(. ~ topic, ncol = 6, nrow = 5) +
  labs(x = "Year", y = "Probability") +
  theme_ipsum(base_family = "Helvetica", 
              base_size = 8, 
              axis_text_size = 8, 
              axis_title_size = 8, 
              axis_title_just = "mc") +
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"), 
        axis.text.x = element_text(angle = 90)) +
  facet_wrap(. ~ topic, ncol = 5)
