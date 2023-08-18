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
  group_by(topic) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = reorder(topic, as.numeric(str_remove_all(topic, "TP"))), y = n)) +
  geom_bar(stat = "identity", fill = pal_orig[1]) +
  labs(x = "Topic", y = "The number of tweets") +
  theme_ipsum(axis_text_size = 12,
              axis_title_size = 12,
              axis_title_just = "center",
              base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90))

# Time series trends ----------------------------------------------------------

N_doc_year <- theta %>% 
  group_by(year) %>% 
  summarise(n_doc = n())
theta %>% 
  group_by(topic, year) %>% 
  summarise(n = n()) %>% 
  left_join(N_doc_year, by = "year") %>% 
  mutate(freq = n / n_doc) %>% 
  mutate(topic = str_replace_all(topic, "TP", "Topic ")) %>% 
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

# Save the visualized result
ggsave("fig/temporal-topic-trend.png",
       units = "mm", width = 174, height = 180)
ggsave("fig/temporal-topic-trend.eps", 
       units = "mm", width = 174, height = 180, device = cairo_ps)

