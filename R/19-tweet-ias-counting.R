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
               lubridate,
               hrbrthemes
               )

# Data
tweet_finalized <- read_csv("data/tweet-05_finalized.csv")
dat_ias_ja <- read_csv("data/basic-ias-info.csv")

# Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

# Count tweets-----------------------------------------------------------------

# The number of tweets of each IAS
tweet_finalized %>% 
  group_by(name_sp, year) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  left_join(dat_ias_ja %>% 
              dplyr::select(name_sp, group_biol) %>% 
              distinct(),
            by = "name_sp") %>% 
  ggplot(aes(x = year, y = count)) +
  geom_line(aes(group = name_sp, colour = group_biol), 
            linewidth = 0.5,
            show.legend = FALSE) +
  facet_wrap(. ~ group_biol) +
  scale_color_manual(values = pal_orig) +
  theme_ipsum(base_family = "Helvetica")

# Yearly count (top 10 species)
ias_top <- tweet_finalized %>% 
  group_by(name_sp) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(30) %>% 
  dplyr::select(name_sp)

tweet_finalized %>% 
  inner_join(ias_top, by = "name_sp") %>% 
  group_by(name_sp, year) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = year, y = count)) +
  geom_bar(stat = "identity") +
  facet_wrap(. ~ name_sp, ncol = 6) +
  theme_ipsum(plot_margin = margin(5, 5, 5, 5),
              strip_text_face = "italic")

tweet_finalized %>% 
  inner_join(ias_top, by = "name_sp") %>%
  group_by(name_sp, year) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  inner_join(dat_ias_ja %>% 
               dplyr::select(name_sp, group_biol) %>% 
               distinct(), 
             by = "name_sp") %>% 
  ggplot(aes(x = reorder(name_sp, count), y = count)) +
  geom_boxplot(aes(fill = group_biol), outlier.size = 0.5, linewidth = 0.5) +
  scale_fill_manual(values = pal_orig) + # cols25かalphabet2のどちらかが良さそう。
  labs(x = "Species", 
       y = "No. of tweets") +
  theme_ipsum(base_family = "Helvetica", 
              base_size = 8, 
              axis_text_size = 8,
              axis_title_size = 10,
              axis_title_just = "mc") + 
  scale_fill_manual(values = pal_orig) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        axis.text.y = element_text(face = "italic"),
        legend.position = "right",
        legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.key.size = unit(5, 'mm'),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
  labs(x = "") +
  ylim(0, 3000) # Outlier of Solenopsis invicta was removed from the plot

# Save the visualized result
ggsave("fig/boxplot_top-occurred-ias.png",
       units = "mm", width = 190, height = 120)
ggsave("fig/boxplot_top-occurred-ias.eps",
       units = "mm", width = 190, height = 120, device = cairo_ps)

# 最初に出現した年の分布
tweet_finalized %>%
  group_by(name_sp, year) %>% 
  summarise(count = n()) %>% 
  group_by(name_sp) %>% 
  summarise(year_1st_occur = min(year)) %>% 
  group_by(year_1st_occur) %>% 
  summarise(n = n())

# Calculate typical values
tweet_count <- tweet_finalized %>%
  group_by(name_sp, year) %>% 
  summarise(count = n()) %>% 
  group_by(name_sp) %>% 
  summarise(sum = sum(count),
            mean = mean(count),
            sd = sd(count),
            se = sd(count) / n(),
            mean_1 = mean(count, trim = 0.1),
            mean_3 = mean(count, trim = 0.3),
            mean_5 = mean(count, trim = 0.5),
            median = median(count),
            min = min(count),
            max = max(count),
            mid_range = ((min + max)/2))

# Select typical value for this study
filter(tweet_count, str_detect(name_sp, "Soleno|Micro|Lepomis"))

# Re-add the IAS information---------------------------------------------------

# カタカナ名による重複を除外しておく
glimpse(dat_ias_ja)
dat_ias_ja <- dat_ias_ja %>% 
  dplyr::select(group_biol, name_ja, name_sp, reg1, reg2) %>% 
  distinct(.keep_all = TRUE)

# Merge
tweet_count <- left_join(tweet_count, dat_ias_ja, by = "name_sp")

# Write data
write_csv(tweet_count, "data/ias-count.csv")
