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

# Annual trends ---------------------------------------------------------------

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
  facet_wrap(. ~ group_biol, ncol = 3) +
  scale_color_manual(values = pal_orig) +
  theme_ipsum(base_size = 10,
              axis_title_size = 10,
              strip_text_size = 10,
              axis_text_size = 8,
              axis_title_just = "center",
              base_family = "Helvetica")

# Save the visualized result
ggsave("fig-suppl/linegraph-ias-occurrence-trend.png",
       units = "mm", width = 150, height = 120)
ggsave("fig-suppl/linegraph-ias-occurrence-trend.eps",
       units = "mm", width = 150, height = 120, device = cairo_ps)

# Distribution of annual summary（社会現象級を見つけたい）
tweet_finalized %>% 
  group_by(name_sp, year) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  left_join(dat_ias_ja %>% 
              dplyr::select(name_sp, group_biol) %>% 
              distinct(),
            by = "name_sp") %>% 
  arrange(desc(count)) %>% 
  head(50) %>% 
  mutate(sp_year = str_c(name_sp, "_", year),
         S_invicta = if_else(name_sp == "Solenopsis invicta", "1", "2")) %>% 
  ggplot(aes(x = reorder(sp_year, count), y = count)) +
  geom_bar(aes(fill = S_invicta), stat = "identity") +
  scale_fill_manual(values = c(pal_orig[2], pal_orig[1])) +
  coord_flip() +
  theme_ipsum(base_size = 10,
              axis_title_size = 10,
              strip_text_size = 10,
              axis_text_size = 8,
              axis_title_just = "center",
              base_family = "Helvetica") +
  theme(axis.text.y = element_text(face = "italic"))

# Save the visualized result
ggsave("fig-suppl/linegraph-ias-occurrence-trend.png",
       units = "mm", width = 150, height = 120)
ggsave("fig-suppl/linegraph-ias-occurrence-trend.eps",
       units = "mm", width = 150, height = 120, device = cairo_ps)

# Solenopsis invictaだけプロットを見ておく
tweet_finalized %>%
  mutate(month = month(date),
         ym = date(str_c(year, "-", month, "-01"))) %>% 
  group_by(name_sp, ym) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  left_join(dat_ias_ja %>% 
              dplyr::select(name_sp, group_biol) %>% 
              distinct(),
            by = "name_sp") %>% 
  filter(name_sp == "Solenopsis invicta") %>% 
  ggplot(aes(x = ym, y = count)) +
  geom_bar(stat = "identity", show.legend = FALSE, size = 0.3) +
  facet_wrap(. ~ name_sp, ncol = 1) +
  theme_ipsum(base_size = 10,
              axis_title_size = 10,
              strip_text_size = 10,
              axis_text_size = 8,
              axis_title_just = "center",
              base_family = "Helvetica")

# Save the visualized result
ggsave("fig-suppl/bargraph-ias-occurrence-trend.png",
       units = "mm", width = 150, height = 120)
ggsave("fig-suppl/bargraph-ias-occurrence-trend.eps",
       units = "mm", width = 150, height = 120, device = cairo_ps)

# Extract typical value -------------------------------------------------------

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

# カタカナ名による重複を除外しておく
glimpse(dat_ias_ja)
dat_ias_ja <- dat_ias_ja %>% 
  dplyr::select(group_biol, name_ja, name_sp, reg1, reg2) %>% 
  distinct(.keep_all = TRUE)

# Merge
tweet_count <- left_join(tweet_count, dat_ias_ja, by = "name_sp")

# Write data
write_csv(tweet_count, "data/ias-count.csv")

# Visualization ---------------------------------------------------------------

ias_top <- tweet_count %>% 
  arrange(desc(median)) %>% 
  head(50) %>% 
  dplyr::select(name_sp, median)
  
tweet_finalized %>% 
  group_by(name_sp, year) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  inner_join(dat_ias_ja %>% 
               dplyr::select(name_sp, group_biol) %>% 
               distinct(), 
             by = "name_sp") %>% 
  inner_join(ias_top, by = "name_sp") %>% 
  ggplot(aes(x = reorder(name_sp, desc(median)), y = count)) +
  geom_boxplot(aes(fill = group_biol), outlier.size = 0.5, linewidth = 0.5) +
  scale_fill_manual(values = pal_orig) + # cols25かalphabet2のどちらかが良さそう。
  labs(x = "Species", 
       y = "No. of tweets") +
  theme_ipsum(base_size = 10,
              axis_title_size = 10,
              strip_text_size = 10,
              axis_text_size = 8,
              axis_title_just = "center",
              base_family = "Helvetica",
              plot_margin = margin(5, 5, 5, 5)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, face = "italic"), 
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

# Each species
l_biol <- unique(pull(tweet_count, group_biol))
for (i in 1:length(l_biol)) {
  biol <- l_biol[i]
  # Plot
  tweet_finalized %>% 
    group_by(name_sp, year) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    inner_join(dat_ias_ja %>% 
                 dplyr::select(name_sp, group_biol) %>% 
                 distinct(), 
               by = "name_sp") %>% 
    inner_join(tweet_count %>% 
                 dplyr::select(name_sp, mean, median), 
               by = "name_sp") %>% 
    filter(group_biol == biol) %>% 
    ggplot(aes(x = reorder(name_sp, desc(median)), y = count)) +
    geom_boxplot(outlier.size = 0.5, linewidth = 0.5) +
    labs(x = "Species", 
         y = "No. of tweets") +
    theme_ipsum(base_size = 10,
                axis_title_size = 10,
                strip_text_size = 10,
                axis_text_size = 8,
                axis_title_just = "center",
                base_family = "Helvetica",
                plot_margin = margin(5, 5, 5, 5)) + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1, face = "italic"), 
          legend.position = "right",
          legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
          legend.key.size = unit(5, 'mm'),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
    labs(x = "")
  # Save path
  path_png <- str_c("fig/boxplot_ias-count_", biol, ".png")
  path_eps <- str_c("fig/boxplot_ias-count_", biol, ".eps")
  # Save plot
  ggsave(path_png, units = "mm", width = 150, height = 90)
  ggsave(path_eps, units = "mm", width = 150, height = 90, device = cairo_ps)
}
