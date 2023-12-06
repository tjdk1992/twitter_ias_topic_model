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
               hrbrthemes,
               rstatix
               )

# Data
tweet_finalized <- read_csv("data/tweet-05_finalized.csv")
dat_ias_ja <- read_csv("data/basic-ias-info.csv")

# Color palette
pal_orig <- pals::cols25(7)[c(5, 4, 7, 2, 1, 6, 3)]

# Annual trends ---------------------------------------------------------------

dat_ias_ja$group_biol <- factor(dat_ias_ja$group_biol, 
                                levels = c("mammal", "bird", "reptile", 
                                           "amphibian", "fish", 
                                           "invertebrate", "plant"))

# The number of tweets of each IAS
tweet_finalized %>% 
  group_by(name_sp, year) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  left_join(dat_ias_ja %>% 
              dplyr::select(name_sp, group_biol) %>% 
              distinct(),
            by = "name_sp") %>% 
  mutate(group_biol = str_to_title(group_biol)) %>% 
  ggplot(aes(x = year, y = count)) +
  geom_line(aes(group = name_sp, colour = group_biol), 
            linewidth = 0.5,
            show.legend = FALSE) +
  facet_wrap(. ~ group_biol, ncol = 2) +
  scale_color_manual(values = pal_orig) +
  theme_ipsum(base_size = 10,
              axis_title_size = 10,
              strip_text_size = 10,
              axis_text_size = 8,
              axis_title_just = "center",
              base_family = "Helvetica")

ggsave("fig-suppl/linegraph_ias-count-annual-trend.png", 
       units = "mm", width = 170, height = 200)
ggsave("fig-suppl/linegraph_ias-count-annual-trend.eps", 
       units = "mm", width = 170, height = 200, device = cairo_ps)

# Bursted NIS -----------------------------------------------------------------

tweet_finalized %>% 
  group_by(name_sp, year) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  left_join(dat_ias_ja %>% 
              dplyr::select(name_sp, group_biol) %>% 
              distinct(),
            by = "name_sp") %>% 
  arrange(desc(count)) %>% 
  mutate(id_top = row_number()) %>% 
  filter(id_top <= 50) %>% 
  mutate(sp_year = str_c(name_sp, "_", year),
         S_invicta = if_else(name_sp == "Solenopsis invicta", "1", "2")) %>% 
  ggplot(aes(x = reorder(sp_year, count), y = count)) +
  geom_bar(aes(fill = S_invicta), stat = "identity", show.legend = FALSE) +
  geom_text(aes(x = reorder(sp_year, count), y = count,
                label = as.character(count), 
                hjust = -0.2), size = 3) +
  scale_fill_manual(values = c("#ff0000", "#565656")) +
  ylim(0, 5000) +
  labs(x = "", y = "Count") +
  coord_flip() +
  theme_ipsum(base_size = 10,
              axis_title_size = 8,
              axis_text_size = 8,
              axis_title_just = "center",
              base_family = "Helvetica") +
  theme(axis.text.y = element_text(face = "italic"))

# Save the visualized result
ggsave("fig-suppl/bargraph-ias-count-burst_1-50.png",
       units = "mm", width = 170, height = 230)
ggsave("fig-suppl/bargraph-ias-count-burst_1-50.eps",
       units = "mm", width = 170, height = 230, device = cairo_ps)

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
  mutate(id_top = row_number()) %>% 
  filter(id_top <= 100 & id_top >= 51) %>% 
  mutate(sp_year = str_c(name_sp, "_", year),
         S_invicta = if_else(name_sp == "Solenopsis invicta", "1", "2")) %>% 
  ggplot(aes(x = reorder(sp_year, count), y = count)) +
  geom_bar(aes(fill = S_invicta), stat = "identity", show.legend = FALSE) +
  geom_text(aes(x = reorder(sp_year, count), y = count,
                label = as.character(count), 
                hjust = -0.2), size = 3) +
  scale_fill_manual(values = c("#ff0000", "#565656")) +
  ylim(0, 5000) +
  labs(x = "", y = "Count") +
  coord_flip() +
  theme_ipsum(base_size = 10,
              axis_title_size = 8,
              axis_text_size = 8,
              axis_title_just = "center",
              base_family = "Helvetica") +
  theme(axis.text.y = element_text(face = "italic"))

# Save the visualized result
ggsave("fig-suppl/bargraph-ias-count-burst_50-100.png",
       units = "mm", width = 170, height = 230)
ggsave("fig-suppl/bargraph-ias-count-burst_50-100.eps",
       units = "mm", width = 170, height = 230, device = cairo_ps)

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
  summarise(total = sum(count),
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

# Summarizing unfiltered values ------------------------------------------------------

# All
tweet_count %>% 
  summarise(mean = round(mean(total), 2),
            sd = round(sd(total), 2),
            min = min(total),
            max = max(total)) %>% 
  as.data.frame()

# Max and Minimum
tweet_count %>% 
  arrange(desc(total)) %>% 
  head(5)
tweet_count %>% 
  filter(total == 1) %>% 
  group_by(group_biol) %>% 
  summarise(n = n())

# Distribution
tweet_count %>% 
  ggplot(aes(x = total)) +
  geom_histogram(bins = 50) +
  labs(x = "The number of tweets", 
       y = "The number of species") +
  theme_ipsum(base_size = 10,
              axis_title_size = 10,
              strip_text_size = 10,
              axis_text_size = 8,
              axis_title_just = "center",
              base_family = "Helvetica",
              plot_margin = margin(5, 5, 5, 5)) + 
  theme(legend.position = "right",
        legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.key.size = unit(5, 'mm'),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
  labs(x = "")

# Save the visualized result
ggsave("fig-suppl/histogram-ias-count.png",
       units = "mm", width = 150, height = 100)
ggsave("fig-suppl/histogram-ias-count.eps",
       units = "mm", width = 150, height = 100, device = cairo_ps)

# Popular NIS
tweet_count %>% 
  arrange(desc(total)) %>% 
  head(10) %>% 
  summarise(sum = sum(total))
tweet_count %>% 
  summarise(sum = sum(total))
round(88844/186028 * 100, 2)
tweet_count %>% 
  filter(total >= 100) %>% 
  group_by(group_biol) %>% 
  summarise(n = n())

# Top NIS
arrange(filter(tweet_count, group_biol == "mammal"), desc(total))
arrange(filter(tweet_count, group_biol == "bird"), desc(total))
arrange(filter(tweet_count, group_biol == "reptile"), desc(total))
arrange(filter(tweet_count, group_biol == "amphibian"), desc(total))
arrange(filter(tweet_count, group_biol == "fish"), desc(total))
arrange(filter(tweet_count, group_biol == "invertebrate"), desc(total))
arrange(filter(tweet_count, group_biol == "plant"), desc(total))

# Taxonomic group for popular NIS
tweet_popular <- tweet_count %>% 
  filter(total >= 100) %>% 
  dplyr::select(name_sp, name_ja, group_biol, total)

tweet_popular %>% 
  group_by(group_biol) %>% 
  summarise(mean = round(mean(total), 2),
            sd = round(sd(total), 2),
            min = min(total),
            max = max(total)) %>% 
  as.data.frame()

write_csv(tweet_popular, "data/ias-popular.csv")

# Visualization ---------------------------------------------------------------

tweet_popular$group_biol <- factor(tweet_popular$group_biol, 
                                   levels = c("mammal", "bird", "reptile", 
                                              "amphibian", "fish", 
                                              "invertebrate", "plant"))

tweet_popular %>% 
  arrange(desc(total)) %>% 
  head(50) %>% 
  ggplot(aes(x = reorder(name_sp, desc(total)), y = total)) +
  geom_bar(aes(fill = group_biol), stat = "identity") +
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
        legend.title = element_blank(),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
  labs(x = "")

# 日本語版 for 解釈
# tweet_popular %>% 
#   arrange(desc(total)) %>% 
#   head(50) %>% 
#   ggplot(aes(x = reorder(name_ja, desc(total)), y = total)) +
#   geom_bar(aes(fill = group_biol), stat = "identity") +
#   scale_fill_manual(values = pal_orig) + # cols25かalphabet2のどちらかが良さそう。
#   labs(x = "Species", 
#        y = "No. of tweets") +
#   theme_ipsum(base_size = 12,
#               axis_title_size = 12,
#               strip_text_size = 12,
#               axis_text_size = 12,
#               axis_title_just = "center",
#               base_family = "HiraKakuPro-W3",
#               plot_margin = margin(5, 5, 5, 5)) + 
#   theme(axis.text.x = element_text(angle = 60, hjust = 1, face = "italic"), 
#         legend.position = "right",
#         legend.box.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
#         legend.key.size = unit(5, 'mm'),
#         plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
#   labs(x = "")

# Save the visualized result
ggsave("fig/barplot_top-occurred-ias.png",
       units = "mm", width = 190, height = 120)
ggsave("fig/barplot_top-occurred-ias.eps",
       units = "mm", width = 190, height = 120, device = cairo_ps)

# Each species
l_biol <- c("mammal", "bird", "reptile", "amphibian", 
            "fish", "invertebrate", "plant")
vis_n_tweet <- list()
for (i in 1:length(l_biol)) {
  # Biological group
  biol <- l_biol[i]
  vis_n_tweet[[i]] <- tweet_count %>% 
    filter(total >= 100 & group_biol == biol) %>% 
    ggplot(aes(x = reorder(name_sp, desc(total)), y = total)) +
    geom_bar(aes(fill = group_biol), stat = "identity") +
    scale_fill_manual(values = pal_orig[i]) + # cols25かalphabet2のどちらかが良さそう。
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
    labs(subtitle = str_to_title(biol), x = "Species")
}

# Layout and save the visualized result
((vis_n_tweet[[1]] | vis_n_tweet[[2]]) /
  (vis_n_tweet[[3]] | vis_n_tweet[[4]])) & theme(legend.position = 'none')
ggsave("fig-suppl/barplot_ias-tweet-count_A.png",
       units = "mm", width = 170, height = 200)
ggsave("fig-suppl/barplot_ias-tweet-count_A.eps",
       units = "mm", width = 170, height = 200, device = cairo_ps)

((vis_n_tweet[[5]] | vis_n_tweet[[6]]) / 
  vis_n_tweet[[7]]) & theme(legend.position = 'none')
ggsave("fig-suppl/barplot_ias-tweet-count_B.png",
       units = "mm", width = 170, height = 200)
ggsave("fig-suppl/barplot_ias-tweet-count_B.eps",
       units = "mm", width = 170, height = 200, device = cairo_ps)
