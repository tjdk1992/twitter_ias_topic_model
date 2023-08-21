#-----------------------------------------------------------------------------#
# Script Name: 23-comprehensive-assessment.R                                  #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script analyzes topic distribution over taxonomic groups and         #
# predictability of the IAS occurrence by the topic distribution              #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Packages
pacman::p_load(tidyverse,  # for data manipulation
               hrbrthemes, # for nice visualization
               magrittr, 　# for data manipulation
               pals,
               patchwork  # to use color palette
               )

# Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

# Data

## LDA output
theta <- read_csv("data/lda-output-03_doc-topic-tweet.csv")

## Tweets counts
ias_count <- read_csv("data/ias-count.csv")

## 最終的に用いる代表値
ias_count %<>% 
  transmute(name_sp, name_ja, 
            group_biol, 
            reg1, reg2, 
            sum = sum,
            count = median # final typical value
  )

## Order of biological group
ias_count$group_biol <- factor(ias_count$group_biol, 
                               levels = c("mammal", "bird", "reptile", 
                                          "amphibian", "fish", 
                                          "invertebrate", "plant"))
# Data preparation ------------------------------------------------------------

# Count of document aligned given topics
N_doc <- theta %>% 
  group_by(name_sp) %>% 
  summarise(n_doc = n())
theta_topic <- theta %>% 
  group_by(name_sp, topic) %>% 
  summarise(n = n()) %>% 
  left_join(N_doc, by = "name_sp") %>% 
  mutate(freq = n / n_doc) %>% 
  dplyr::select(name_sp, topic, freq) %>% 
  ungroup() %>% 
  arrange(topic) %>% 
  pivot_wider(names_from = topic, values_from = freq)
theta_topic[is.na(theta_topic)] <- 0

# Merge count data to LDA output
theta_topic <- inner_join(ias_count, theta_topic, by = "name_sp")

# -----------------------------------------------------------------------------

theta_topic %>% 
  filter(count >= 100) %>% 
  pivot_longer(cols = TP01:TP25,
               names_to = "topic",
               values_to = "freq") %>% 
  group_by(topic, group_biol) %>% 
  summarise(prob = mean(freq)) %>% 
  ggplot(aes(x = topic, y = prob)) +
  geom_bar(stat = "identity") +
  facet_wrap(. ~ group_biol, ncol = 2) +
  theme_ipsum()

# Association visualization ---------------------------------------------------

g_bubble_topic <- theta_topic %>% 
  arrange(desc(count)) %>% 
  group_by(group_biol) %>% 
  mutate(rank_group = row_number()) %>% 
  ungroup() %>% 
  filter(sum >= 100 & rank_group <= 10) %>% 
  arrange(count) %>% 
  arrange(group_biol) %>% 
  mutate(id_reorder = row_number()) %>% 
  pivot_longer(cols = TP01:TP25, 
               names_to = "topic", 
               values_to = "value") %>% 
  ggplot(aes(x = topic, y = reorder(name_sp, id_reorder), label = group_biol)) +
  geom_point(aes(size = value, color = group_biol), alpha = 0.7) +
  scale_color_manual(values = pal_orig) + # cols25かalphabet2のどちらかが良さそう。
  scale_size(range = c(0.05, 10)) +  # Adjust the range of points size
  scale_x_discrete(position = "top") +
  labs(x = "", y = "") +
  theme_ipsum(base_family = "HiraKakuPro-W3",
              base_size = 8) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))

g_bar_rank <- theta_topic %>% 
  arrange(desc(count)) %>% 
  group_by(group_biol) %>% 
  mutate(rank_group = row_number()) %>% 
  ungroup() %>% 
  filter(sum >= 100, rank_group <= 10) %>% 
  arrange(count) %>% 
  arrange(group_biol) %>% 
  mutate(id_reorder = row_number()) %>% 
  ggplot() + 
  geom_bar(aes(x = reorder(name_sp, id_reorder), 
               y = count, 
               fill = group_biol), 
           stat = "identity",
           show.legend = FALSE) +
  geom_text(aes(x = reorder(name_sp, id_reorder), 
                y = count, 
                label = str_c(round(count, 2), "(", sum, ")"), 
                hjust = -0.2), size = 3) +
  scale_fill_manual(values = pal_orig) + # cols25かalphabet2のどちらかが良さそう。
  labs(x = "Species", 
       y = "No. of tweets") +
  theme_ipsum(base_family = "HiraKakuPro-W3", 
              base_size = 8, 
              axis_text_size = 8,
              axis_title_size = 10,
              axis_title_just = "mc") + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
  coord_flip()

g_bubble_topic + 
  g_bar_rank + 
  plot_layout(guides = "collect", widths = c(7, 2)) & 
  theme(legend.position = 'bottom') # 縦横比を設定し凡例をまとめ

# Save the visualized result
ggsave("fig/bubble-biol-ordered-category.png",
       units = "mm", width = 170, height = 200)
ggsave("fig/bubble-biol-ordered-category.eps",
       units = "mm", width = 170, height = 200, device = cairo_ps)

# Association visualization ---------------------------------------------------
H = 75

g_bubble_topic2 <- theta_topic %>% 
  arrange(desc(count)) %>% 
  head(H) %>% 
  arrange(count) %>% 
  mutate(id_reorder = row_number()) %>% 
  pivot_longer(cols = TP01:TP25, 
               names_to = "topic", 
               values_to = "value") %>% 
  ggplot(aes(x = topic, 
             y = reorder(name_ja, id_reorder), 
             label = group_biol)) +
  geom_point(aes(size = value, color = group_biol), alpha = 0.7) +
  scale_color_manual(values = pal_orig) + # cols25かalphabet2のどちらかが良さそう。
  scale_size(range = c(0.05, 10)) +  # Adjust the range of points size
  theme_ipsum(base_family = "HiraKakuPro-W3",
              base_size = 8) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
  scale_x_discrete(position = "top")

g_bar_rank2 <- theta_topic %>% 
  arrange(desc(count)) %>% 
  head(H) %>%  
  arrange(count) %>% 
  mutate(id_reorder = row_number()) %>% 
  ggplot() + 
  geom_bar(aes(x = reorder(name_ja, id_reorder), 
               y = count, 
               fill = group_biol), 
           stat = "identity",
           show.legend = FALSE) +
  geom_text(aes(x = reorder(name_ja, id_reorder), y = count, label = round(count, 2), hjust = -0.2), size = 3) +
  scale_fill_manual(values = pal_orig) + # cols25かalphabet2のどちらかが良さそう。
  labs(x = "Species", 
       y = "No. of tweets") +
  theme_ipsum(base_family = "HiraKakuPro-W3", 
              base_size = 8, 
              axis_text_size = 8,
              axis_title_size = 10,
              axis_title_just = "mc") + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
  coord_flip()

g_bubble_topic2 + 
  g_bar_rank2 + 
  plot_layout(guides = "collect", widths = c(5, 1)) & 
  theme(legend.position = 'bottom') # 縦横比を設定し凡例をまとめ

# Save the visualized result
ggsave("fig/bubble-biol-category.png",
       units = "mm", width = 170, height = 200)
ggsave("fig/bubble-biol-category.eps",
       units = "mm", width = 170, height = 200, device = cairo_ps)

# Association visualization ---------------------------------------------------

sp = "bird"
g_bubble_topic3 <- theta_topic %>% 
  filter(group_biol == sp) %>% 
  arrange(count) %>% 
  mutate(id_reorder = row_number()) %>% 
  pivot_longer(cols = TP01:TP25, 
               names_to = "topic", 
               values_to = "value") %>% 
  ggplot(aes(x = topic, y = reorder(name_ja, id_reorder), label = group_biol)) +
  geom_point(aes(size = value, color = group_biol), alpha = 0.7) +
  scale_color_manual(values = pal_orig) + # cols25かalphabet2のどちらかが良さそう。
  scale_size(range = c(0.05, 10)) +  # Adjust the range of points size
  theme_ipsum(base_family = "HiraKakuPro-W3",
              base_size = 8) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
  scale_x_discrete(position = "top")

g_bar_rank3 <- theta_topic %>% 
  filter(group_biol == sp) %>% 
  arrange(count) %>% 
  mutate(id_reorder = row_number()) %>% 
  ggplot() + 
  geom_bar(aes(x = reorder(name_ja, id_reorder), 
               y = count, 
               fill = group_biol), 
           stat = "identity",
           show.legend = FALSE) +
  geom_text(aes(x = reorder(name_ja, id_reorder), 
                y = count, label = count, hjust = -0.2), size = 3) +
  scale_fill_manual(values = pal_orig) + # cols25かalphabet2のどちらかが良さそう。
  labs(x = "Species", 
       y = "No. of tweets") +
  theme_ipsum(base_family = "HiraKakuPro-W3", 
              base_size = 8, 
              axis_text_size = 8,
              axis_title_size = 10,
              axis_title_just = "mc") + 
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")) +
  coord_flip()

g_bubble_topic3 + 
  g_bar_rank3 + 
  plot_layout(guides = "collect", widths = c(5, 1)) # 縦横比を設定し凡例をまとめ

# # Hierarchical clustering -----------------------------------------------------

# library(vegan)
# 
# theta_topic_clust <- filter(theta_topic, count >= 100)
# 
# # あああ
# df_clust <- dplyr::select(theta_topic_clust, TP01:TP25)
# dist_ias <- vegdist(df_clust, method = "bray")
# res_clust <- hclust(dist_ias, method = "ward.D2")
# plot(res_clust)
# 
# rect.hclust(res_clust, 2, border = "red")
# rect.hclust(res_clust, 3, border = "blue")
# rect.hclust(res_clust, 4, border = "green")
# 
# theta_topic_clust <- theta_topic_clust %>%
#   mutate(k2 = cutree(res_clust, 2),
#          k3 = cutree(res_clust, 3),
#          k4 = cutree(res_clust, 4))
# 
# theta_topic_clust %>%
#   pivot_longer(cols = c(k2, k3, k4),
#                names_to = "k",
#                values_to = "clust") %>%
#   ggplot() +
#   geom_boxplot(aes(x = clust, y = log(count), group = clust)) +
#   facet_wrap(. ~ k) +
#   theme_ipsum()
# 
# theta_topic_clust %>%
#   pivot_longer(cols = TP01:TP25,
#                names_to = "topic",
#                values_to = "freq") %>%
#   group_by(topic, k3) %>%
#   summarise(mean_freq = mean(freq)) %>%
#   ggplot(aes(x = k3, y = mean_freq, fill = topic)) +
#   geom_bar(stat = "identity", position = "fill") +
#   scale_fill_manual(values = pal_orig) +
#   theme_ipsum()
# 
# theta_topic_clust %>%
#   pivot_longer(cols = TP01:TP25,
#                names_to = "topic",
#                values_to = "freq") %>%
#   group_by(topic, k2) %>%
#   summarise(mean_freq = mean(freq)) %>%
#   ggplot(aes(x = topic, y = mean_freq)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(. ~ k2) +
#   scale_fill_manual(values = pal_orig) +
#   theme_ipsum()

# -----------------------------------------------------------------------------

data.frame(value = quantile(theta_topic$count))

theta_topic %>% filter(count <= 5.00)
theta_topic %>% filter(count > 5.00, count <= 27.50)
theta_topic %>% filter(count > 27.50, count <= 126.75)

theta_topic_quant <- theta_topic %>% 
  mutate(qt = if_else(count <= 5.00, "first",
                      if_else(count > 5.00 & count <= 27.50, "second",
                              if_else(count > 27.50 & count <= 126.75, "third", 
                                      "fourth")))) %>% 
  mutate(ht = if_else(count <= 27.50, "first", "second"))

theta_topic_quant %>% 
  pivot_longer(cols = TP01:TP25,
               names_to = "topic",
               values_to = "freq") %>% 
  group_by(topic, qt) %>% 
  summarise(mean_freq = mean(freq)) %>% 
  ggplot(aes(x = qt, y = mean_freq, fill = topic)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = pal_orig) +
  theme_ipsum()

theta_topic_quant %>% 
  pivot_longer(cols = TP01:TP25,
               names_to = "topic",
               values_to = "freq") %>% 
  group_by(topic, ht) %>% 
  summarise(mean_freq = mean(freq)) %>% 
  ggplot(aes(x = ht, y = mean_freq, fill = topic)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = pal_orig) +
  theme_ipsum()

# エントロピー算出
# https://bi.biopapyrus.jp/seq/entropy.html
pre_calc_entropy <- theta_topic_quant %>% 
  pivot_longer(cols = TP01:TP25,
               names_to = "topic",
               values_to = "freq") %>% 
  group_by(topic, qt) %>% 
  summarise(mean_freq = mean(freq)) %>% 
  ungroup()

pre_calc_entropy$ln.prob <- log(pre_calc_entropy$mean_freq)
pre_calc_entropy$pre.ent <- pre_calc_entropy$mean_freq * pre_calc_entropy$ln.prob
quantile_entropy <- pre_calc_entropy %>% 
  group_by(qt) %>% 
  summarise(entropy = sum(pre.ent)*(-1))

quantile_entropy %>% 
  ggplot(aes(x = qt, y = entropy)) +
  geom_bar(stat = "identity") + 
  theme_ipsum(base_size = 8, axis_title_size = 12) +
  labs(x="Country", y="Entropy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))








