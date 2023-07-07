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
               pals　　　　# to use color palette
               )

# Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

# Data

## LDA output
theta <- read_csv("data/lda-output-03_doc-topic-tweet.csv")

## Tweets counts
tweet_count_total <- read_csv("data/ias-count_total.csv")

# Data preparation ------------------------------------------------------------

# Summarize LDA posterior by species
## Pattern 1: Probability of topics
theta_prob <- theta %>% 
  pivot_longer(cols = TP01:TP25,
               names_to = "no_topic", 
               values_to = "prob") %>% 
  group_by(name_sp, no_topic) %>% 
  summarise(prob = mean(prob)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = no_topic, 
              values_from = prob)
## Pattern 2: Count of document aligned given topics
N_doc <- theta %>% 
  group_by(name_sp) %>% 
  summarise(n_doc = n())
theta_topic <- theta %>% 
  group_by(name_sp, topic) %>% 
  summarise(n = n()) %>% 
  left_join(N_doc, by = "name_sp") %>% 
  mutate(freq = n / n_doc) %>% 
  ungroup() %>% 
  pivot_wider(names_from = topic, values_from = freq)
theta_topic[is.na(theta_topic)] <- 0

# Merge count data to LDA output
theta_prob_count <- inner_join(tweet_count_total, theta_prob, by = "name_sp")
theta_topic_count <- inner_join(tweet_count_total, theta_topic, by = "name_sp")

# 階層クラスタリング-----------------------------------------------------------

# クラスタリング用のデータフレーム
df_clust_theta_prob <- theta_prob_count %>% 
  dplyr::select(TP01:TP25) %>% 
  as.data.frame()
df_clust_theta_topic <- theta_topic_count %>% 
  dplyr::select(TP01:TP25) %>% 
  as.data.frame()

df_clust_theta_prob <- scale(df_clust_theta_prob)
df_clust_theta_topic <- scale(df_clust_theta_topic)

rownames(df_clust_theta_prob) <- theta_prob_count$name_sp
rownames(df_clust_theta_topic) <- theta_topic_count$name_sp

# ユークリッド距離の計算
dist_theta_prob <- dist(df_clust_theta_prob)
dist_theta_topic <- dist(df_clust_theta_topic)

# 階層クラスタリングの実装
rc_prob = hclust(d = dist_theta_prob, method = "ward.D2")
rc_topic = hclust(d = dist_theta_topic, method = "ward.D2")

# デンドログラムのチェック
plot(rc_prob)
plot(rc_topic)

# 各クラスタに振り分けられたのIASの数
for (i in 2:10) {
  print(table(cutree(rc, k = i)))
}

# k = 4か5が妥当そう…
theta_count$k4 <- cutree(rc, k = 4)
theta_count$k5 <- cutree(rc, k = 5)

theta_count %>% 
  ggplot(aes(y = log(count))) +
  geom_boxplot(aes(x = as.factor(k5))) +
  theme_ipsum()

theta_count %>% 
  pivot_longer(cols = TP01:TP25, 
               names_to = "topic", values_to = "prob") %>% 
  group_by(k5, topic) %>% 
  summarise(prob = mean(prob)) %>% 
  ggplot() +
  geom_tile(aes(x = k5, y = topic, fill = prob)) +
  scale_fill_gradient2(low = cols25(25)[1], 
                       mid = cols25(25)[21], 
                       high = cols25(25)[2], 
                       midpoint = 0.06)
  # scale_fill_viridis_c()

# Statistical modelling--------------------------------------------------------

# データの分布
theta_count %>% 
  ggplot() +
  geom_histogram(aes(x = count))

# 傾向の把握
theta_count %>% 
  pivot_longer(cols = TP01:TP25, 
               names_to = "topic", values_to = "prob") %>% 
  ggplot() +
  geom_tile(aes(x = topic, 
                y = reorder(name_sp, count), 
                fill = prob)) +
  scale_fill_gradient2(low = cols25(25)[1], 
                       mid = cols25(25)[21], 
                       high = cols25(25)[2], 
                       midpoint = 0.13)

# 負の二項分布（グループも説明変数に入れて解析）
mod_nb <- MASS::glm.nb(count ~ 
                         TP01 + TP02 + TP03 + TP04 + TP05 + 
                         TP06 + TP07 + TP08 + TP09 + TP10 +
                         TP11 + TP12 + TP13 + TP14 + TP15 +
                         TP16 + TP17 + TP18 + TP19 + TP20 +
                         TP21 + TP22 + TP23 + TP24 + TP25, 
                     data = theta_count)
summary(mod_nb)

# MuMInパッケージを用いた総当たり法によるモデル選択
library(MuMIn)
options(na.action = "na.fail")
res.AIC <- dredge(mod_nb, rank="AIC")
res.AIC
