# R script for the analysis in:
# Tomojiri, D., Takaya, K. (2022) Quantitative assessment of relative topics
# and occurrence of invasive alien species in the Twitter.
# Submitted to "Conservation Biology"
#
# R Script 10: Manually determining the number of topics
#
# Author: Daiki Tomojiri
#
# Outline:
## Step 1. 

#------------------------------------------------------------------------------

# Setup

## Packages
library(tidyverse) # for data manipulation
library(magrittr) # to use %<>% command
library(wordcloud) # to draw wordcloud
library(pals) # to use color palette
library(hrbrthemes) # to use theme_ipsum() function
library(readxl)

## Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

## Data
### Outputs of LDA inference
theta <- read_csv("data/result-lda-inference-topic.csv")
beta <- read_csv("data/result-lda-inference-term.csv")

### 元のTweetデータ
tweet_ias <- read_csv("data-raw/tweets_ias_2008_2022.csv")

### LDA用にDTM変換する直前のデータ（前処理により複数のツイートデータが失われている。）
df_id_tokens_pre <- read_csv("data/df-id-tokens-rm-stopword.csv")

#------------------------------------------------------------------------------

# Characteristics of each topics

## Create data frame for Wordclouds
beta_df <- as.data.frame(beta)
beta_t_matrix <- t(beta_df)
beta_t_df <- as.data.frame(beta_t_matrix)

df_topic_name <- data.frame(topic = "topic", 
                            num1 = c(rep("0", 9), 
                                     rep("", 30-9)),
                            num2 = seq(1:30)) %>% 
  transmute(topic = str_c(topic, num1, num2))

colnames(beta_t_df) <- df_topic_name$topic
beta_t_df$term <- rownames(beta_t_df)

# Create wordcloud of each topic
for (i in 1:30) {
  # term-frequency
  df_wc <- beta_t_df %>% 
    transmute(term, freq = beta_t_df[, i]) %>% 
    arrange(desc(freq)) %>% 
    head(30) # top 30 words
  # term
  words <- df_wc$term
  # frequency
  freq <- as.vector(unlist(df_wc$freq))
  # family to show Japanese words
  par(family = "HiraKakuProN-W3")
  # random seed
  set.seed(135)
  # plot wordcloud
  path_wc <- str_c("fig/wc_topic/wc-", i, ".png")
  png(path_wc, width = 600, height = 600)
  par(family="HiraKakuPro-W3") ##日本語フォントでもok
  wordcloud(words = words, 
            freq = freq, 
            max.words = 30, 
            random.order = FALSE, 
            rot.per = 0.50, 
            colors = cols25(6))
  dev.off()
  Sys.sleep(1)
}

remove(beta, beta_df, beta_t_df, beta_t_matrix)

#------------------------------------------------------------------------------

# Topic distribution in whole text data

theta %>% 
  pivot_longer(cols = c(`1`:`30`),
               names_to = "topic",
               values_to = "prob") %>% 
  group_by(topic) %>% 
  summarise(sum = sum(prob)) %>% 
  ggplot(aes(x = reorder(topic, as.numeric(topic)), y = sum)) +
  geom_bar(stat = "identity", fill = pal_orig[1]) +
  labs(x = "Topic", y = "Posterior probability") +
  theme_ipsum(axis_text_size = 12,
              axis_title_size = 12,
              axis_title_just = "center",
              base_family = "Helvetica")

#------------------------------------------------------------------------------

# time series trend

## 最終的にLDAにかけられたもののみ抽出する。
lda_tweets <- tweet_ias %>% 
  right_join(df_id_tokens_pre %>% 
               transmute(id = title) %>% 
               distinct(.keep_all = FALSE), by = "id") %>% 
  arrange(id)

lda_tweets %<>% 
  arrange(id) %>% 
  mutate(year = lubridate::year(date)) %>% 
  dplyr::select(id, year, date, text)

## 年で集計する。
topic_proportion_per_year <- aggregate(theta, 
                                       by = list(year = lda_tweets$year), 
                                       mean)

## Plot
topic_proportion_per_year %>% 
  pivot_longer(cols = -1, 
               names_to = "topic", values_to = "prob") %>% 
  mutate(topic = as.numeric(topic)) %>% 
  mutate(name_add = if_else(topic <=  9, "0", ""),
         topic = str_c("Topic ", name_add, topic)) %>% 
  ggplot(aes(x = year, y = prob, 
             group = topic, colour = as.factor(topic))) + 
  geom_line(size = 1, show.legend = FALSE) + 
  scale_color_manual(values = as.vector(pal_orig)) + 
  facet_wrap(. ~ topic, ncol = 6, nrow = 5) +
  labs(x = "Year", y = "Probability") +
  theme_ipsum(base_family = "Helvetica", 
              base_size = 8, 
              axis_text_size = 8, 
              axis_title_size = 8, 
              axis_title_just = "mc") +
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"), 
        axis.text.x = element_text(angle = 90))

## Save the visualized result
ggsave("fig/Fig-S03_temporal-topic-trend.png",
       units = "mm", width = 174, height = 150)
ggsave("submission/biodivers-conserv-1st/images/Fig-S03_temporal-topic-trend.eps", 
       units = "mm", width = 174, height = 150, device = cairo_ps)

#------------------------------------------------------------------------------

# Ranking change

## 期間で集計する。
### 3期間
lda_tweets %<>% 
  mutate(period = if_else(
    year >=  2018, "3rd",
      if_else(
        year >=  2013, "2nd",
          if_else(
            year >=  2008, "1st", "other")))) %>% 
  dplyr::select(-year)
### 5期間
# lda_tweets %<>% 
#   mutate(period = if_else(
#     year > =  2020, "5th",
#     if_else(
#       year > =  2017, "4th",
#       if_else(
#         year > =  2014, "3rd",
#         if_else(
#           year > =  2011, "2nd",
#           if_else(
#             year > =  2008, "1st", "other")))))) %>% 
#   dplyr::select(-year)

## 年で集計する。
topic_proportion_per_period <- aggregate(theta, 
                                         by = list(period = lda_tweets$period), 
                                         mean)

## Ranking change of the topics
topic_proportion_per_period_t <- t(topic_proportion_per_period %>% 
                                     dplyr::select(-period)) %>% 
  as.data.frame()
colnames(topic_proportion_per_period_t) <- c("1st", "2nd", "3rd"
                                             #, "4th", "5th" # 5期間の場合
                                             )
rownames(topic_proportion_per_period_t) <- 
  colnames(topic_proportion_per_period %>% 
             dplyr::select(-period))

topic_proportion_per_period_t

topic_rank_change <- topic_proportion_per_period_t %>% 
  arrange(desc(`1st`)) %>% mutate(`1st` = c(1:30)) %>% 
  arrange(desc(`2nd`)) %>% mutate(`2nd` = c(1:30)) %>% 
  arrange(desc(`3rd`)) %>% mutate(`3rd` = c(1:30)) %>% 
  arrange(`1st`)

topic_rank_change %<>% 
  mutate(topic = rownames(topic_rank_change), 
         topic = as.integer(topic),
         change = `1st` - `3rd`) %>% 
  arrange(topic) %>% 
  mutate(name_add = c(rep(0, 9), rep("", 30-9)),
         name_topic = str_c("Topic", name_add, topic)) %>% 
  dplyr::select(-name_add)

topic_rank_change %<>% 
  left_join(topic_rank_change %>% 
              pivot_longer(cols = c(`1st`:`3rd`),
                           names_to = "period",
                           values_to = "rank") %>% 
              group_by(topic) %>% 
              summarise(min = min(rank),
                        max = max(rank)),
            by = "topic") %>% 
  mutate(initial = `1st`,
         final = `3rd`) %>% 
  mutate(topic_rank = str_c(name_topic, 
                            "(", initial, ", ", final, ")"))

## Rank change plot
topic_rank_change %>% 
  mutate(topic_rank = fct_reorder(topic_rank, change),
         arrow_change = if_else(change > 0, change - 0.3,
                                if_else(change < 0, change + 0.3, 0)),
         negpos = if_else(change > 0, "pos", "neg")) %>%
  ggplot() +
  geom_segment(aes(x = topic_rank,
                   xend = topic_rank,
                   y = 0,
                   yend = arrow_change),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black", 
               size = 0.3) +
  geom_point(aes(x = topic_rank, 
                 y = change, 
                 color = negpos), 
             size = 3, 
             show.legend = FALSE) +
  scale_color_manual(values = pal_orig[c(1, 2)]) +
  geom_text(aes(x = topic_rank, 
                y = change + 0.5, 
                label = ifelse(change > 0, as.character(change),'')), 
            hjust = 0) +
  geom_text(aes(x = topic_rank, 
                y = change - 0.5, 
                label = ifelse(change < 0, as.character(change),'')), 
            hjust = 1) +
  coord_flip()+
  labs(x = "Topic (final rank position)", 
       y = "Change in rank") +
  scale_y_continuous(breaks = seq(-50, 50, 10)) +
  theme_ipsum(base_family = "Helvetica", 
              base_size = 12, 
              axis_text_size = 8, 
              axis_title_size = 12, 
              axis_title_just = "mc") +
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"),
        axis.text.y = element_text(hjust = 0))

## Save the visualized result
ggsave("fig/Fig-03_topic-rank-change.png",
       units = "mm", width = 174, height = 150)
ggsave("submission/j-nat-conserv_1st/figs/Fig-03_topic-rank-change.eps", 
       units = "mm", width = 174, height = 150, device = cairo_ps)

## Rank change plot2
topic_rank_change %>% 
  mutate(negpos = if_else(change < 0, "neg", "pos")) %>% 
  pivot_longer(cols = c(`1st`:`3rd`),
               names_to = "period",
               values_to = "rank") %>% 
  ggplot(aes(x = period, 
             y = as.factor(rank), 
             group = topic, 
             color = negpos)) +
  geom_line(show.legend = FALSE) +
  geom_point(size = 3, 
             show.legend = FALSE) +
  scale_color_manual(values = pal_orig[c(1, 2)]) +
  labs(x = "Period", y = "Rank") +
  theme_ipsum()

## Save the visualized result
ggsave("fig/Fig-04_topic-rank-change-periods.png",
       units = "mm", width = 174, height = 150)
ggsave("submission/j-nat-conserv_1st/figs/Fig-04_topic-rank-change-periods.eps", 
       units = "mm", width = 174, height = 150, device = cairo_ps)

## ランク間の類似度を算出
attach(topic_rank_change)
cor(`1st`, `2nd`, method = "spearman")
cor(`2nd`, `3rd`, method = "spearman")
detach(topic_rank_change)

topic_rank_change %>% 
  ggplot(aes(x = `1st`, y = `2nd`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_ipsum()
topic_rank_change %>% 
  ggplot(aes(x = `2nd`, y = `3rd`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_ipsum()
