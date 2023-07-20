#-----------------------------------------------------------------------------#
# Script Name: 11-stpw-rm-general.R                                           #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script removes stop words which can be automatically selected by     #
# using general tweets data.                                                  #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Package
library(tidyverse) # for data manipulation

## Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

## Data
token_rm_stpw_basic <- read_csv("data/tokens-04_rm-stpw-basic.csv")

# Automatic original stopword selection ---------------------------------------

# My general stopwords
stopword_test_tokens <- read_csv("~/Dropbox/02_Research/01_Project/04_Suspend/01_Lead/twitter_invasion_culturomics/data/df-id-tokens-general-tweet.csv")

# 動詞・形容詞のストップワードの作成

## 実際に解析に含まれたツイート数
len_tweet_stopword_test <- nrow(stopword_test_tokens %>% 
                                  distinct(.keep_all = TRUE) %>% 
                                  dplyr::select(title) %>% 
                                  distinct())

## 動詞、名詞、形容詞のみ抽出したものを書き出しておく。
## 他の研究の解析にも使える。
stopword_test_tokens %>% 
  filter(hinshi == "動詞" | hinshi == "形容詞" | hinshi == "名詞") %>% 
  write_csv("data/df-id-tokens-general-tweet.csv")

## 動詞・形容詞の抽出
df_id_stop_verb_test_tokens <- stopword_test_tokens %>% 
  distinct(.keep_all = TRUE) %>% # 同一ツイート中の複数の同じ単語の除外
  filter(hinshi == "動詞")
df_id_stop_abj_test_tokens <- stopword_test_tokens %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(hinshi == "形容詞")

## 動詞
stopword_verb_original <- df_id_stop_verb_test_tokens %>% 
  group_by(term) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(id = row_number(),
         freq = n / len_tweet_stopword_test * 100) %>% # n = 2599
  filter(freq > 0.05) %>% 
  dplyr::select(term)
View(stopword_verb_original)

## 形容詞
stopword_adj_original <- df_id_stop_abj_test_tokens %>% 
  group_by(term) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(id = row_number(),
         freq = n / len_tweet_stopword_test * 100) %>% # n = 2599
  filter(freq > 0.05) %>% 
  dplyr::select(term)
View(stopword_adj_original)

df_id_stop_verb_test_tokens %>% 
  filter(term == "です")

## 出現頻度 > 0.05の動詞・形容詞と
stopword_additional_original <- data.frame(
  term = c(
    ## 敬語表現、
    "らっしゃる", "られる", "ます", "なさる", "さる", 
    ## 敬称・接尾語、
    "くん", "君", "ちゃん", "氏", "さん",
    "さま", "様", "殿", "さん", "っち" ,
    ## 代名詞
    "わたし", "私", "ぼく", "僕", "あなた", 
    "きみ", "かれ", "彼", "かのじょ", "彼女",
    "だれ", "誰", "どなた", "これ", "ここ",
    "こちら", "それ", "そこ", "そちら", "あれ",
    "あそこ", "あちら", "どれ", "どこ", "どちら"
  )
)

## ストップワードを除外したデータの書き出し
token_rm_stpw_basic %<>% 
  anti_join(stopword_verb_original, by = "term") %>% 
  anti_join(stopword_adj_original, by = "term") %>% 
  anti_join(stopword_additional_original, by = "term")

## Write data
write_csv(token_rm_stpw_basic,
          "data/tokens-05_rm-stpw-general.csv")
