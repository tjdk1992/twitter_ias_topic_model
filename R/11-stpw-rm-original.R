#-----------------------------------------------------------------------------#
# Script Name: 11-stpw-rm-original.R                                          #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script removes stop words that selected manually from terms          #
# which are frequently occur in the retrieved tweets.                         #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

## Packages
library(tidyverse) # for data manipulation
library(tidytext) # to create document-term matrix (DTM)
library(readxl) # for reading excel sheet
library(lubridate) # For address date data
library(magrittr) # To use %<>% 
library(stringi) # for Japanese characters
library(academictwitteR) # to use Twitter API v2
library(tictoc) # for calculating time consumed in the processes
library(RMeCab) # for 
library(purrr) # for

## Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

## Data
### Preprocessed data
df_id_tokens_rm_stopword <- read_csv("data/df-id-tokens-cutted.csv")
### Original
df_id_tokens_orig <- read_csv("data/df-id-tokens-original.csv")
#------------------------------------------------------------------------------

# General stopwordsの除外

## General stopwordのダウンロード
## (svn.sourceforge.jp/svnroot/slothlib/CSharp/Version1/
## SlothLib/NLP/Filter/StopWord/word/Japanese.txt)
stopword_general <- read.csv("data/jpn_stopword.csv", 
                             header = T, fileEncoding = "UTF-8-BOM")

## General stopwordを除外
df_id_tokens_rm_stopword %<>% 
  anti_join(stopword_general, by = "term")

#------------------------------------------------------------------------------

# Original stopwordsの除外（名詞）

## ストップワード抽出用の単語リストを作成
## どの程度がいいんかな…
## 実際に分析に含まれたツイート数の取得
len_tweet <- nrow(df_id_tokens_rm_stopword %>% 
                    distinct(.keep_all = TRUE) %>% 
                    dplyr::select(title) %>% 
                    distinct()) # *0.001

## 分布はどんな感じ？
df_id_tokens_rm_stopword %>% 
  distinct(.keep_all = TRUE) %>% 
  group_by(term) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  mutate(freq_doc = freq / len_tweet * 100) %>% 
  filter(freq_doc > 0.1)

# 5%だったら上位22個
# 1%だったら上位224個
# 0.5%だったら上位561個
# 0.1%だったら上位2386個

## 分布を可視化してみる
df_id_tokens_rm_stopword %>% 
  distinct(.keep_all = TRUE) %>% 
  group_by(term) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  mutate(freq_doc = freq / len_tweet * 100) %>% 
  filter(freq_doc > 5) %>% # 5%以上
  ggplot(aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(family = "HiraKakuPro-W3",
                                 size = 8,
                                 angle = 90))

## トップ2854位までの単語に含まれる名詞
df_id_tokens_rm_stopword %>% 
  distinct(.keep_all = TRUE) %>% 
  group_by(term) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  mutate(freq_doc = freq / len_tweet * 100) %>% 
  filter(freq_doc > 0.1) %>% # 5%以上
  left_join(df_id_tokens_orig %>% 
              dplyr::select(-title) %>% 
              distinct(.keep_all = TRUE), by = "term") %>% 
  filter(hinshi == "名詞") %>% # 動詞と形容詞は自動でジャッジする
  writexl::write_xlsx("data/stopword-noun-selection-base.xlsx")

## selectionが終わったら"stopword-selection-finalized.excel"で保存する。
## Categories of noun stopword:

### A) organism
### B) place
### C) proper
### D) unknown

## read stopwords which authors judged manually
stopword_noun_original <- 
  read_excel("data/stopword-noun-selection-finalized_tomojiri.xlsx", sheet = 1)

## Stopword (noun)を除外
df_id_tokens_rm_stopword %<>% 
  anti_join(
    stopword_noun_original %>% 
      filter(organism == 1 | 
               place == 1 | 
               proper == 1 | 
               unknown == 1) %>%
      transmute(term), 
    by = "term"
  )

#------------------------------------------------------------------------------

# Original stopwordの除外（動詞・形容詞）
# 一般的なツイートから一般語を抽出（以前、すでにやっているのでそれを読み込む）
path_gen_tweet <- 
  "~/Dropbox/02_Research/01_Project/01_Lead/twitter_invasion_culturomics/"
df_id_stopword_test_tokens <- 
  read_csv(str_c(path_gen_tweet, "data/df-id-tokens-general-tweet.csv"))

## 実際に解析に含まれたツイート数
len_tweet_stopword_test <- nrow(df_id_stopword_test_tokens %>% 
                                  distinct(.keep_all = TRUE) %>% 
                                  dplyr::select(title) %>% 
                                  distinct())

## 動詞・形容詞の抽出
df_id_stop_verb_test_tokens <- df_id_stopword_test_tokens %>% 
  distinct(.keep_all = TRUE) %>% # 同一ツイート中の複数の同じ単語の除外
  filter(hinshi == "動詞")
df_id_stop_abj_test_tokens <- df_id_stopword_test_tokens %>% 
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

## 形容詞
stopword_adj_original <- df_id_stop_abj_test_tokens %>% 
  group_by(term) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(id = row_number(),
         freq = n / len_tweet_stopword_test * 100) %>% # n = 2599
  filter(freq > 0.05) %>% 
  dplyr::select(term)

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

## 選択したストップワードを除外
df_id_tokens_rm_stopword %<>% 
  anti_join(stopword_verb_original, by = "term") %>% 
  anti_join(stopword_adj_original, by = "term") %>% 
  anti_join(stopword_additional_original, by = "term")

## データの書き出し
write_csv(df_id_tokens_rm_stopword, "data/df-id-tokens-rm-stopword.csv")
