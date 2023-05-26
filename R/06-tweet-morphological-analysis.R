# R script for the analysis in:
# Tomojiri, D., Takaya, K. (2022) Quantitative assessment of relative topics
# and occurrence of invasive alien species in the Twitter.
# Submitted to "Conservation Biology"
#
# R Script 03: Preprocessing the tweet data
#
# Author: Daiki Tomojiri
#
# Outline:
## Step 1. Estimate the maximum number of tweeets / year

#------------------------------------------------------------------------------

# Setup

## Packages
library(tidyverse) # for data manipulation
library(readxl) # to read excel sheet
library(lubridate) # to address date data
library(magrittr) # to use pipe function
library(stringi) # to address Japanese characters
library(tictoc) # for calculation of time consumed in the processes
library(RMeCab) # to preprocess Japanese text data
library(purrr) 
library(hrbrthemes)

## Data
tweet_tidy <- read_csv("data/basic-ias-tweet.csv")
dat_ias_ja <- read_csv("data/basic-ias-info.csv")

#------------------------------------------------------------------------------

# Data check

## エンコーディングのチェック
guess_encoding("data/basic-ias-tweet.csv") # UTF-8

## 年別に取得できたツイート数を確認する
tweet_tidy %>% 
  group_by(year) %>% 
  summarise(n = n())
#------------------------------------------------------------------------------

# Step 1. 強制抽出ワード

## DFの作成（DFじゃないと形態素解析してくれない。）
df_ias <- tweet_tidy %>% 
  dplyr::select(id_orig, text) %>% 
  as.data.frame()
remove(tweet_tidy)

## 強制抽出ワードの処理
## 強制抽出する単語：
## 検索ワード(外来、侵入、移入、帰化)
## 検索外来種名
term_mask <- c(dat_ias_ja %>% 
  mutate(n_chr = nchar(KATAKANA)) %>% 
  arrange(desc(n_chr)) %>% 
  dplyr::select(KATAKANA) %>% 
  unlist(use.names = FALSE),
  "外来", "侵入", "移入", "帰化")


## 繰り返し処理により、検索名と生物名にマーカー
tic()
for (i in 1:length(term_mask)) {
  term_rm <- term_mask[i]
  df_ias <- mutate(df_ias, text = str_replace_all(text, term_rm, " "))
}
toc() # 2194.799 sec elapsed (2023-05-23)

#------------------------------------------------------------------------------

# Step 2. 形態素解析

## MeCabによる形態素解析で単語のリストを取得
tic() # コードの実行時間を計測するコマンド
df_id <- RMeCabDF(df_ias, "text", 1)
toc() # 1444.696 sec elapsed (2023-05-23)

## リストをトークンのDFに変換
tic()
df_id_tokens <- purrr::pmap_df(list(nv = df_id,
                                    title = df_ias$id),
                               function(nv, title){
                                 tibble(title = title,
                                        term = nv,
                                        hinshi = names(nv))
                               })
toc() # 1251.964 sec elapsed（2023-05-23）

## 重くならないように重いデータはremoveしておく
remove(df_id)

## 形態素解析から得られたトークンの書き出し
write_csv(df_id_tokens, "data/df-id-tokens-original.csv")
