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

# Packages
pacman::p_load(tidyverse,
               writexl,
               readxl)

# Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

# Data
token_rm_stpw_original <- read_csv("data/tokens-05_rm-stpw-general.csv")
token_ias <- read_csv("data/tokens-01_original.csv")
#------------------------------------------------------------------------------

# Original stopwordsの除外（名詞）

## ストップワード抽出用の単語リストを作成
## どの程度がいいんかな…
## 実際に分析に含まれたツイート数の取得
len_tweet <- nrow(token_rm_stpw_original %>% 
                    distinct(.keep_all = TRUE) %>% 
                    dplyr::select(id_cleansed) %>% 
                    distinct()) # *0.001

# 分布はどんな感じ？
token_rm_stpw_original %>% 
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

# 分布を可視化してみる
token_rm_stpw_original %>% 
  distinct(.keep_all = TRUE) %>% 
  group_by(term) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  mutate(freq_doc = freq / len_tweet * 100) %>% 
  filter(freq_doc > 0.5) %>% # 5%以上
  ggplot(aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text = element_text(family = "HiraKakuPro-W3",
                                 size = 8,
                                 angle = 90))

# トップ2854位までの単語に含まれる名詞
token_rm_stpw_original %>% 
  distinct(.keep_all = TRUE) %>% 
  group_by(term) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  mutate(freq_doc = freq / len_tweet * 100) %>% 
  filter(freq_doc > 0.1) %>%
  left_join(token_ias %>% 
              dplyr::select(-id_cleansed) %>% 
              distinct(.keep_all = TRUE), by = "term") %>% 
  filter(hinshi == "名詞") %>% # 動詞と形容詞は自動でジャッジする
  mutate(judge = "") %>% 
  write_xlsx("data-manual/token-rm-stpw-checking.xlsx")

# Check the selected stop words
stopword_noun_original <- 
  read_excel("data-manual/token-rm-stpw-checked.xlsx", sheet = 1)

# Summarise
stopword_noun_original %>% 
  group_by(judge, sub) %>% 
  summarise(n = n()) %>% 
  as.data.frame()

# Pattern VXIII: lda-test-runの結果、決定
token_rm_stpw_original %<>% 
  anti_join(stopword_noun_original %>% 
              filter(judge == "general" |
                       judge == "unclear" |
                       sub == "searched" |
                       sub == "lower" |
                       sub == "middle" |
                       sub == "upper" |
                       sub == "top" |
                       judge == "place") %>% 
              dplyr::select(term),
            by = "term")

# データの書き出し
write_csv(token_rm_stpw_original, "data/tokens-06_rm-stpw-original.csv")
