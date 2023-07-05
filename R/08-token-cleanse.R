#-----------------------------------------------------------------------------#
# Script Name: 08-token-cleanse.R                                             #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script cleanse tokens by removing unrelated parts of documents.      #
#-----------------------------------------------------------------------------#


# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Package
pacman::p_load(tidyverse,
               magrittr,
               stringi,
               zipangu
               )

# Data
token_cleansing <- read_csv("data/tokens-01_original.csv")

# Automatic cleaning ----------------------------------------------------------

# 品詞の選択
token_cleansing %<>% 
  filter(hinshi == "名詞" |
           hinshi == "動詞" |
           hinshi == "形容詞")

# 日本語を正規化する。
token_cleansing %<>% 
  mutate(term = stri_trans_general(term, "Halfwidth-Fullwidth"),
         term = str_jnormalize(term))

# 細々した不用品のクリーニング
token_cleansing %<>% 
  mutate(
    # 半角にしておく
    term = stri_trans_general(term, "Fullwidth-Halfwidth"),
    # 記号の除外
    term = str_replace_all(term, "[[:punct:]]", ""),
    # 数字の除外
    term = str_replace_all(term, "[[:digit:]]", ""),
    # 英語の除外
    term = str_replace_all(term, "[[:lower:]]", ""),
    term = str_replace_all(term, "[[:upper:]]", ""),
    # 空白、タブ、改行の除外
    term = str_replace_all(term, "[[:space:]]", ""),
    # 絵文字の除外
    term = stri_replace_all(term, regex = "\\p{Emoji}", "")) %>%
  filter(!(term == "")) %>% 
  filter(!(term == " ")) %>% 
  filter(!(term == "　"))

# 日本語を扱う上で半角カタカナが邪魔なので全角に変換しておく
token_cleansing %<>% 
  mutate(term = stri_trans_general(term, "Halfwidth-Fullwidth"))

# コーパスのチェック①（出現頻度順で上位5000単語をチェック）
viewTop5000(token_cleansing)

# 先頭が"ー"のものを削除
token_cleansing %<>% mutate(term = str_replace_all(term, "^ー*", ""))

# Corpus check
viewTop5000(token_cleansing)

# Manual cleaning (transformative) --------------------------------------------

# 出現頻度が1のものは必ずノイズになるので除外する。
token_cleansing %<>% 
  anti_join(token_cleansing %>% 
              group_by(term) %>% 
              summarise(n = n()) %>% 
              filter(n == 1) %>% 
              dplyr::select(term),
            by = "term")

# Corpus check
viewTop5000(token_cleansing)

## 記号は意味の解釈が困難なので除外
token_cleansing %<>% 
  anti_join(token_cleansing %>% 
              group_by(term) %>% 
              summarise(n = n()) %>% 
              arrange(term) %>% 
              head(407) %>% 
              dplyr::select(term), 
            by = "term")

## コーパスのチェック③
viewTop5000(token_cleansing)

## 漢字以外の1文字のtermは除外する。
# all type of kana
kana_all <- unique(c(kana(type = "hira", dakuon = TRUE),
                     kana(type = "hira", handakuon = TRUE),
                     kana(type = "hira", kogaki = TRUE),
                     kana(type = "kata", dakuon = TRUE),
                     kana(type = "kata", handakuon = TRUE),
                     kana(type = "kata", kogaki = TRUE)))
# 全角の濁音がついてるやつ
kana_all <- c(kana_all, str_c(kana_all, "゛"), str_c(kana_all, "゙"))

# 漢字以外の1文字のトークンは除外する。
for (i in 1:length(kana_all)) {
  token_cleansing %<>% 
    filter(term != kana_all[i])
}

# コーパスのチェック
viewTop5000(token_cleansing)

# 繰り返しは鳴き声だったりして意味がないので除外
# チェック
filter(token_cleansing, str_detect(term, "(.)\\1{2,}")) %>% pull(term)
filter(token_cleansing, str_detect(term, "(..)\\1{2,}")) %>% pull(term)
filter(token_cleansing, str_detect(term, "(...)\\1{2,}")) %>% pull(term)
filter(token_cleansing, str_detect(term, "(....)\\1{2,}")) %>% pull(term)

# 2回の繰り返しは意味が取れる可能性があるのでここでは残す（例：オオコウモリ）。
token_cleansing %<>% 
  filter(!(str_detect(term, "(.)\\1{2,}"))) %>% 
  filter(!(str_detect(term, "(..)\\1{2,}"))) %>%
  filter(!(str_detect(term, "(...)\\1{2,}"))) %>%
  filter(!(str_detect(term, "(....)\\1{2,}")))

# コーパスのチェック④
viewTop5000(token_cleansing)

# クリーニングが完了したデータを書き出し
write_csv(token_cleansing, "data/tokens-02_cleansed.csv")
