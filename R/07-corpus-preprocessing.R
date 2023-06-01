# R script for the analysis in:
# Tomojiri, D., Takaya, K. (2022) Quantitative assessment of relative topics
# and occurrence of invasive alien species in the Twitter.
# Submitted to "Conservation Biology"
#
# R Script 06: Preprocessing tokens for LDA inference
#
# Author: Daiki Tomojiri
#
# Outline:
## Step 1. Estimate the maximum number of tweeets / year

# Setup -----------------------------------------------------------------------

## Packages
library(tidyverse) # for data manipulation
library(readxl) # to read excel sheet
library(lubridate) # to address date data
library(magrittr) # to use pipe function
library(stringi) # to address Japanese characters
library(zipangu)
library(tictoc) # for calculation of time consumed in the processes
library(RMeCab) # to preprocess Japanese text data
library(purrr) 
library(hrbrthemes)

## データの読み込み
df_id_tokens <- read_csv("data/df-id-tokens-original.csv")

# Automatic cleaning ----------------------------------------------------------

# 品詞の絞り込み（動詞、形容詞、名詞のみ抽出）
df_id_tokens %<>% 
  filter(hinshi == "動詞" | hinshi == "形容詞" | hinshi == "名詞")

# 細々したクリーニング
df_id_tokens %<>% 
  mutate(
    # 記号を除去したいので半角になるものはしておく
    term = stri_trans_general(term, "Fullwidth-Halfwidth"),
    term = str_replace_all(term, "[[:punct:]]", ""), # 半角記号の除去
    term = str_replace_all(term, "[[:number:]]", ""), # 半角数字の除去
    term = str_replace_all(term, "[[:lower:]]", ""), # 半角英語（小文字）
    term = str_replace_all(term, "[[:upper:]]", ""), # 半角英語（大文字） 
    term = str_replace_all(term, "\\^", ""), # 顔文字の目"https?://.*[a-zA-Z0-9]"
    term = str_replace_all(term, "https?://.*[a-zA-Z0-9]", ""), # URL
    term = stri_replace_all(term, regex = "\\p{Emoji}", ""), # 絵文字
    term = str_replace_all(term, "~", ""),
    # 日本語を扱うので全角になるものは戻しておく
    term = stri_trans_general(term, "Halfwidth-Fullwidth")
  ) %>% #ニョロニョロ
  filter(!(term == "")) %>% 
  filter(!(term == "  ")) %>% 
  filter(!(term == " ")) %>% 
  filter(!(term == "　　")) %>% 
  filter(!(term == "　"))

# Manual cleaning (transformative) --------------------------------------------

# コーパスの上位5000をチェックするための関数
viewTop5000 <- function(df_tokens) {
  df_tokens %>% 
    group_by(term) %>% 
    summarise(n = n()) %>% 
    arrange(term) %>% 
    head(5000) %>% 
    View()
}

## コーパスのチェック①（出現頻度順で上位5000単語をチェック）
viewTop5000(df_id_tokens) # 先頭に"ー"がついているやつらがいる。

# 先頭がーであるものを削除
## 最も文字数が多いterm
max_nchar <- max(nchar(df_id_tokens$term))

## 先頭がーのものを修正
for (i in 1:max_nchar) {
  t_bar <- rep("ー", i)
  bars <- paste(t_bar, collapse = "")
  repl <- str_c("^", bars)
  df_id_tokens %<>% 
    mutate(term = str_replace(term, repl, ""))
}

rm(bars, t_bar, repl)

## 生じた空文字""を削除
df_id_tokens %<>% filter(!(term == ""))

# Manual cleaning (意味不明な単語の削除) --------------------------------------

# 出現頻度が1のものは必ずノイズになるので除外する。
df_id_tokens %<>% 
  anti_join(df_id_tokens %>% 
              group_by(term) %>% 
              summarise(n = n()) %>% 
              filter(n == 1) %>% 
              dplyr::select(term),
            by = "term")

# コーパスのチェック②
viewTop5000(df_id_tokens) # 1223までは記号

# 記号は意味の推定が困難なので除外する。
df_id_tokens %<>% 
  anti_join(df_id_tokens %>% 
              group_by(term) %>% 
              summarise(n = n()) %>% 
              arrange(term) %>% 
              head(530) %>% 
              dplyr::select(term), 
            by = "term")

## コーパスのチェック④
viewTop5000(df_id_tokens)

# all type of kana
kana_all <- unique(c(kana(type = "hira", dakuon = TRUE),
                     kana(type = "hira", handakuon = TRUE),
                     kana(type = "hira", kogaki = TRUE),
                     kana(type = "kata", dakuon = TRUE),
                     kana(type = "kata", handakuon = TRUE),
                     kana(type = "kata", kogaki = TRUE)))
# 漢字以外の1文字のトークンは除外する。
for (i in 1:length(kana_all)) {
  df_id_tokens %<>% 
    filter(term != kana_all[i])
}

## コーパスのチェック④
viewTop5000(df_id_tokens)

# 全角の濁音がついてるやつがいる
kana_all2 <- c(str_c(kana_all, "゛"), str_c(kana_all, "゙"))
for (i in 1:length(kana_all2)) {
  df_id_tokens %<>% 
    filter(term != kana_all2[i])
}

## コーパスのチェック④
viewTop5000(df_id_tokens)

## 3回以上の繰り返しは鳴き声だったりして意味がないので除外
## 2回の繰り返しは意味が取れる可能性があるのでここでは残す（例：オオコウモリ）。
kana_all3 <- c(kana_all, "ー") # ーも繰り返すと意味不明なので除外
for (i in 1:length(kana_all3)) {
  pattern <- str_c(kana_all3[i], "{3,}")
  df_id_tokens %<>% 
    filter(!(str_detect(term, pattern = pattern)))
}

# コーパスのチェック④
viewTop5000(df_id_tokens)

# 2回の繰り返しを検索（manualで不明・理解可能を分ける）
max_nchar <- max(nchar(df_id_tokens$term))
multiple_chr <- data.frame()
for (i in 1:max_nchar) {
  pattern = str_c("(.+)\\1{", i, ",}")
  t_multiple_chr <- df_id_tokens %>% 
    filter(str_detect(term, pattern = pattern))
  if (nrow(t_multiple_chr) == 0) break
  multiple_chr <- rbind(multiple_chr, t_multiple_chr)
}

# 書き出して不明じゃないものをチェック
multiple_chr_summary <- multiple_chr %>% 
  group_by(term) %>% 
  summarise(n = n())

# 確実に意味不明そうなのは2文字以上の3回繰り返し以上かな…
max_nchar <- max(nchar(multiple_chr$term))
multiple_chr_check <- data.frame()
for (i in 1:max_nchar) {
  pattern = str_c("(.+)\\1{", i, ",}")
  t_multiple_chr_check <- multiple_chr_summary %>% 
    filter(str_detect(term, pattern = pattern)) %>% 
    mutate(pattern = pattern)
  if (nrow(t_multiple_chr_check) == 0) break
  multiple_chr_check <- rbind(multiple_chr_check, t_multiple_chr_check)
}
(multiple_chr_rm <- multiple_chr_check %>% 
  separate(pattern, sep = "{", into = c("a", "b")) %>% 
  transmute(term,
            replic = str_replace_all(b, "\\D", "")) %>% 
  filter(replic >= 2) %>% # 2以上は意味不明
  dplyr::select(term) # %>% 
  # unlist(use.names = FALSE))
)
# 除外
df_id_tokens %<>% 
  anti_join(multiple_chr_rm, by = "term")
rm(multiple_chr, multiple_chr_check, multiple_chr_rm, 
   multiple_chr_summary, t_multiple_chr, t_multiple_chr_check)
# クリーニングが完了したデータを書き出し
write_csv(df_id_tokens, "data/df-id-tokens-preprocessed.csv")
