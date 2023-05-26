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
df_id_tokens <- read_csv("data/df-id-tokens-preprocessed.csv")

# 出現頻度による単語の除外 ----------------------------------------------------

# 出現頻度のチェック
N_doc <- length(unique(df_id_tokens$title))
tokens_summary_check <- df_id_tokens %>% 
  group_by(term) %>% 
  summarise(n = n(), freq = n / N_doc * 100)

# 出現頻度の高い単語を確認
arrange(filter(tokens_summary_check, freq > 50), desc(freq))
arrange(filter(tokens_summary_check, freq > 25), desc(freq))
arrange(filter(tokens_summary_check, freq > 20), desc(freq)) # この辺かな
arrange(filter(tokens_summary_check, freq > 10), desc(freq))
arrange(filter(tokens_summary_check, freq > 5), desc(freq))

# 出現頻度の低い単語を確認
arrange(filter(tokens_summary_check, freq < 0.01), desc(freq))
arrange(filter(tokens_summary_check, freq < 0.05), desc(freq))
arrange(filter(tokens_summary_check, freq < 0.001), desc(freq)) # この辺かな
arrange(filter(tokens_summary_check, freq < 0.0005), desc(freq))
arrange(filter(tokens_summary_check, freq < 0.0006), desc(freq))
arrange(filter(tokens_summary_check, freq < 0.0007), desc(freq))
arrange(filter(tokens_summary_check, freq < 0.0008), desc(freq))
arrange(filter(tokens_summary_check, freq < 0.0009), desc(freq))


# 出現頻度の低い単語を除外する（n < 5）。
df_id_tokens %<>% 
  anti_join(tokens_summary_check %>% 
              filter(freq > 20 | freq < 0.001) %>% 
              dplyr::select(term), 
            by = "term")

rm(tokens_summary_check)

# 前処理が完了したデータの書き出し
write_csv(df_id_tokens, "data/df-id-tokens-cutted.csv")
