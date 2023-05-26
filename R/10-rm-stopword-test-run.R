# R script for the analysis in:
# Tomojiri, D., Takaya, K. (2022) Quantitative assessment of relative topics
# and occurrence of invasive alien species in the Twitter.
# Submitted to "Conservation Biology"
#
# R Script 08: Testing combination of stopwords
#
# Author: Daiki Tomojiri
#
# Outline:
## Step 1. Estimate the maximum number of tweeets / year

#------------------------------------------------------------------------------

# Setup

## Packages
library(tidytext)
library(topicmodels)

## Data
df_id_tokens_rm_stopword <- read_csv("data/df-id-tokens-rm-stopword.csv")

###############################################################################

## original stopwordの除外をしないパターン

# 単語の出現頻度の算出
df_rm_stopword_count <- df_id_tokens_rm_stopword %>% 
  group_by(title, term) %>% 
  summarise(count = n()) %>% 
  ungroup()

# DTM (document-term matrix)の作成
dtm_stopword_not_rm <- cast_dtm(df_rm_stopword_count,
                                document = "title", 
                                term = "term", 
                                value = "count")

# メモリ消費しないようにdtm用のdfは消しておく
rm(df_id_tokens_rm_stopword, df_rm_stopword_count)

# LDAモデルの推定
# トピック数
K <- 20
# 推定
tic()
set.seed(135)
topicModel <- LDA(dtm_stopword_not_rm, K, method = "Gibbs", 
                  control = list(alpha = 1, iter = 1000, verbose = 25))
toc()

# 各トピックを構成する単語のリスト
topicList_stopword_not_rm <- as.data.frame(terms(topicModel, 20))
View(topicList_stopword_not_rm)
topicList_stopword_not_rm %>% 
  write_csv("data/lda-test-run/topiclist_stopword_not_rm.csv")

###############################################################################
# 2023-01-17 (TUE)
###############################################################################

## original stopwordの除外を除外（freq_doc > 0.1）

# LDAモデルの推定
# トピック数
K <- 20
# 推定
tic()
set.seed(135)
topicModel <- LDA(dtm_rm_stopword, K, method = "Gibbs", 
                  control = list(alpha = 1, iter = 1000, verbose = 25))
toc()

# 各トピックを構成する単語のリスト
topicList_stopword_not_rm <- as.data.frame(terms(topicModel, 20))
topicList_stopword_not_rm %>% 
  write_csv("data/lda-test-run/topiclist_r135_rm_noun-verb005-adj005_K20.csv")

###############################################################################

K <- 30
# 推定
tic()
set.seed(135)
topicModel <- LDA(dtm_rm_stopword, K, method = "Gibbs", 
                  control = list(alpha = 1, iter = 1000, verbose = 25))
toc()

# 各トピックを構成する単語のリスト
topicList_stopword_not_rm <- as.data.frame(terms(topicModel, 20))
topicList_stopword_not_rm %>% 
  write_csv("data/lda-test-run/topiclist_r135_rm_noun-verb005-adj005_K30.csv")

###############################################################################

K <- 40
# 推定
tic()
set.seed(135)
topicModel <- LDA(dtm_rm_stopword, K, method = "Gibbs", 
                  control = list(alpha = 1, iter = 1000, verbose = 25))
toc()

# 各トピックを構成する単語のリスト
topicList_stopword_not_rm <- as.data.frame(terms(topicModel, 20))
topicList_stopword_not_rm %>% 
  write_csv("data/lda-test-run/topiclist_r135_rm_noun-verb005-adj005_K40.csv")

###############################################################################

K <- 50
# 推定
tic()
set.seed(135)
topicModel <- LDA(dtm_rm_stopword, K, method = "Gibbs", 
                  control = list(alpha = 1, iter = 1000, verbose = 25))
toc()

# 各トピックを構成する単語のリスト
topicList_stopword_not_rm <- as.data.frame(terms(topicModel, 20))
topicList_stopword_not_rm %>% 
  write_csv("data/lda-test-run/topiclist_r135_rm_noun-verb005-adj005_K50.csv")

###############################################################################
# 2023-01-18 (WED)
###############################################################################

## original stopwordの除外を除外（freq_doc > 0.1）

# LDAモデルの推定
# トピック数
K <- 20
# 推定
tic()
set.seed(135)
topicModel <- LDA(dtm_rm_stopword, K, method = "Gibbs", 
                  control = list(alpha = 1, iter = 1000, verbose = 25))
toc()

# 各トピックを構成する単語のリスト
topicList_stopword_not_rm <- as.data.frame(terms(topicModel, 20))
topicList_stopword_not_rm %>% 
  write_csv("data/lda-test-run/topiclist_r135_rm_noun-verb005-adj005-additional_K20.csv")

###############################################################################

K <- 30
# 推定
tic()
set.seed(135)
topicModel <- LDA(dtm_rm_stopword, K, method = "Gibbs", 
                  control = list(alpha = 1, iter = 1000, verbose = 25))
toc()

# 各トピックを構成する単語のリスト
topicList_stopword_not_rm <- as.data.frame(terms(topicModel, 20))
topicList_stopword_not_rm %>% 
  write_csv("data/lda-test-run/topiclist_r135_rm_noun-verb005-adj005-additional_K30.csv")

###############################################################################
