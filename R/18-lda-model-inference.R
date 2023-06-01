#-----------------------------------------------------------------------------#
# Script Name: 02-tweet-retrieval.R
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script screening and cleanse tweet                                   #
#                                                                             #
#-----------------------------------------------------------------------------#


# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

## Packages
library(tidyverse) # for data manipulation
library(topicmodels) # for LDA inference™
library(tidytext) # to use cast_dtm function
library(writexl)
library(tictoc)

## Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

## Data
df_id_tokens_rm_stopword <- read_csv("data/df-id-tokens-rm-stopword.csv")
df_id_tokens_rm_stopword %<>% 
  filter(term != "好き") %>% 
  filter(term != "嫌い") %>% 
  filter(term != "良い") %>% 
  filter(term != "悪い")
#------------------------------------------------------------------------------

# Document-term matrix

## Create document term matrix
dtm_rm_stopword <- df_id_tokens_rm_stopword %>% 
  arrange(title) %>% 
  cast_dtm(document = "title", 
           term = "term", 
           value = "count")

## Remove tokens data to release memory
remove(df_id_tokens_rm_stopword)

#------------------------------------------------------------------------------

# infer lda topic model

K <- 30
tic()
topicModel <- LDA(dtm_rm_stopword,
                  k = K,
                  method = "Gibbs",
                  control = list(alpha = 1, 
                                 iter = 1000, 
                                 verbose = 25, 
                                 seed = 123))
toc()

topicList <- as.data.frame(terms(topicModel, 20)) # label manually
write_xlsx(topicList, "table/TABLE_1_20230430_B_alpha1.xlsx")

## Extract data
tmResult <- topicmodels::posterior(topicModel)
str(tmResult)
theta <- tmResult$topics
beta <- tmResult$terms

  ## Export data
### topic-document distribution
write_csv(as_tibble(theta), "data/result-lda-inference-topic.csv")
### term-topic distribution
write_csv(as_tibble(beta), "data/result-lda-inference-term.csv")

#------------------------------------------------------------------------------

# Topic listの英訳

## 書き出し
topicList <- as.data.frame(terms(topicModel, 20)) # label manually
class(topicList)
topicList_t <- t(topicList) %>% 
  as.data.frame()
topicList_t$topic <- rownames(topicList_t)
topicList_table <- topicList_t %>% 
  as_tibble() %>% 
  mutate(terms = str_c(V1, ", ", V2, ", ", V3, ", ", V4, ", ", V5, ", ",
                       V6, ", ", V7, ", ", V8, ", ", V9, ", ", V10, ", ",
                       V11, ", ", V12, ", ", V13, ", ", V14, ", ", V15, ", ",
                       V16, ", ", V17, ", ", V18, ", ", V19, ", ", V20, ", ")) %>%
  dplyr::select(topic, terms)

write_xlsx(topicList_table, "table/TABLE_1_20230422.xlsx")

write_xlsx(topicList, "table/TABLE_1_20230422.xlsx")
