#-----------------------------------------------------------------------------#
# Script Name: 15-lda-model-infer.R                                           #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script infers LDA topic model by setting fixed optimal K.            #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Packages
pacman::p_load(tidyverse,   # for data manipulation
               topicmodels, # for LDA inference
               tidytext,    # to create document-term matrix
               writexl      # to write excel sheet
               )

# Data
## Token
tokens_finalized <- read_csv("data/tokens-05_rm-stpw-general.csv")
## Tweets
tweet_finalizing <- read_csv("data/tweet-03_cleansed.csv")

# Document-term matrix---------------------------------------------------------

# Remove tweets containing only 1 term
tokens_finalized %<>% 
  anti_join(tokens_finalized %>% 
              group_by(id_orig) %>% 
              summarise(n = n()) %>% 
              filter(n < 2) %>% 
              dplyr::select(id_orig),
            by = "id_orig")

# Export tweets finally included for LDA inference
tweet_finalizing %<>% 
  arrange(id_orig) %>% 
  inner_join(tokens_finalized %>% 
               dplyr::select(id_orig) %>% 
               arrange(id_orig) %>% 
               distinct(.keep_all = TRUE),
             by = "id_orig")

# Export final tweets
write_csv(tweet_finalizing, "data/tweet-04_finalized.csv")

# Create DTM
dtm_finalized <- tokens_finalized %>% 
  group_by(id_orig, term) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(id_orig) %>% 
  tidytext::cast_dtm(document = "id_orig",
                     term = "term",
                     value = "count")

# Inference LDA----------------------------------------------------------------

# The number of topics
K <- 30

# Model inference
topicModel <- LDA(dtm_finalized,
                  k = K,
                  method = "Gibbs",
                  control = list(alpha = 50 / K, 
                                 iter = 1000, 
                                 verbose = 25, 
                                 seed = 123))

# Extract data-----------------------------------------------------------------

# LDA outputs
tmResult <- topicmodels::posterior(topicModel)
str(tmResult)

theta <- tmResult$topics
beta <- tmResult$terms

# Export data
## term-topic distribution
write_csv(as_tibble(beta), "data/lda-output-01_topic-term.csv")
## topic-document distribution
write_csv(as_tibble(theta), "data/lda-output-02_doc-topic.csv")
## topic-document distribution with tweet information
theta %>% 
  as_tibble() %>% 
  cbind(tweet_finalizing) %>% 
  write_csv("data/lda-output-03_doc-topic-tweet.csv")

# Topic-term relation----------------------------------------------------------

# Table of terms contained in each topic
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
                       V16, ", ", V17, ", ", V18, ", ", V19, ", ", V20)) %>%
  dplyr::select(topic, terms) %>% 
  write_xlsx("table/TABLE_LDA-topic-term20.xlsx")

# English version term list
topicList %>% 
  mutate(id_pivot = row_number()) %>% 
  pivot_longer(cols = -id_pivot,
               names_to = "topic",
               values_to = "term") %>% 
  group_by(term) %>% 
  summarise(n = n()) %>% 
  mutate(term_en = "") %>% 
  write_xlsx("data-manual/term-topic-translating.xlsx")

term_en <- read_excel("data-manual/term-topic-translated.xlsx")

topicList_en <- topicList %>% 
  mutate(id_pivot = row_number()) %>% 
  pivot_longer(cols = -id_pivot,
               names_to = "topic",
               values_to = "term") %>% 
  left_join(term_en %>% 
              dplyr::select(term, term_en),
            by = "term") %>% 
  pivot_wider(names_from = topic,
              values_from = term)

topicList_t_en <- t(topicList_en) %>% 
  as.data.frame()
topicList_t$topic <- rownames(topicList_t_en)
topicList_table <- topicList_t_en %>% 
  as_tibble() %>% 
  mutate(terms = str_c(V1, ", ", V2, ", ", V3, ", ", V4, ", ", V5, ", ",
                       V6, ", ", V7, ", ", V8, ", ", V9, ", ", V10, ", ",
                       V11, ", ", V12, ", ", V13, ", ", V14, ", ", V15, ", ",
                       V16, ", ", V17, ", ", V18, ", ", V19, ", ", V20)) %>%
  dplyr::select(topic, terms) %>% 
  write_xlsx("table/TABLE_LDA-topic-term20_en.xlsx")
