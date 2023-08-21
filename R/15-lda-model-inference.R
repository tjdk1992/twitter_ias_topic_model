#-----------------------------------------------------------------------------#
# Script Name: 16-lda-model-infer.R                                           #
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
               magrittr,    # to overwrite data
               tidytext,    # to create document-term matrix
               writexl,      # to write excel sheet
               readxl,
               xtable
)

# Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

# Data
## Tokens
tokens_finalized <- read_csv("data/tokens-06_rm-stpw-original.csv")

## Tweets
tweet_finalizing <- read_csv("data/tweet-04_cleansed.csv")

# Data preparation ------------------------------------------------------------

# Check the IAS occurrence difference

# 元の数
count_original <- tweet_finalizing %>% 
  group_by(name_sp) %>% 
  summarise(n_original = n()) %>% 
  arrange(n_original)

# 単語数が足りなかったTweetの削除後
count_filtered <- tweet_finalizing %>% 
  inner_join(
    tokens_finalized %>% 
      anti_join(
        tokens_finalized %>% 
          group_by(id_cleansed) %>% 
          summarise(n = n()) %>% 
          filter(n < 5) %>% 
          dplyr::select(id_cleansed),
        by = "id_cleansed") %>% 
      distinct(id_cleansed, .keep_all = FALSE),
    by = "id_cleansed") %>% 
  group_by(name_sp) %>% 
  summarise(n_filtered = n()) %>% 
  arrange(n_filtered)

count_test <- count_original %>% 
  left_join(count_filtered, by = "name_sp") %>% 
  na.omit()

# 相関図と相関係数
v_cor <- cor(count_test$n_original, count_test$n_filtered)
count_test %>%
  ggplot(aes(x = n_original, y = n_filtered)) +
  geom_point() +
  geom_smooth(se = FALSE, colour = pal_orig[1]) +
  annotate(geom = "text", 
           x = 12500, y = 12500,
           label = str_c("r = ", round(v_cor, 4))) +
  theme_ipsum(base_size = 10)

# Save the plot
ggsave("fig/orig-filtered-count-correlation.png",
       units = "mm", width = 100, height = 100)
ggsave("fig/orig-filtered-count-correlation.eps", 
       units = "mm", width = 100, height = 100, device = cairo_ps)

# Document-term matrix---------------------------------------------------------

# Remove tweets containing only 1 term
tokens_finalized %<>% 
  anti_join(
    tokens_finalized %>% 
      group_by(id_cleansed) %>% 
      summarise(n = n()) %>% 
      filter(n < 5) %>% 
      dplyr::select(id_cleansed),
    by = "id_cleansed")

tokens_finalized %>% 
  dplyr::select(id_cleansed) %>% 
  distinct()

# Export tweets finally included for LDA inference
tweet_finalizing %<>% 
  arrange(id_cleansed) %>% 
  inner_join(
    tokens_finalized %>% 
      dplyr::select(id_cleansed) %>% 
      arrange(id_cleansed) %>% 
      distinct(.keep_all = TRUE),
    by = "id_cleansed") %>% 
  mutate(id_finalized = row_number())

# Export final tweets
write_csv(tweet_finalizing, "data/tweet-05_finalized.csv")

# Create DTM
dtm_finalized <- tokens_finalized %>% 
  group_by(id_cleansed, term) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(id_cleansed) %>% 
  tidytext::cast_dtm(document = "id_cleansed",
                     term = "term",
                     value = "count")

# Infer LDA model -------------------------------------------------------------

# The number of topics
K <- 25

# Model inference
topicModel <- LDA(dtm_finalized,
                  k = K,
                  method = "Gibbs",
                  control = list(alpha = 50 / K, 
                                 iter = 1000, 
                                 verbose = 25, 
                                 seed = 123))

# Export data -----------------------------------------------------------------

# Extract LDA posterior probability matrices
tmResult <- topicmodels::posterior(topicModel)

# term-topic distribution
write_csv(as_tibble(tmResult$terms), "data/lda-output-01_topic-term.csv")

# topic-document distribution (not being use in this study)
write_csv(as_tibble(tmResult$topics), "data/lda-output-02_doc-topic.csv")

# 1 topic / 1 document distribution (being used in this study) with tweet info
tweet_finalizing %>% 
  mutate(topic = topicmodels::topics(topicModel),
         nam_tp = if_else(topic <= 9, "0", ""),
         topic = str_c("TP", nam_tp, topic)) %>% 
  dplyr::select(-nam_tp) %>% 
  write_csv("data/lda-output-03_doc-topic-tweet.csv")

# Table of terms contained in each topic
topicModel %>% 
  topicmodels::terms(50) %>% 
  as.data.frame() %>% 
  write_xlsx("data/lda-output-04_topic-top50-terms.xlsx")
