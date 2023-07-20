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
               readxl
)

# Data
## Tokens
tokens_finalized <- read_csv("data/tokens-06_rm-stpw-original.csv")

## Tweets
tweet_finalizing <- read_csv("data/tweet-04_cleansed.csv")

#------------------------------------------------------------------------------

# Check the IAS occurrence difference

# 元の数
count_original <- tweet_finalizing %>% 
  group_by(name_sp) %>% 
  summarise(n_original = n()) %>% 
  arrange(n_original)

# 単語数が足りなかったTweetの削除後
count_filtered <- tweet_finalizing %>% 
  inner_join(tokens_finalized %>% 
               anti_join(tokens_finalized %>% 
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
plot(count_test$n_original, count_test$n_filtered)
cor(count_test$n_original, count_test$n_filtered)

# Document-term matrix---------------------------------------------------------

# Remove tweets containing only 1 term
tokens_finalized %<>% 
  anti_join(tokens_finalized %>% 
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
  inner_join(tokens_finalized %>% 
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

# Inference LDA----------------------------------------------------------------

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

# Extract data-----------------------------------------------------------------

# LDA outputs
tmResult <- topicmodels::posterior(topicModel)

theta <- tmResult$topics
beta <- tmResult$terms

# Export data
## term-topic distribution
write_csv(as_tibble(beta), "data/lda-output-01_topic-term.csv")
## topic-document distribution
write_csv(as_tibble(theta), "data/lda-output-02_doc-topic.csv")
## topic-document distribution with tweet information
df_theta <- as.data.frame(theta)
colnames(df_theta) <- c(str_c("TP", "0", seq(9)), str_c("TP", 10:25))
df_theta %<>% 
  mutate(topic = topics(topicModel)) %>% 
  cbind(tweet_finalizing)
df_theta %<>% 
  mutate(topic = if_else(topic < 10, 
                         str_c("TP0", topic),
                         str_c("TP", topic)))
## write
write_csv(df_theta, "data/lda-output-03_doc-topic-tweet.csv")

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
  write_xlsx("table/table-lda-topic-term20.xlsx")

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
  write_xlsx("table/table-lda-topic-term20_en.xlsx")

# Validation-------------------------------------------------------------------

# Data
theta <- read_csv("data/lda-output-03_doc-topic-tweet.csv")

set.seed(123)
val_test <- theta %>% 
  sample_n(10) %>% 
  dplyr::select(id_cleansed, name_ja, text)

dat_test <- theta %>% 
  inner_join(val_test %>% 
               dplyr::select(id_cleansed), 
             by = "id_cleansed")

dat_test %>% 
  pivot_longer(cols = TP01:TP25,
               names_to = "no_topic",
               values_to = "prob") %>% 
  group_by(no_topic, id_cleansed) %>% 
  summarise(prob_mean = mean(prob)) %>% 
  ggplot(aes(x = no_topic, y = prob_mean)) +
  geom_bar(stat = "identity") +
  facet_wrap(. ~ id_cleansed, ncol = 2) +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  theme(axis.text.x = element_text(angle = 90))

View(val_test)

dat_test %>% 
  pivot_longer(cols = TP01:TP25,
               names_to = "no_topic",
               values_to = "prob") %>% 
  group_by(no_topic, id_cleansed) %>% 
  summarise(prob_mean = mean(prob)) %>% 
  filter(id_cleansed == 261503) %>% 
  group_by(prob_mean) %>% 
  summarise(n = n())

theta %>% 
  sample_n(100) %>% 
  dplyr::select(id_cleansed, name_ja, topic, text) %>% 
  write_xlsx("test.xlsx")

theta %>% 
  dplyr::select(-topic) %>% 
  filter(id_cleansed == 223062) %>% 
  pivot_longer(TP01:TP25, names_to = "topic", values_to = "prob") %>% 
  group_by(prob) %>% 
  summarise(n = n())

theta %>% 
  filter(id_cleansed == 223062) %>% 
  pull(text)
theta %>% 
  dplyr::select(-topic) %>% 
  filter(id_cleansed == 223062) %>% 
  pivot_longer(TP01:TP25, names_to = "topic", values_to = "prob") %>% 
  arrange(desc(prob))
  group_by(prob) %>% 
  summarise(n = n())
