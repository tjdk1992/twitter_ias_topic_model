#-----------------------------------------------------------------------------#
# Script Name: 15-lda-test-run.R                                              #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script explores optimal number of topics by checking multiple        #
# patterns of combination of the topic number K and random seeds.             #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Package
pacman::p_load(tidyverse, # for data manipulation
               tidytext, # to use cast_dtm function
               textmineR, # to create DTM
               ldatuning, # determine the proper number of topics inferred
               tictoc, # to calculate ran time
               hrbrthemes, # for visualization
               doParallel,
               scales,
               topicmodels
               )

# Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

# Data
tokens_rm_stpw_original <- read_csv("data/tokens-06_rm-stpw-original.csv")

# Run ldatuning ---------------------------------------------------------------

# DTMの作成
dtm_rm_stpw_original <- tokens_rm_stpw_original %>% 
  anti_join(tokens_rm_stpw_original %>% 
              group_by(id_cleansed) %>% 
              summarise(n = n()) %>% 
              filter(n == 1) %>% 
              dplyr::select(id_cleansed),
            by = "id_cleansed") %>% 
  group_by(id_cleansed, term) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  tidytext::cast_dtm(document = "id_cleansed",
                     term = "term",
                     value = "count")

# Topic modelling with different K --------------------------------------------

# ldatuningの結果を踏まえて10〜60 BY 10でトピックを推定してみる。
for (K in seq(10, 60, by = 5)) {
  topicModel <- LDA(dtm_rm_stpw_original,
                    k = K,
                    method = "Gibbs",
                    control = list(alpha = 50/K, 
                                   iter = 1000, 
                                   verbose = 25, 
                                   seed = 123))
  name_path <- str_c("data-manual/lda-manual-tuning/TP-manual-tuning-K", K,".csv")
  as.data.frame(terms(topicModel, 20)) %>% 
    write_csv(name_path)
}

# ldatuningの結果を踏まえて10〜60 BY 10でトピックを推定してみる。
for (K in seq(21, 29, by = 1)) {
  topicModel <- LDA(dtm_rm_stpw_original,
                    k = K,
                    method = "Gibbs",
                    control = list(alpha = 50/K, 
                                   iter = 1000, 
                                   verbose = 25, 
                                   seed = 123))
  name_path <- str_c("data-manual/lda-manual-tuning/TP-manual-tuning-K", K,".csv")
  as.data.frame(terms(topicModel, 20)) %>% 
    write_csv(name_path)
}

# K = 30で乱数を変える。
for (rn in c(123, 135, 159, 246, 369, 111, 222, 333, 444, 555)) {
  topicModel <- LDA(dtm_rm_stopword,
                    k = 30,
                    method = "Gibbs",
                    control = list(alpha = 1, 
                                   iter = 1000, 
                                   verbose = 25, 
                                   seed = rn))
  name_path <- str_c("data-manual/lda-manual-tuning/TP-manual-tuning-K25_seed", rn,".csv")
  as.data.frame(terms(topicModel, 20)) %>% 
    write_csv(name_path)
}
