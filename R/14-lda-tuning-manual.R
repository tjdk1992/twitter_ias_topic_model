#-----------------------------------------------------------------------------#
# Script Name: 14-lda-test-run.R                                              #
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

## Packages
library(tidyverse) # for data manipulation
library(tidytext) # to use cast_dtm function
library(textmineR) # to create DTM
library(ldatuning) # determine the proper number of topics inferred
library(tictoc) # to calculate ran time
library(hrbrthemes) # for visualization
library(doParallel)
library(scales)
library(topicmodels)

## Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

## Data
df_id_tokens_rm_stopword <- read_csv("data/df-id-tokens-rm-stopword.csv")

#------------------------------------------------------------------------------

# Determining the number of topics, K

## Create document term matrix
dtm_rm_stopword <- cast_dtm(df_id_tokens_rm_stopword,
                            document = "title", 
                            term = "term", 
                            value = "count")
## Remove tokens data to release memory
remove(df_id_tokens_rm_stopword)

#------------------------------------------------------------------------------

# ldatuningの結果を踏まえて25〜150 BY 25でトピックを推定してみる。
tic()
for (k in seq(25, 150, by = 25)) {
  topicModel <- LDA(dtm_rm_stopword,
                    k,
                    method = "Gibbs",
                    control = list(alpha = 1, 
                                   iter = 1000, 
                                   verbose = 25, 
                                   seed = 123))
  name_path <- str_c("data/k-manual-tuning/topiclist_stopword_general_rm_seed123-K",k ,".csv")
  as.data.frame(terms(topicModel, 20)) %>% 
    write_csv(name_path)
}
toc()

#------------------------------------------------------------------------------

# ldatuningの結果を踏まえて30〜150 BY 30でトピックを推定してみる。
tic()
# seq(30, 50, by = 2)
for (k in seq(32, 48, by = 2)) {
  topicModel <- LDA(dtm_rm_stopword,
                    k,
                    method = "Gibbs",
                    control = list(alpha = 1, 
                                   iter = 1000, 
                                   verbose = 25, 
                                   seed = 123))
  name_path <- str_c("data/k-manual-tuning/topiclist_stopword_general_rm_seed123-K",k ,".csv")
  as.data.frame(terms(topicModel, 20)) %>% 
    write_csv(name_path)
}
toc()

#------------------------------------------------------------------------------

# ldatuningの結果を踏まえて30〜150 BY 30でトピックを推定してみる。
tic()
# seq(30, 50, by = 2)
for (k in seq(29, 39, by = 2)) {
  topicModel <- LDA(dtm_rm_stopword,
                    k,
                    method = "Gibbs",
                    control = list(alpha = 1, 
                                   iter = 1000, 
                                   verbose = 25, 
                                   seed = 123))
  name_path <- str_c("data/k-manual-tuning/topiclist_stopword_general_rm_seed123-K",k ,".csv")
  as.data.frame(terms(topicModel, 20)) %>% 
    write_csv(name_path)
}
toc()

#------------------------------------------------------------------------------

# ldatuningの結果を踏まえて30〜150 BY 30でトピックを推定してみる。
tic()
for (k in c(seq(20, 28, by = 2), seq(33, 39, by = 2))) {
  topicModel <- LDA(dtm_rm_stopword,
                    k,
                    method = "Gibbs",
                    control = list(alpha = 1, 
                                   iter = 1000, 
                                   verbose = 25, 
                                   seed = 123))
  name_path <- str_c("data/k-manual-tuning/topiclist_stopword_general_rm_seed123-K",k ,".csv")
  as.data.frame(terms(topicModel, 20)) %>% 
    write_csv(name_path)
}
toc()

#------------------------------------------------------------------------------

# K = 30で乱数を変える。
tic()
# 123, 135, 159, 246, 369, 111, 222, 333, 444, 555
for (rn in c(159, 246, 369, 111, 222, 333, 444, 555)) {
  topicModel <- LDA(dtm_rm_stopword,
                    k = 30,
                    method = "Gibbs",
                    control = list(alpha = 1, 
                                   iter = 1000, 
                                   verbose = 25, 
                                   seed = rn))
  name_path <- str_c("data/k-manual-tuning/topiclist_stopword_general_rm_seed", rn,"-K30.csv")
  as.data.frame(terms(topicModel, 20)) %>% 
    write_csv(name_path)
}
toc()
