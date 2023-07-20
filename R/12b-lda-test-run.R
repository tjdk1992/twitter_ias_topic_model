#-----------------------------------------------------------------------------#
# Script Name: 13-lda-test-run.R                                              #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script is for test run checking validity of the original stop words. #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Package
pacman::p_load(tidyverse,
               magrittr,
               readxl,
               tidytext,
               topicmodels)

# Data
tokens_rm_stpw_basic <- read_csv("data/tokens-04_rm-stpw-basic.csv")
tokens_rm_stpw_general <- read_csv("data/tokens-05_rm-stpw-general.csv")
tokens_rm_stpw_original <- read_csv("data/tokens-06_rm-stpw-original.csv")

#------------------------------------------------------------------------------

# DTMの作成
dtm_rm_stpw_basic <- makeDTMatrix(tokens_rm_stpw_basic)
dtm_rm_stpw_general <- makeDTMatrix(tokens_rm_stpw_general)
dtm_rm_stpw_original <- makeDTMatrix(tokens_rm_stpw_original)

#------------------------------------------------------------------------------

# 2023-05-29
runLDAtest(dtm_rm_stpw_basic, file_name = "TP-list-rm-stpw-basic")
runLDAtest(dtm_rm_stpw_general, file_name = "TP-list-rm-stpw-general")
runLDAtest(dtm_rm_stpw_original, file_name = "TP-list-rm-stpw-original")

# Trial and error of original stop words---------------------------------------
# K = 30でチェックしてみる。

token_rm_stpw_original <- read_csv("data/tokens-05_rm-stpw-general.csv")
stopword_noun_original <- 
  read_excel("data-manual/token-rm-stpw-checked.xlsx", sheet = 1)
stopword_noun_original %<>% filter(!(is.na(judge)))
stopword_noun_original %>% 
  group_by(judge, sub) %>% 
  summarise(n = n()) %>% 
  as.data.frame()

## Pattern I:
dtm_rm_stpw_original <- token_rm_stpw_original %>% 
  anti_join(stopword_noun_original %>% 
              filter(judge == "general" |
                       judge == "unclear") %>% 
              dplyr::select(term),
            by = "term") %>%
  makeDTMatrix()

runLDAtest(dtm_rm_stpw_original, K_from = 30, K_to = 30, K_by = 30,
           file_name = "TP-list-rm-stpw-original-I")

## Pattern II: 
dtm_rm_stpw_original <- token_rm_stpw_original %>% 
  anti_join(stopword_noun_original %>% 
              filter(judge == "general" |
                       judge == "unclear" |
                       sub == "searched") %>% 
              dplyr::select(term),
            by = "term") %>%
  makeDTMatrix()

runLDAtest(dtm_rm_stpw_original, K_from = 30, K_to = 30, K_by = 30,
           file_name = "TP-list-rm-stpw-original-II")

## Pattern III: 
dtm_rm_stpw_original <- token_rm_stpw_original %>% 
  anti_join(stopword_noun_original %>% 
              filter(judge == "general" |
                       judge == "unclear" |
                       sub == "searched" |
                       sub == "lower" |
                       sub == "middle" |
                       sub == "upper") %>% 
              dplyr::select(term),
            by = "term") %>%
  makeDTMatrix()

runLDAtest(dtm_rm_stpw_original, K_from = 30, K_to = 30, K_by = 30,
           file_name = "TP-list-rm-stpw-original-III")

## Pattern IV: 
dtm_rm_stpw_original <- token_rm_stpw_original %>% 
  anti_join(stopword_noun_original %>% 
              filter(judge == "general" |
                       judge == "unclear" |
                       sub == "searched" |
                       sub == "lower" |
                       sub == "middle" |
                       sub == "upper" |
                       sub == "top") %>% 
              dplyr::select(term),
            by = "term") %>%
  makeDTMatrix()

runLDAtest(dtm_rm_stpw_original, K_from = 30, K_to = 30, K_by = 30,
           file_name = "TP-list-rm-stpw-original-IV")

## Pattern V: 
dtm_rm_stpw_original <- token_rm_stpw_original %>% 
  anti_join(stopword_noun_original %>% 
              filter(judge == "general" |
                       judge == "unclear" |
                       sub == "searched" |
                       sub == "lower" |
                       sub == "middle" |
                       sub == "upper" |
                       judge == "place") %>% 
              dplyr::select(term),
            by = "term") %>%
  makeDTMatrix()

runLDAtest(dtm_rm_stpw_original, K_from = 30, K_to = 30, K_by = 30,
           file_name = "TP-list-rm-stpw-original-V")

## Pattern VI: 
dtm_rm_stpw_original <- token_rm_stpw_original %>% 
  anti_join(stopword_noun_original %>% 
              filter(judge == "general" |
                       judge == "unclear" |
                       sub == "searched" |
                       sub == "lower" |
                       sub == "middle" |
                       sub == "upper" |
                       sub == "top" |
                       judge == "place") %>% 
              dplyr::select(term),
            by = "term") %>%
  makeDTMatrix()

runLDAtest(dtm_rm_stpw_original, K_from = 30, K_to = 30, K_by = 30,
           file_name = "TP-list-rm-stpw-original-VI")

## Pattern VII: 
dtm_rm_stpw_original <- token_rm_stpw_original %>% 
  anti_join(stopword_noun_original %>% 
              filter(judge == "general" |
                       judge == "unclear" |
                       sub == "searched" |
                       sub == "lower" |
                       sub == "middle" |
                       judge == "place") %>% 
              dplyr::select(term),
            by = "term") %>%
  makeDTMatrix()

runLDAtest(dtm_rm_stpw_original, K_from = 30, K_to = 30, K_by = 30,
           file_name = "TP-list-rm-stpw-original-VII")

## Pattern VIII: 
dtm_rm_stpw_original <- token_rm_stpw_original %>% 
  anti_join(stopword_noun_original %>% 
              filter(judge == "general" |
                       judge == "unclear" |
                       sub == "searched" |
                       sub == "lower" |
                       sub == "middle" |
                       judge == "place") %>% 
              dplyr::select(term),
            by = "term") %>%
  makeDTMatrix()

runLDAtest(dtm_rm_stpw_original, K_from = 30, K_to = 30, K_by = 30,
           file_name = "TP-list-rm-stpw-original-VIII")

## Pattern VIX: 
dtm_rm_stpw_original <- token_rm_stpw_original %>% 
  anti_join(stopword_noun_original %>% 
              filter(judge == "general" |
                       judge == "unclear" |
                       sub == "searched" |
                       sub == "lower" |
                       sub == "middle" |
                       sub == "upper" |
                       judge == "place") %>% 
              dplyr::select(term),
            by = "term") %>%
  makeDTMatrix()

runLDAtest(dtm_rm_stpw_original, K_from = 30, K_to = 30, K_by = 30,
           file_name = "TP-list-rm-stpw-original-VIX")

## Pattern VX: 
dtm_rm_stpw_original <- token_rm_stpw_original %>% 
  anti_join(stopword_noun_original %>% 
              filter(judge == "general" |
                       judge == "unclear" |
                       sub == "searched" |
                       sub == "lower" |
                       sub == "middle" |
                       sub == "upper" |
                       sub == "top" |
                       judge == "place") %>% 
              dplyr::select(term),
            by = "term") %>%
  makeDTMatrix()

runLDAtest(dtm_rm_stpw_original, K_from = 30, K_to = 30, K_by = 30,
           file_name = "TP-list-rm-stpw-original-VX")

## Pattern VXI: 
dtm_rm_stpw_original <- token_rm_stpw_original %>% 
  anti_join(stopword_noun_original %>% 
              filter(judge == "general" |
                       judge == "unclear" |
                       sub == "searched" |
                       sub == "lower" |
                       sub == "middle" |
                       judge == "place") %>% 
              dplyr::select(term),
            by = "term") %>%
  makeDTMatrix()

runLDAtest(dtm_rm_stpw_original, K_from = 30, K_to = 30, K_by = 30,
           file_name = "TP-list-rm-stpw-original-VXI")

## Pattern VXII: 
dtm_rm_stpw_original <- token_rm_stpw_original %>% 
  anti_join(stopword_noun_original %>% 
              filter(judge == "general" |
                       judge == "unclear" |
                       sub == "searched" |
                       sub == "lower" |
                       sub == "middle" |
                       sub == "upper" |
                       judge == "place") %>% 
              dplyr::select(term),
            by = "term") %>%
  makeDTMatrix()

runLDAtest(dtm_rm_stpw_original, K_from = 30, K_to = 30, K_by = 30,
           file_name = "TP-list-rm-stpw-original-VXII")

## Pattern VXIII: 決定！
dtm_rm_stpw_original <- token_rm_stpw_original %>% 
  anti_join(stopword_noun_original %>% 
              filter(judge == "general" |
                       judge == "unclear" |
                       sub == "searched" |
                       sub == "lower" |
                       sub == "middle" |
                       sub == "upper" |
                       sub == "top" |
                       judge == "place") %>% 
              dplyr::select(term),
            by = "term") %>%
  makeDTMatrix()

runLDAtest(dtm_rm_stpw_original, K_from = 30, K_to = 30, K_by = 30,
           file_name = "TP-list-rm-stpw-original-VXIII")
