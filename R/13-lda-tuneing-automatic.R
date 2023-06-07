#-----------------------------------------------------------------------------#
# Script Name: 13-lda-test-run.R                                              #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script explores optimal number of topics by running the ldatuning(). #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Package
pacman::p_load(tidyverse,
               magrittr,
               tidytext,
               topicmodels,
               ldatuning,
               pals,
               hrbrthemes,
               tictoc)

# Data
tokens_rm_stpw_basic <- read_csv("data/tokens-04_rm-stpw-basic.csv")
tokens_rm_stpw_general <- read_csv("data/tokens-05_rm-stpw-general.csv")
tokens_rm_stpw_original <- read_csv("data/tokens_06-rm-stpw-original.csv")

#------------------------------------------------------------------------------

# DTMの作成
dtm_rm_stpw_basic <- makeDTMatrix(tokens_rm_stpw_basic)
dtm_rm_stpw_general <- makeDTMatrix(tokens_rm_stpw_general)
dtm_rm_stpw_original <- makeDTMatrix(tokens_rm_stpw_original)

# リストにまとめる
list_dtm <- list(dtm_rm_stpw_basic,
                 dtm_rm_stpw_general)
                 #dtm_rm_stpw_original)

# ldatuning -------------------------------------------------------------------

list_res_ldatuning <- list()
for (i in 1:2) {
  dtm <- list_dtm[[i]]
  list_res_ldatuning[[i]] <- FindTopicsNumber(
    dtm,
    topics = c(1:10 * 5, 6:11 * 10,  110, 120, 130, 140, 150, 0:3 * 50 + 150),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 123),
    mc.cores = NA,
    verbose = TRUE
  )
}

# Resultの書き出し
write_csv(as.data.frame(list_res_ldatuning[1]),
          "data-manual/ldatuning-rm-stpw-basic.csv")
write_csv(as.data.frame(list_res_ldatuning[2]),
          "data-manual/ldatuning-rm-stpw-general.csv")
write_csv(as.data.frame(list_res_ldatuning[3]),
          "data-manual/ldatuning-rm-stpw-original.csv")

# Visualize the results -------------------------------------------------------

# Read the results 
res_ldatuning_basic <- read_csv("data-manual/ldatuning-rm-stpw-basic.csv")
res_ldatuning_general <- read_csv("data-manual/ldatuning-rm-stpw-general.csv")
res_ldatuning_original <- read_csv("data-manual/ldatuning-rm-stpw-original.csv")

# Visualize the results
vizLDAtuning(res_ldatuning_basic)
vizLDAtuning(res_ldatuning_general)
vizLDAtuning(res_ldatuning_original)

## Save the visualized result
# ggsave("fig/Fig-S01_ldatuning-output.png",
#        units = "mm", width = 174, height = 200)
# ggsave("submission/biodivers-conserv-1st/images/Fig-S01_ldatuning-output.eps", 
#        units = "mm", width = 174, height = 200, device = cairo_ps)
