#-----------------------------------------------------------------------------#
# Script Name: 16-lda-topic-label.R                                           #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script labels topics by checking term frequency in each topic.       #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Packages
pacman::p_load(tidyverse,
               magrittr,
               wordcloud,
               pals,
               readxl,
               writexl)

# Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

# Data
beta <- read_csv("data/lda-output-topic-term.csv")

# Topic labelling--------------------------------------------------------------

# The number of topics
K <- nrow(beta)

# Create data frame for Wordclouds
beta_df <- as.data.frame(beta)
beta_t_matrix <- t(beta_df)
beta_t_df <- as.data.frame(beta_t_matrix)

df_topic_name <- data.frame(topic = "topic", 
                            num1 = c(rep("0", 9), 
                                     rep("", K-9)),
                            num2 = seq(1:K)) %>% 
  transmute(topic = str_c(topic, num1, num2))

colnames(beta_t_df) <- df_topic_name$topic
beta_t_df$term <- rownames(beta_t_df)

# Create wordcloud of each topic
for (i in 1:K) {
  # term-frequency
  df_wc <- beta_t_df %>% 
    transmute(term, freq = beta_t_df[, i]) %>% 
    arrange(desc(freq)) %>% 
    head(30) # top 30 words
  # term
  words <- df_wc$term
  # frequency
  freq <- as.vector(unlist(df_wc$freq))
  # family to show Japanese words
  par(family = "HiraKakuProN-W3")
  # random seed
  set.seed(135)
  # plot wordcloud
  path_wc <- str_c("fig/wordclouds/wc-", i, ".png")
  png(path_wc, width = 600, height = 600)
  par(family="HiraKakuPro-W3") ##日本語フォントでもok
  wordcloud(words = words, 
            freq = freq, 
            max.words = 30, 
            random.order = FALSE, 
            rot.per = 0.50, 
            colors = cols25(6))
  dev.off()
  Sys.sleep(1)
}

# Label topics-----------------------------------------------------------------

topicList_table <- read_xlsx("table/TABLE_LDA-topic-term20.xlsx")
topicList_table %>% 
  mutate(label_ja = "",
         label_en = "") %>% 
  dplyr::select(topic, label_ja, label_en, terms) %>% 
  write_xlsx("data-manual/table-topic-labelling.xlsx")
