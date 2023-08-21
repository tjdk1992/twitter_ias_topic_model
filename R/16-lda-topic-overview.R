#-----------------------------------------------------------------------------#
# Script Name: 17-lda-topic-label.R                                           #
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
beta <- read_csv("data/lda-output-01_topic-term.csv")
theta <- read_csv("data/lda-output-03_doc-topic-tweet.csv")

# Topic distribution in whole text data----------------------------------------
theta %>% 
  group_by(topic) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = reorder(topic, as.numeric(str_remove_all(topic, "TP"))), y = n)) +
  geom_bar(stat = "identity", fill = pal_orig[1]) +
  labs(x = "Topic", y = "The number of tweets") +
  theme_ipsum(base_size = 10,
              axis_title_size = 10,
              strip_text_size = 10,
              axis_text_size = 8,
              axis_title_just = "center",
              base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 90))

# Save the visualized result
ggsave("fig-suppl/bargraph-tweet-number-topic.png",
       units = "mm", width = 140, height = 120)
ggsave("fig-suppl/bargraph-tweet-number-topic.eps",
       units = "mm", width = 140, height = 120, device = cairo_ps)

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
  path_wc <- str_c("fig/wordclouds/wc-topic", i, ".png")
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

# Load list of top term belonging to each topics
topic_term <- read_excel("data/lda-output-04_topic-top50-terms.xlsx")

# View the top terms
topic_term[, 1:10]
topic_term[, 11:20]
topic_term[, 21:25]

label_topic <- data.frame(
  topic = str_c("Topic ", seq(25)),
  label_topic = c("Occurrence of NIS", "Population dynamics", # 1, 2
                  "Perception of NIS", "Temporal trend", # 3, 4
                  "Regulation", "Detection of poisonous NIS", # 5, 6
                  "Ecological damage", "Release of NIS", # 7, 8
                  "Extinction of endemic things", "Establishment", # 9, 10
                  "Draining management", "Environmental issue", # 11, 12
                  "Food use", "Capture for conservation", # 13, 14
                  "Danger information", "Hybridization and introgression", # 15, 16
                  "Reproductive capacity", "Emotion", # 17, 18
                  "Handling Invasive NIS", "Flower as color traits", # 19, 20
                  "Control measures", "Keep as pets", # 21, 22
                  "Human dimension of management", "Origin of NIS", # 23, 24
                  "Destroy NIS" # 25
                  ))

# Export table ----------------------------------------------------------------

# Top 20 terms in Japanese for Supplementary materials
topic_term_top25 <- head(topic_term, 25)
topic_term_top25_a <- topic_term_top25[, 1:13]
topic_term_top25_b <- topic_term_top25[, 14:25]

# Write as tex format
print(xtable(topic_term_top25_a), include.rownames = FALSE,
      file = "table-suppl/table-top25-term-TP01-TP13.tex")
print(xtable(topic_term_top25_b), include.rownames = FALSE,
      file = "table-suppl/table-top25-term-TP14-TP25.tex")

# Write table for English translation
topic_term %>% 
  head(5) %>%
  t() %>% 
  as.data.frame() %>% 
  mutate(topic = str_c("Topic ", seq(25))) %>% 
  dplyr::select(topic, V1:V5) %>% 
  write_xlsx("table-manual/table-top5-term-Japanese.xlsx")

# Read translated table
topic_term_en <- read_excel("table-manual/table-top5-term-English.xlsx")

# Format final table for manuscript
table_topic_en <- topic_term_en %>% 
  mutate(terms = str_c(V1, ", ", V2, ", ", V3, ", ", V4, ", ", V5)) %>%
  dplyr::select(topic, terms) %>% 
  left_join(label_topic, by = "topic") %>% 
  transmute(Topic = topic,
            `Topic label` = label_topic,
            `Top 10 terms` = terms)

# Write table as tex format
print(xtable(table_topic_en), include.rownames = FALSE,
      file = "table/table-top5-term-translated.tex")
