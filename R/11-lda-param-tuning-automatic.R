# R script for the analysis in:
# Tomojiri, D., Takaya, K. (2022) Quantitative assessment of relative topics
# and occurrence of invasive alien species in the Twitter.
# Submitted to "Conservation Biology"
#
# R Script 09: Automatically determining the number of topics
#
# Author: Daiki Tomojiri
#
# Outline:
## Step 1. Estimate the maximum number of tweeets / year

#------------------------------------------------------------------------------

# Setup

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
library(pals)

## Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

## Data
df_id_tokens_rm_stopword <- read_csv("data/df-id-tokens-rm-stopword.csv")

#------------------------------------------------------------------------------

# Determining the number of topics, K

# 単語の出現頻度の算出
df_rm_stopword_count <- df_id_tokens_rm_stopword %>% 
  group_by(title, term) %>% 
  summarise(count = n()) %>% 
  ungroup()

## Create document term matrix
dtm_rm_stopword <- cast_dtm(df_rm_stopword_count,
                            document = "title", 
                            term = "term", 
                            value = "count")

## Remove tokens data to release memory
remove(df_id_tokens_rm_stopword, df_id_tokens_rm_stopword)

## Automatic determination of K by using ldatuning package
tic()
ldatuning_result <- FindTopicsNumber(
  dtm_rm_stopword,
  topics = c(1:10 * 5, 110, 120, 130, 140, 150, 0:3 * 50 + 150),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 123),
  mc.cores = NA,
  verbose = TRUE
)
toc()

## Resultの書き出し
ldatuning_result %>% 
  as.data.frame %>% 
  write_csv("data/result-ldatuning.csv")

#------------------------------------------------------------------------------

# Result of the ldatuning

## Read the result of the ldatuning (run in another PC in Kyoto Univ.)
ldatuning_result <- read_csv("data/result-ldatuning_external-PC.csv")

## Visualization of the ldatuning result
ldatuning_result %>% 
  pivot_longer(-1, names_to = "metrics", values_to = "value") %>% 
  group_by(metrics) %>% 
  mutate(value_scaled = scale(value)[,1]) %>% 
  ungroup() %>% 
  mutate(group = if_else(metrics == "Arun2010" | metrics == "CaoJuan2009", "1", "2")) %>% 
  ggplot(aes(x = topics, y = value_scaled, group = metrics, colour = metrics)) + 
  geom_point(aes(shape = metrics)) +
  geom_line(aes(linetype = metrics)) +
  annotate("rect", 
           xmin = 25, xmax = 150, 
           ymin = -3, ymax = 3,
           alpha = 0.2, fill = "grey") +
  scale_x_continuous(breaks = seq(0, 300, 10)) +
  facet_grid(group ~ .) +
  scale_color_manual(values=as.vector(cols25(4))) +
  labs(x = "Metrics values", y = "The number of topics (K)") +
  theme_ipsum(axis_text_size = 8,
              axis_title_just = "center",
              base_family = "Helvetica") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_blank())

## Save the visualized result
ggsave("fig/Fig-S01_ldatuning-output.png",
       units = "mm", width = 174, height = 200)
ggsave("submission/biodivers-conserv-1st/images/Fig-S01_ldatuning-output.eps", 
       units = "mm", width = 174, height = 200, device = cairo_ps)
