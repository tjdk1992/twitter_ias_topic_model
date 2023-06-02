#-----------------------------------------------------------------------------#
# Script Name: helper-functions                                               #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script contains helpful functions used in the series of analysis.    #
#                                                                             #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Packages
pacman::p_load(tidyverse, # data manipulation + ggplot2 graphics
               )

#------------------------------------------------------------------------------
# Function name: XXXXXX
# Functioning: 
# 
# Inputs:
#
# Outputs: 
#
#------------------------------------------------------------------------------

viewTop5000 <- function(df_tokens) {
  df_tokens %>% 
    group_by(term) %>% 
    summarise(n = n()) %>% 
    arrange(term) %>% 
    head(5000) %>% 
    View()
}


#------------------------------------------------------------------------------
# Function name: XXXXXX
# Functioning: 
# 
# Inputs:
#
# Outputs: 
#
#------------------------------------------------------------------------------

makeDTMatrix <- function(df_tokens) {
  df_tokens %>% 
    anti_join(df_tokens %>% 
                group_by(id_orig) %>% 
                summarise(n = n()) %>% 
                filter(n == 2) %>% 
                dplyr::select(id_orig),
              by = "id_orig") %>% 
    group_by(id_orig, term) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    cast_dtm(document = "id_orig",
             term = "term",
             value = "count")
}

#------------------------------------------------------------------------------
# Function name: XXXXXX
# Functioning: 
# 
# Inputs:
#
# Outputs: 
#
#------------------------------------------------------------------------------

runLDAtest <- function(dtm, 
                       K_from = 15, K_to = 60, K_by = 15, 
                       seed = 123, 
                       path_base = "data/data-lda-test/", 
                       file_name = "topic-list-stopword-rm") {
  for (K in seq(K_from, K_to, K_by)) {
    topicModel <- LDA(dtm,
                      k = K,
                      method = "Gibbs",
                      control = list(alpha = 50/K, 
                                     iter = 1000, 
                                     verbose = 25, 
                                     seed = seed))
    name_path <- str_c(path_base,
                       Sys.time() %>% 
                         str_remove_all("-") %>% 
                         str_replace_all(c(" " = "_",
                                           ":" = "")) %>% 
                         str_remove(".{2}$"),
                       "_",
                       file_name,
                       "_seed",
                       seed,
                       "_K",
                       K,
                       ".xlsx"
    )
    as.data.frame(terms(topicModel, 20)) %>% 
      writexl::write_xlsx(name_path)
  }
}

#------------------------------------------------------------------------------
# Function name: XXXXXX
# Functioning: 
# 
# Inputs:
#
# Outputs: 
#
#------------------------------------------------------------------------------

vizLDAtuning <- function(ldatuning_result) {
  ldatuning_result %>% 
    pivot_longer(-topics, names_to = "metrics", values_to = "value") %>% 
    group_by(metrics) %>% 
    mutate(value_scaled = scale(value)[,1]) %>% 
    ungroup() %>% 
    mutate(group = if_else(metrics == "Arun2010" | 
                             metrics == "CaoJuan2009", "1", "2")) %>% 
    ggplot(aes(x = topics, y = value_scaled, group = metrics, colour = metrics)) + 
    geom_point(aes(shape = metrics)) +
    geom_line(aes(linetype = metrics)) +
    annotate("rect", 
             xmin = 25, xmax = 150, 
             ymin = -4, ymax = 4,
             alpha = 0.2, fill = "grey") +
    scale_x_continuous(breaks = seq(0, 300, 10)) +
    facet_grid(group ~ .) +
    scale_color_manual(values=as.vector(cols25(4))) +
    labs(x = "Metrics values", y = "The number of topics (K)") +
    theme_ipsum(axis_text_size = 8,
                axis_id_orig_just = "center",
                base_family = "Helvetica") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_blank())
}
