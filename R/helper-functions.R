#-----------------------------------------------------------------------------#
# Script Name: helper-functions                                               #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script contains helpful functions used in the series of analysis.    #
#                                                                             #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------
library(tidyverse)

#------------------------------------------------------------------------------
# Function name: viewTop5000
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
                       path_base = "data-manual/lda-stpw-test/", 
                       file_name = "topic-list-stopword-rm") {
  for (K in seq(K_from, K_to, K_by)) {
    topicModel <- topicmodels::LDA(dtm,
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
# Function name: vizTopSpecieCt
# Functioning: 
# 
# Inputs:
#
# Outputs: 
#
#------------------------------------------------------------------------------

vizTopSpecieCt = function(dat, biol, lang){
  # 1:Mammal, 2:Bird, 3:Reptile, 4:Amphibian, 5:Fish,
  # 6.Invertebrate(insect), 7:Invertebrate(other), 8:Plant
  biol <- c("mammal", "bird", "reptile", "amphibian", "fish", 
            "invertebrate(insect)", "invertebrate(other)", "plant")[biol]
  # 1:English, 2:Japanese
  lang <- c("name_sp", "name_ja")[lang]
  if (lang == "name_sp") {
    fam <- "Helvetica"
  } else {
    fam <- "HiraKakuPro-W3"
  }
  dat %>% 
    filter(group_biol == biol) %>% 
    #group_by(eval(parse(text = lang))) %>%
    # summarise(n = n()) %>%
    mutate(species = eval(parse(text = lang))) %>% 
    arrange(desc(count)) %>% 
    head(30) %>% 
    ggplot() + 
    geom_bar(aes(x = reorder(species, count), 
                 y = count), 
             stat = "identity", fill = "#565555") +
    # coord_flip() +
    labs(x = "", y = "", subtitle = toupper(biol)) +
    theme_ipsum(base_family = fam, base_size = 8, axis_text_size = 8, 
                axis_title_size = 12, axis_title_just = "mc") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))
}
