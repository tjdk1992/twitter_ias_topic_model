#-----------------------------------------------------------------------------#
# Script Name: 20-ias-count-test.R                                            #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script statistically test difference of IAS occurrence among         #
# specific groups and visualize the differences.                              #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

## Package
library(tidyverse) # for data manipulation
library(readxl)
library(magrittr) # for data manipulation
library(hrbrthemes) # for nice visualization
library(ggpubr) # to use ggarrange() function
library(pals)
library(rstatix)

## Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

## Data
tweet_tidy <- read_csv("data/basic-ias-tweet.csv")
ias_count <- read_csv("data/basic-ias-count.csv")

# Annual summary --------------------------------------------------------------

# The number of tweets in total
tweet_tidy %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = "identity")

# The number of tweets of each IAS
tweet_tidy %>% 
  group_by(name_ja, year) %>% 
  summarise(count_year = n()) %>% 
  ggplot(aes(x = year, y = count_year)) +
  geom_line(aes(colour = name_ja), 
            linewidth = 1.5,
            show.legend = FALSE) +
  theme_ipsum(base_family = "HiraKakuPro-W3")

# -----------------------------------------------------------------------------

## show the top 10 species
ias_count %>% 
  arrange(desc(count)) %>% 
  head(10)

## Draw a histogram
ias_count %>% 
  ggplot(aes(x = count)) + 
  geom_histogram(bins = 50, fill = "#1f78b4") + 
  #  geom_histogram(bins = 30, fill = "red", alpha = 0.5) + 
  theme_ipsum(base_family = "Helvetica", 
              base_size = 8, 
              axis_text_size = 8, 
              axis_title_size = 10, 
              axis_title_just = "mc") +
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))

## Save the visualized result
ggsave("fig/histogram_ias-count.png",
       units = "mm", width = 174, height = 150)
# ggsave("/Fig-S01_ldatuning-output.eps", 
#        units = "mm", width = 174, height = 150, device = cairo_ps)

#------------------------------------------------------------------------------

# 出現率上位の種の可視化

## 全グループで集計した場合
ias_count %>% 
    arrange(desc(count)) %>% 
    head(50) %>% 
    ggplot() + 
    geom_bar(aes(x = reorder(name_ja, count), 
                 y = count, 
                 fill = group_biol), 
             stat = "identity") +
    scale_fill_manual(values = pal_orig) + # cols25かalphabet2のどちらかが良さそう。
    labs(x = "Species", 
         y = "No. of tweets") +
    theme_ipsum(base_family = "HiraKakuPro-W3", 
                base_size = 8, 
                axis_text_size = 8,
                axis_title_size = 10,
                axis_title_just = "mc") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          legend.position = "bottom",
          legend.key.size = unit(5, 'mm'),
          plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))

## Save the visualized result
ggsave("fig/bargraph_top-occurred-ias.png",
       units = "mm", width = 174, height = 150)
# ggsave("submission/j-nat-conserv_1st/figs/Fig-05_top-occurred-ias.eps", 
#        units = "mm", width = 174, height = 150, device = cairo_ps)

## グループ別にプロット
ias_count
### 上位30までをプロットする関数
plotTopSpecies = function(dat, biol, lang){
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
  ias_count %>% 
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

## test the function
plotTopSpecies(ias_count, 1, 2)
plotTopSpecies(ias_count, 2, 2)

### 各グループのプロット
for (i in 1:8) {
  nam <- paste("g", i, sep="")
  assign(nam, plotTopSpecies(ias_count, i, 1))
}

### 各プロットの保存
for (i in 1:8) {
  g <- eval(parse(text = str_c("g", i)))
  path_save <- str_c("submission/j-nat-conserv_1st/figs/Fig-S04_", i,".eps")
  ggsave(path_save,
         plot = g,
         units = "mm", width = 119, height = 80)
}

## Arrange multiple figures
(g1_4 <- ggpubr::ggarrange(g1, g2, g3, g4, ncol = 2, nrow = 2))
(g5_8 <- ggpubr::ggarrange(g5, g6, g7, g8, ncol = 2, nrow = 2))

## Display the arranged figures (1-4)
annotate_figure(g1_4, bottom = text_grob("Species", hjust = 1),
                left = text_grob("No. of tweets (log transformed)", rot = 90))
## Save the visualized result
ggsave("fig/bargraph_top-occurred-ias-4group01.png",
       units = "mm", width = 174, height = 200)

## Display the arranged figures (5-8)
annotate_figure(g5_8, bottom = text_grob("Species", hjust = 1),
                left = text_grob("No. of tweets (log transformed)", rot = 90))
## Save the visualized result
ggsave("fig/bargraph_top-occurred-ias-4group02.png",
       units = "mm", width = 174, height = 200)

remove(g1, g2, g3, g4, g5, g6, g7, g8, g1_4, g5_8)
