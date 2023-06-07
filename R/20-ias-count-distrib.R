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

# Package
pacman::p_load(tidyverse,
               hrbrthemes,
               ggpubr)
library(tidyverse) # for data manipulation
library(readxl)
library(magrittr) # for data manipulation
library(hrbrthemes) # for nice visualization
library(ggpubr) # to use ggarrange() function
library(pals)
library(rstatix)

# Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

# Data
ias_count_total <- read_csv("data/ias-count_total.csv")
ias_count_annual <- read_csv("data/ias-count_annual.csv")

# -----------------------------------------------------------------------------

## show the top 10 species
ias_count_total %>% 
  arrange(desc(count)) %>% 
  head(10)

## Draw a histogram
ias_count_total %>% 
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
ias_count_total %>%
# ias_count_annual %>% 
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

# グループ別にプロット
## test the function
vizTopSpecieCt(ias_count_total, 1, 2)
vizTopSpecieCt(ias_count_total, 2, 2)

### 各グループのプロット
for (i in 1:8) {
  nam <- paste("g", i, sep="")
  assign(nam, vizTopSpecieCt(ias_count_total, i, 1))
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
