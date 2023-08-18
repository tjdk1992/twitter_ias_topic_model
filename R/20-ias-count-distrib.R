#-----------------------------------------------------------------------------#
# Script Name: 21-ias-count-distrib.R                                         #
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
pacman::p_load(tidyverse, # for data manipulation
               readxl,
               magrittr, # for data manipulation
               hrbrthemes, # for nice visualization
               ggpubr, # to use ggarrange() function
               pals,
               rstatix
               )

# Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

# Data
ias_count <- read_csv("data/ias-count.csv")

# -----------------------------------------------------------------------------

# 最終的に用いる代表値
ias_count %<>% 
  transmute(name_sp, name_ja, 
            group_biol, 
            reg1, reg2, 
            count = mean # final typical value
            )

# Invertebrateを1つにする
ias_count %<>% 
  mutate(group_biol = str_replace_all(group_biol,
                                      c("\\(insect\\)" = "",
                                        "\\(other\\)" = "")))

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
       units = "mm", width = 100, height = 80)
ggsave("fig/histogram_ias-count.eps",
       units = "mm", width = 100, height = 80, device = cairo_ps)

# Visualization of top-occurring IAS-------------------------------------------

# 全グループで集計した場合
ias_count %>%
  arrange(desc(count)) %>% 
  head(50) %>% 
  ggplot() + 
  geom_bar(aes(x = reorder(name_sp, count), 
               y = count, 
               fill = group_biol), 
           stat = "identity") +
  scale_fill_manual(values = pal_orig) + # cols25かalphabet2のどちらかが良さそう。
  labs(x = "Species", 
       y = "No. of tweets") +
  theme_ipsum(base_family = "Helvetica", 
              base_size = 8, 
              axis_text_size = 8,
              axis_title_size = 10,
              axis_title_just = "mc") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, face = "italic"), 
        legend.position = "bottom",
        legend.key.size = unit(5, 'mm'),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))

# Save the visualized result
ggsave("fig/bargraph_top-occurred-ias.png",
       units = "mm", width = 150, height = 120)
ggsave("fig/bargraph_top-occurred-ias.eps",
       units = "mm", width = 150, height = 120, device = cairo_ps)

# グループ別にプロット

# Helper function -------------------------------------------------------------

l_biol <- unique(pull(ias_count, group_biol))
for (i in 1:length(l_biol)) {
  biol <- l_biol[i]
  # Plot
  ias_count %>% 
      filter(group_biol == "mammal") %>% 
      arrange(desc(count)) %>% 
      head(30) %>% 
      ggplot() + 
      geom_bar(aes(x = reorder(name_sp, count), 
                   y = count), 
               stat = "identity", fill = "#565555") +
      labs(x = "", y = "", subtitle = str_to_title(biol)
      ) +
      theme_ipsum(base_family = "Helvetica", base_size = 8,
                  axis_text_size = 8, axis_title_size = 10, 
                  axis_title_just = "mc") + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1, face = "italic"), 
            plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))
  # Save path
  path_png <- str_c("fig/histogram_ias-count_", biol, ".png")
  path_eps <- str_c("fig/histogram_ias-count_", biol, ".eps")
  # Save plot
  ggsave(path_png, units = "mm", width = 150, height = 90)
  ggsave(path_eps, units = "mm", width = 150, height = 90, device = cairo_ps)
}
