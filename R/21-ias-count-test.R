#-----------------------------------------------------------------------------#
# Script Name: 22-ias-count-test.R                                            #
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
pacman::p_load(tidyverse,  # for data manipulation
               hrbrthemes, # for visualization
               ggpubr,     # for 
               tidyverse,  # for data manipulation
               readxl,     # to read excel sheet
               magrittr,   # for data manipulation
               hrbrthemes, # for nice visualization
               ggpubr,     # to use ggarrange function
               pals,
               rstatix
               )

# Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

# Data
ias_count <- read_csv("data/ias-count.csv")

# 最終的に用いる代表値
ias_count %<>% 
  transmute(name_sp, name_ja, 
            group_biol, 
            reg1, reg2, 
            count = mean # final typical value
  )

# 多群の差の検定---------------------------------------------------------------

# Invertebrateを1つにする
ias_count %<>% 
  mutate(group_biol = str_replace_all(group_biol,
                                      c("\\(insect\\)" = "",
                                        "\\(other\\)" = "")))

# groupをさらに大分類に分ける
ias_count %<>% 
  mutate(category = if_else(group_biol == "plant", "plant", 
                            if_else(group_biol == "invertebrate", 
                                    "invertebrate", "vertebrate")))

# 各グループにおける出現頻度の分布を見る
## カテゴリー間
ias_count %>% 
  ggplot(aes(x = count)) + 
  geom_histogram(bins = 20) + 
  facet_grid(. ~ category) + 
  theme_ipsum()

## 生物グループ間
ias_count %>% 
  ggplot(aes(x = count)) + 
  geom_histogram(bins = 20) + 
  facet_wrap(. ~ group_biol, nrow = 2) + 
  theme_ipsum()

# Save the visualized result
ggsave("fig/histogram-biol-category.png",
       units = "mm", width = 150, height = 130)
ggsave("fig/histogram-biol-category.eps",
       units = "mm", width = 150, height = 130, device = cairo_ps)

# 対数変換してなお正規性を確認できないのでノンパラでやる。
# Kruskal-Wallis one-way analysis of variance test

## pattern 1.脊椎動物 vs 無脊椎動物 vs 植物
kruskal.test(count ~ category, data = ias_count) # 有意差あり

## pattern 2.生物グループ間
kruskal.test(count ~ group_biol, data = ias_count) # 有意差あり

# 多重比較(multiple comparison)------------------------------------------------

# Pattern 1. 脊椎動物 vs 無脊椎動物 vs 植物
# Wilcox's multiple pairwise test
pwc_wilcox_group <- ias_count %>% 
  wilcox_test(count ~ category, p.adjust.method = "bonferroni") 
pwc_wilcox_group %>% filter(p.adj < 0.05) # 有意な組み合わせの抽出
# Dunnett's multiple pairwise test
pwc_dunn_group <- ias_count %>%
  dunn_test(count ~ category, p.adjust.method = "bonferroni")
pwc_dunn_group %>% filter(p.adj < 0.05) # 有意な組み合わせの抽出

# pattern 2. 生物グループ間
# Wilcox's multiple pairwise test
pwc_wilcox_group <- ias_count %>% 
  wilcox_test(count ~ group_biol, p.adjust.method = "bonferroni") 
pwc_wilcox_group %>% filter(p.adj < 0.05) # 有意な組み合わせの抽出
# Dunnett's multiple pairwise test
pwc_dunn_group <- ias_count %>%
  dunn_test(count ~ group_biol, p.adjust.method = "bonferroni")
pwc_dunn_group %>% filter(p.adj < 0.05) # 有意な組み合わせの抽出

# Visualize the results of multiple-text --------------------------------------

# Pattern 1.の可視化（多群の差の検定は有意ではない）
ias_count %>% 
  ggplot(aes(x = category, y = log(count))) + 
  # ggplot(aes(x = category, y = count)) + # plot raw data
  geom_boxplot() + 
  geom_jitter(aes(color = category), 
              size = 0.5, 
              show.legend = FALSE, 
              width = 0.3) +
  # invertebrate-plant: 0.005**
  annotate("text", x = 1.5, y = 8.0, label = "0.005**") +
  annotate("segment", x = 1, xend = 1, y = 7.5, yend = 7.8, linewidth = 0.3) +
  annotate("segment", x = 1, xend = 2, y = 7.8, yend = 7.8, linewidth = 0.3) +
  annotate("segment", x = 2, xend = 2, y = 7.5, yend = 7.8, linewidth = 0.3) +
  # cols25かalphabet2のどちらかが良さそう。
  scale_color_manual(values = as.vector(cols25(18))) +
  labs(x = "Organism group", 
       y = "No. of tweets (log transformed)") +
  theme_ipsum(base_family = "Helvetica", 
              base_size = 8, 
              axis_text_size = 8, 
              axis_title_size = 12, 
              axis_title_just = "mc") + 
  theme(
    # axis.text.x = element_text(angle = 90, hjust = 1), 
    plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))

## Save the visualized result
ggsave("fig/boxplot-big-category.png",
       units = "mm", width = 80, height = 100)
ggsave("fig/boxplot-big-category.eps",
       units = "mm", width = 80, height = 100, device = cairo_ps)

# pattern 2.の可視化
ias_count %>% 
  transform(group_biol = factor(
    group_biol, 
    levels = c("mammal", "bird", "reptile", "amphibian", "fish", 
               "invertebrate", "plant"))) %>% 
  ggplot(aes(x = group_biol, y = log(count))) + 
  geom_boxplot() + 
  geom_jitter(aes(color = group_biol), size = 0.5, show.legend = FALSE, width = 0.3) +
  scale_color_manual(values=as.vector(cols25(18))) + # cols25かalphabet2のどちらかが良さそう。
  # invertebrate mammal 0.001**
  annotate("text", x = 3.5, y = 10.1, label = "0.001*") +
  annotate("segment", x = 1, xend = 1, y = 9.5, yend = 9.8, linewidth = 0.3) +
  annotate("segment", x = 1, xend = 6, y = 9.8, yend = 9.8, linewidth = 0.3) +
  annotate("segment", x = 6, xend = 6, y = 9.5, yend = 9.8, linewidth = 0.3) +
  # bird-mammal 0.00018***
  annotate("text", x = 1.5, y = 9.3, label = "0.00018**") +
  annotate("segment", x = 1, xend = 1, y = 8.7, yend = 9.0, linewidth = 0.3) +
  annotate("segment", x = 1, xend = 2, y = 9.0, yend = 9.0, linewidth = 0.3) +
  annotate("segment", x = 2, xend = 2, y = 8.7, yend = 9.0, linewidth = 0.3) +
  # bird-plant 0.005**
  annotate("text", x = 4.5, y = 8.5, label = "0.005*") +
  annotate("segment", x = 2, xend = 2, y = 7.9, yend = 8.2, linewidth = 0.3) +
  annotate("segment", x = 2, xend = 7, y = 8.2, yend = 8.2, linewidth = 0.3) +
  annotate("segment", x = 7, xend = 7, y = 7.9, yend = 8.2, linewidth = 0.3) +
  # invertebrate plant 0.036*
  annotate("text", x = 6.5, y = 7.7, label = "0.036*") +
  annotate("segment", x = 6, xend = 6, y = 7.1, yend = 7.4, linewidth = 0.3) +
  annotate("segment", x = 6, xend = 7, y = 7.4, yend = 7.4, linewidth = 0.3) +
  annotate("segment", x = 7, xend = 7, y = 7.1, yend = 7.4, linewidth = 0.3) +
  labs(x = "Biological group", y = "No. of tweets (log transformed)") +
  theme_ipsum(base_family = "Helvetica", base_size = 8, 
              axis_text_size = 8, axis_title_size = 12, axis_title_just = "mc") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.margin = margin(0.1,0.1,0.1,0.1, "cm")) +
  ylim(c(-0.1, 10.5))

# Save the visualized result
ggsave("fig/boxplot-biol-category.png",
       units = "mm", width = 80, height = 130)
ggsave("fig/boxplot-biol-category.eps",
       units = "mm", width = 80, height = 130, device = cairo_ps)
