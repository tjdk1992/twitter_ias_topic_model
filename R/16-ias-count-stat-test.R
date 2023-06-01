# R script for the analysis in:
# Tomojiri, D., Takaya, K. (2022) Quantitative assessment of relative topics
# and occurrence of invasive alien species in the Twitter.
# Submitted to "Conservation Biology"
#
# R Script 05: Analyzing quantified occurrence of IAS in the tweets
#
# Author: Daiki Tomojiri
#
# Outline:
## Step 1. Estimate the maximum number of tweeets / year

#------------------------------------------------------------------------------

# Setup

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

#------------------------------------------------------------------------------

# 多群の差の検定

## groupをさらに大分類に分ける
ias_count %<>% 
  mutate(category = if_else(
    group_biol == "plant", "plant", 
    if_else(group_biol == "invertebrate(insect)" |
              group_biol == "invertebrate(other)", "invertebrate", 
            "vertebrate")))

## 各グループにおける出現頻度の分布を見る
### グループ間
ias_count %>% 
  # ggplot(aes(x = count)) + 
  ggplot(aes(x = log(count))) + 
  geom_histogram(bins = 20) + 
  facet_grid(. ~ group_biol) + 
  theme_ipsum()

### カテゴリー間
ias_count %>% 
  ggplot(aes(x = log(count))) + 
  geom_histogram(bins = 20) + 
  facet_grid(. ~ category) + 
  theme_ipsum()

## 対数変換してなお正規性を確認できないのでノンパラでやる。
## Kruskal-Wallis one-way analysis of variance test

### pattern 1.脊椎動物 vs 無脊椎動物 vs 植物
kruskal.test(count ~ category, data = ias_count) # ぎりぎり有意
### pattern 2.生物グループ間
kruskal.test(count ~ group_biol, data = ias_count) # 有意
### pattern 3.法的指定
kruskal.test(count ~ reg2, data = ias_count) # 有意
### pattern 4.脊椎動物間
ias_count_vertebrate <- filter(ias_count, category == "vertebrate")
kruskal.test(count ~ group_biol, data = ias_count_vertebrate) # 有意

#------------------------------------------------------------------------------

# 多重比較

## Pattern 1は有意ではなかったので多重比較からは除外
## wilcox's multiple pairwise test
pwc_dunn_group <- ias_count %>% 
  dunn_test(count ~ category, p.adjust.method = "bonferroni") 
pwc_dunn_group %>% filter(p.adj < 0.05) # 有意な組み合わせの抽出
## Dunnett's multiple pairwise test
pwc_wilcox_group <- ias_count %>% 
  wilcox_test(count ~ category, p.adjust.method = "bonferroni") 
pwc_wilcox_group %>% filter(p.adj < 0.05) # 有意な組み合わせの抽出

## pattern 2.生物グループ間
### Dunnett's test
pwc_dunn_group <- ias_count %>% 
  dunn_test(count ~ group_biol, p.adjust.method = "bonferroni") 
pwc_dunn_group %>% filter(p.adj < 0.05) # 有意な組み合わせの抽出
### Wilcox's test
pwc_wilcox_group <- ias_count %>% 
  wilcox_test(count ~ group_biol, p.adjust.method = "bonferroni") 
pwc_wilcox_group %>% filter(p.adj < 0.05) # 有意な組み合わせの抽出

## pattern 3.法的指定
### Dunnett's test
pwc_dunn_regulation <- ias_count %>% 
  dunn_test(count ~ reg2, p.adjust.method = "bonferroni") 
filter(pwc_dunn_regulation, p.adj < 0.05) # 有意な組み合わせの抽出
### Wilcox's test
pwc_wilcox_regulation <- ias_count %>% 
  wilcox_test(count ~ reg2, p.adjust.method = "bonferroni") 
pwc_wilcox_regulation %>% filter(p.adj < 0.05) # 有意な組み合わせの抽出

## pattern 4.脊椎動物グループ間
### Dunnett's test
pwc_dunn_vertebrate <- ias_count_vertebrate %>% 
  dunn_test(count ~ group_biol, p.adjust.method = "bonferroni") 
pwc_dunn_vertebrate %>% filter(p.adj < 0.05) # 有意な組み合わせの抽出
### Wilcox's test
pwc_wilcox_vertebrate <- ias_count_vertebrate %>% 
  wilcox_test(count ~ group_biol, p.adjust.method = "bonferroni") 
pwc_wilcox_vertebrate %>% filter(p.adj < 0.05) # 有意な組み合わせの抽出

#------------------------------------------------------------------------------

# 多重比較の結果を可視化

## Pattern 1.の可視化（多群の差の検定は有意ではない）
ias_count %>% 
  ggplot(aes(x = category, y = log(count))) + 
  # ggplot(aes(x = category, y = count)) + # plot raw data
  geom_boxplot() + 
  geom_jitter(aes(color = category), 
              size = 0.5, 
              show.legend = FALSE, 
              width = 0.3) +
  # cols25かalphabet2のどちらかが良さそう。
  scale_color_manual(values = as.vector(cols25(18))) +
  labs(x = "Organism group", 
       y = "No. of tweets (log transformed)") +
  theme_ipsum(base_family = "Helvetica", 
              base_size = 8, 
              axis_text_size = 8, 
              axis_title_size = 12, 
              axis_title_just = "mc") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))

## Save the visualized result
ggsave("fig/Fig-S05_boxplot-big-category.png",
       units = "mm", width = 174, height = 150)
ggsave("submission/j-nat-conserv_1st/figs/Fig-S05_boxplot-big-category.eps", 
       units = "mm", width = 174, height = 150, device = cairo_ps)

## pattern 2.の可視化
ias_count %>% 
  transform(group_biol = factor(
    group_biol, 
    levels = c("mammal", "bird", "reptile", "amphibian", "fish", 
               "invertebrate(insect)", "invertebrate(other)", "plant"))) %>% 
  ggplot(aes(x = group_biol, y = log(count))) + 
  geom_boxplot() + 
  geom_jitter(aes(color = group_biol), size = 0.5, show.legend = FALSE, width = 0.3) +
  scale_color_manual(values=as.vector(cols25(18))) + # cols25かalphabet2のどちらかが良さそう。
  # bird-mammal: 0.002**
  annotate("text", x = 1.5, y = 12.3, label = "0.002**") +
  annotate("segment", x = 1, xend = 1, y = 11.7, yend = 12.0, linewidth = 0.3) +
  annotate("segment", x = 1, xend = 2, y = 12.0, yend = 12.0, linewidth = 0.3) +
  annotate("segment", x = 2, xend = 2, y = 11.7, yend = 12.0, linewidth = 0.3) +
  # fish-mammal: 0.044*
  annotate("text", x = 3, y = 13.1, label = "0.044*") +
  annotate("segment", x = 1, xend = 1, y = 12.5, yend = 12.8, linewidth = 0.3) +
  annotate("segment", x = 1, xend = 5, y = 12.8, yend = 12.8, linewidth = 0.3) +
  annotate("segment", x = 5, xend = 5, y = 12.5, yend = 12.8, linewidth = 0.3) +
  # plant-mammal: 0.024*
  annotate("text", x = 4.5, y = 13.9, label = "0.024*") +
  annotate("segment", x = 1, xend = 1, y = 13.3, yend = 13.6, linewidth = 0.3) +
  annotate("segment", x = 1, xend = 8, y = 13.6, yend = 13.6, linewidth = 0.3) +
  annotate("segment", x = 8, xend = 8, y = 13.3, yend = 13.6, linewidth = 0.3) +
  labs(x = "Biological group", y = "No. of tweets (log transformed)") +
  theme_ipsum(base_family = "Helvetica", base_size = 8, 
              axis_text_size = 8, axis_title_size = 12, axis_title_just = "mc") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.margin = margin(0.1,0.1,0.1,0.1, "cm")) +
  ylim(c(-0.1, 14))

## Save the visualized result
ggsave("fig/Fig-06_boxplot-biol-category.png",
       units = "mm", width = 174, height = 150)
ggsave("submission/j-nat-conserv_1st/figs/Fig-06_boxplot-biol-category.eps", 
       units = "mm", width = 174, height = 150, device = cairo_ps)

## pattern 3.の可視化
ias_count %>% 
  mutate(
    regulation2 = if_else(regulation2 == "特定", "特定外来生物", 
                          if_else(regulation2 == "要注意", "要注意外来生物", "その他")),
    regulation2 = factor(regulation2, levels = c("その他", "要注意外来生物", 
                                                 "特定外来生物"))) %>% 
  ggplot(aes(x = regulation2, y = log(n))) + 
  geom_boxplot() + 
  geom_jitter(aes(color = regulation2), size = 1, show.legend = FALSE, width = 0.3) +
  scale_color_manual(values=as.vector(cols25(18))) + # cols25かalphabet2のどちらかが良さそう。
  # 要注意外来生物-その他: <0.001***
  annotate("text", x = 1.5, y = 12.3, label = "< 0.001***") +
  annotate("segment", x = 1, xend = 1, y = 11.7, yend = 12.0, size = 0.3) +
  annotate("segment", x = 1, xend = 2, y = 12.0, yend = 12.0, size = 0.3) +
  annotate("segment", x = 2, xend = 2, y = 11.7, yend = 12.0, size = 0.3) +
  # 特定外来生物-その他: <0.001***
  annotate("text", x = 2, y = 13.1, label = "< 0.001***") +
  annotate("segment", x = 1, xend = 1, y = 12.5, yend = 12.8, size = 0.3) +
  annotate("segment", x = 1, xend = 3, y = 12.8, yend = 12.8, size = 0.3) +
  annotate("segment", x = 3, xend = 3, y = 12.5, yend = 12.8, size = 0.3) +
  # 調整
  labs(x = "Biological group", y = "No. of tweets (log transformed)") +
  theme_ipsum(base_family = "HiraKakuPro-W3", base_size = 8, axis_text_size = 8, 
              axis_title_size = 12, axis_title_just = "mc") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.margin = margin(0.1,0.1,0.1,0.1, "cm")) +  
  ylim(c(-0.1, 13.1))

## pattern 4.の可視化
ias_count_vertebrate %>% 
  transform(group = factor(
    group, 
    levels = c("mammal", "bird", "reptile", "amphibian", "fish"))) %>% 
  ggplot(aes(x = group, y = log(n))) + 
  geom_boxplot() + 
  geom_jitter(aes(color = group), size = 1, show.legend = FALSE, width = 0.3) +
  scale_color_manual(values=as.vector(cols25(18))) + # cols25かalphabet2のどちらかが良さそう。
  # mammal-bird: <0.001***
  annotate("text", x = 1.5, y = 12.3, label = "< 0.001***") +
  annotate("segment", x = 1, xend = 1, y = 11.7, yend = 12.0, size = 0.5) +
  annotate("segment", x = 1, xend = 2, y = 12.0, yend = 12.0, size = 0.5) +
  annotate("segment", x = 2, xend = 2, y = 11.7, yend = 12.0, size = 0.5) +
  # mammal-fish: <0.002*
  annotate("text", x = 3, y = 13.1, label = "< 0.002*") +
  annotate("segment", x = 1, xend = 1, y = 12.5, yend = 12.8, size = 0.5) +
  annotate("segment", x = 1, xend = 5, y = 12.8, yend = 12.8, size = 0.5) +
  annotate("segment", x = 5, xend = 5, y = 12.5, yend = 12.8, size = 0.5) +
  # 調整
  labs(x = "Vertebrate group", y = "No. of tweets (log transformed)") +
  theme_ipsum(base_family = "HiraKakuPro-W3", base_size = 8, axis_text_size = 8, 
              axis_title_size = 12, axis_title_just = "mc") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.margin = margin(0.1,0.1,0.1,0.1, "cm")) +  
  ylim(c(-0.1, 13.1))

## Save the visualized result
ggsave("fig/Fig-S06_boxplot-vertebrate-category.png",
       units = "mm", width = 174, height = 150)
ggsave("submission/j-nat-conserv_1st/figs/Fig-S06_boxplot-vertebrate-category.eps", 
       units = "mm", width = 174, height = 150, device = cairo_ps)
