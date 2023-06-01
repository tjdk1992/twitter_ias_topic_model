#-----------------------------------------------------------------------------#
# Script Name: 02-tweet-retrieval.R
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script screening and cleanse tweet                                   #
#                                                                             #
#-----------------------------------------------------------------------------#


# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

## Packages
library(tidyverse) # for data manipulation
library(hrbrthemes) # for nice visualization
library(magrittr) # for data manipulation
library(pals) # to use color palette

## Color palette
pal_orig <- c(rep(pals::cols25(25), 2))

## Data
### Outputs of LDA inference
theta <- read_csv("data/result-lda-inference-topic.csv")

### 元のTweetデータ
tweet_ias <- read_csv("data-raw/tweets_ias_2008_2022.csv")

### LDA用にDTM変換する直前のデータ
df_id_tokens_lda <- read_csv("data/df-id-tokens-rm-stopword.csv")

### Occurred species list
ias_occur_cum <- read_csv("data/ias_occured_cumsum.csv")

### Occurred species list
ias_occur <- read_csv("data/ias_occurred.csv")

#------------------------------------------------------------------------------

# Data preparation

## Merge data
dat_lda_sp <-df_id_tokens_lda %>% 
  transmute(title) %>% 
  distinct(.keep_all = FALSE) %>% 
  arrange(title) %>% 
  cbind(theta) %>% 
  left_join(ias_occur,
            by = "title") %>% 
  filter(!(is.na(common)))

## Check
dat_lda_sp %>% 
  group_by(title) %>% 
  summarise(n = n()) %>% 
  filter(n >= 2)
## Example
dat_lda_sp %>% 
  filter(title == 220 |
           title == 330 |
           title == 686)
## Example text
tweet_ias %>% 
  filter(id == 220 |
           id == 330 |
           id == 686) %>% 
  dplyr::select(text) %>% 
  pull() # OK

## Release memory by remove objects
remove(df_id_tokens_lda, lda_tweets, theta, tweet_ias)

#------------------------------------------------------------------------------

# Topic distribution over organism groups

## 
dat_lda_sp %<>% 
  pivot_longer(cols = c(`1`:`30`), 
               names_to = "topic",
               values_to = "prob") %>% 
  mutate(topic_num = as.numeric(topic)) %>% 
  mutate(name_add = if_else(topic_num <= 9, "0", ""),
         topic = str_c("Topic", name_add, topic)) %>% 
  dplyr::select(-c(name_add, taxon))

## Plot distribution over biological group
dat_lda_sp %>% 
  group_by(group, topic) %>% 
  summarise(prob_mean = mean(prob)) %>% 
  ggplot(aes(x = group, y = topic, fill = prob_mean)) + 
  geom_tile() + 
  geom_text(aes(label = round(prob_mean, 3))) +
  # scale_fill_distiller(palette = 'Spectral', direction = -1) + 
  # scale_fill_gradient2(low = "#1F78C8", mid = "#ff0000", high = "#33a02c", midpoint = 0.06) +
  scale_fill_gradient2(low = cols25(25)[1], 
                       mid = cols25(25)[21], 
                       high = cols25(25)[2], 
                       midpoint = 0.06) +
  labs(x = "Organism group", y = "Topic") + 
  theme_ipsum(base_family = "Helvetica", 
              base_size = 8, 
              axis_text_size = 12, 
              axis_title_size = 12, 
              axis_title_just = "mc") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))

## ComplexHeatmap
df <- scale(mtcars)
df_test <- dat_lda_sp %>% 
  group_by(group, topic) %>% 
  summarise(prob = mean(prob)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = topic,
              values_from = prob)

df_mat <- as.matrix(dplyr::select(df_test, -group))
rownames(df_mat) <- df_test$group
ComplexHeatmap::Heatmap(df_mat, row_split = 4, column_split = 5)

ggsave("table&figure/FIG.6.png", units = "cm", width = 18, height = 14)

#------------------------------------------------------------------------------

## Topic distribution over biological group
df_dend_heatmap <- dat_lda_sp %>% 
  group_by(group, topic) %>% 
  summarise(prob = mean(prob)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "topic",
              values_from = "prob")

## Convert data frame into matrix
mat_dend_heatmap <- as.matrix(dplyr::select(df_dend_heatmap, -group))
rownames(mat_dend_heatmap) <- df_dend_heatmap$group

## Prepare color palette
library(colorRamp2)
col_fun = colorRamp2(c(0, 0.03, 0.25), c("#4fa900", "white", "#fc008c"))
col_fun(seq(-1, 1))

## Plot heatmap
library(ComplexHeatmap)
row_dend = hclust(dist(mat_dend_heatmap))
Heatmap(mat_dend_heatmap, 
        name = "probability", 
        col = col_fun, 
        show_column_dend = FALSE, 
        row_names_gp = gpar(fontsize = 5),
        column_names_gp = gpar(fontsize = 5),
        row_dend_width = unit(50, "mm"),
        # cluster_rows = color_branches(row_dend, k = 10),
        column_order = 
          order(as.numeric(gsub("column", 
                                "", 
                                colnames(mat_dend_heatmap)))))

## Topic distribution over species
df_dend_heatmap <- dat_lda_sp %>% 
  group_by(species, topic) %>% 
  summarise(prob = mean(prob)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "topic",
              values_from = "prob")

## Convert data frame into matrix
mat_dend_heatmap <- as.matrix(dplyr::select(df_dend_heatmap, 
                                            c(2:24, 26:31)
                                            # c(5:9, 11:16, 19:20, 24, 26, 29)
                                            ))
rownames(mat_dend_heatmap) <- df_dend_heatmap$species

## Prepare color palette
library(colorRamp2)
col_fun = colorRamp2(c(0, 0.07, 0.1), c(cols25(25)[1], cols25(25)[21], cols25(25)[2]))

## Plot heatmap
library(ComplexHeatmap)
row_dend = hclust(dist(mat_dend_heatmap))
Heatmap(mat_dend_heatmap, 
        name = "probability", 
        col = col_fun, 
        show_column_dend = FALSE, 
        row_names_gp = gpar(fontsize = 5),
        column_names_gp = gpar(fontsize = 5),
        row_dend_width = unit(50, "mm"),
        # cluster_rows = color_branches(row_dend, k = 10),
        column_order = 
          order(as.numeric(gsub("column", 
                                "", 
                                colnames(mat_dend_heatmap)))))

dat_glm <- df_dend_heatmap %>% 
  left_join(ias_occur_cum %>% 
              dplyr::select(n, species),
            by = "species") %>% 
  dplyr::select(species, n, c(Topic01:Topic30))

#------------------------------------------------------------------------------
dat_lda_sp %>% 
  filter(common == "ヒアリ") %>% 
  ggplot(aes(x = year, y = prob, fill = topic)) +
  geom_area() +
  scale_x_continuous(expand=c(0,0))

dat_lda_sp %>% 
  filter(common == "ヒアリ") %>% 
  group_by(topic) %>% 
  summarise(prob_mean = mean(prob)) %>% 
  ungroup() %>% 
  summarise(sum = sum(prob_mean))

dat_lda_sp %>% 
  group_by(common, topic) %>% 
  summarise(prob_mean = mean(prob)) %>% 
  group_by()
  summarise(sum = sum(prob_mean))

## Proportion in individual species
dat_lda_sp %>% 
  group_by(common, topic) %>% 
  summarise(prob_mean = mean(prob)) %>% 
  ungroup() %>% 
  filter(common == "ヒアリ" | common == "アルゼンチンアリ"| 
           common == "ネコ" | common == "アライグマ"| 
           common == "ミシシッピアカミミガメ" | common == "カミツキガメ"| 
           common == "アメリカザリガニ" | common == "セアカゴケグモ"| 
           common == "スクミリンゴガイ" | common == "Ocorhynchus mykiss") %>% 
  ggplot(aes(x = topic, y = prob_mean, fill = topic)) + 
  geom_bar(stat = "identity") +
  facet_wrap(common ~ ., ncol = 2) +
  scale_fill_manual(values = as.vector(pal_orig)) +
  labs(x = "Year", y = "Probability") +
  theme_ipsum(base_family = "HiraKakuPro-W3",
              # base_family = "Helvetica", 
              base_size = 8, 
              axis_text_size = 8, 
              axis_title_size = 8, 
              axis_title_just = "mc") +
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"), 
        axis.text.x = element_text(angle = 90))

# Entropy
dat_entropy <- dat_lda_sp %>% 
  group_by(common, topic) %>% 
  summarise(prob_mean = mean(prob)) %>% 
  ungroup()

# country_dat$ln.prob <- log(country_dat$probability)
# country_dat$pre.ent <- country_dat$probability*country_dat$ln.prob

dat_entropy$ln.prob <- log(dat_entropy$prob_mean)
dat_entropy$pre.ent <- dat_entropy$prob_mean*dat_entropy$ln.prob

species_entropy <- dat_entropy %>% 
  group_by(common) %>% 
  summarise(entropy = sum(pre.ent)*(-1))

species_entropy %>% 
  ggplot(aes(x = reorder(common, entropy), y = entropy)) +
  geom_bar(stat = "identity") + 
  theme_ipsum(base_family = "HiraKakuPro-W3", base_size = 8, axis_title_size = 12) +
  labs(x="Country", y="Entropy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

species_entropy %>% 
  arrange(desc(entropy))
species_entropy %>% 
  arrange(desc(entropy))

species_prob_var <- dat_entropy %>% 
  group_by(common) %>% 
  summarise(var = var(prob_mean),
            sd = sd(prob_mean))

ias_occur_cum %>% 
  left_join(species_prob_var, by = "common") %>% 
  ggplot(aes(x = sd, y = n)) +
  geom_point()

ias_occur_cum %>% 
  transmute(common, log(n)) %>% 
  left_join(species_entropy, by = "common") %>% 
  dplyr::select(-common) %>% 
  cor()
  ggplot(aes(x = entropy, y = log(n))) +
  geom_point()
  
  