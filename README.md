# Aspects of public attention on popular nonindigenous species, as determined by a comprehensive assessment of Japanese social media

**Author**: Daiki Tomojiri & Kosuke Takaya (2023)

**Publication**: _Biological Conservation_ (current status: under submission)

**Keywords**: invasive alien species, conservation culturomics, twitter, text mining, topic modelling

## Abstract
Invasion culturomics is an emerging field of study that utilizes digital data existing on the Internet to reveal the human dimension of nonindigenous species (NIS) invasions. Although hypothetical approaches have been used to examine explanatory variables that predict the amount of public attention by using proxies, it has been difficult to observe direct associations between these variables. Here, we aimed to deepen our understanding of the relationship between people and NIS by analyzing the content of texts about NIS on social media, and by clarifying the context and aspects to which public attention is directed. Specifically, we quantified tweets about NIS to identify popular NIS that attract a lot of public attention on Twitter. We also identified hidden topics in which NIS names occurred by applying topic modeling to tweets, and we investigated the topic distribution over popular NIS. A relatively small number of species were selected as popular NIS for further analysis, and 25 hidden topics were identified from all the tweets used in the analysis. The topic distribution over popular NIS had three patterns across taxonomic groups: (1) biased among topics but consistent within taxonomic groups; (2) relatively even among topics and consistent within taxonomic groups; and (3) not consistent within taxonomic groups and with biases differing among species. These findings can provide important insights into the formulation of a better strategy for NIS management by approaching the social and human dimensions of NIS invasion; such a strategy would include information dissemination, environmental education, and management campaigns. Our methodological framework can be used to address other important environmental topics that are attracting public attention and in which the objects have similar compositions (e.g., species of disease pathogens or types of marine debris).

## Contents of the Repository

| Dir | Type | File Name | Description |
|:---|:---|:---|:---|
|R  |R script |01-nis-list-compilation.R  |Script for organizing data on NIS copied from NIES website (https://www.nies.go.jp/biodiversity/invasive/) |
|R  |R script |02-tweet-collection.R      |Script for retrieving Tweets data through Twitter academic API by using get_all_tweets function from the academictwitteR package of R. |
|R  |R script |03-tweet-filtering.R       |Script to automatically exclude unnecessary tweets such as retweets. |
|R  |R script |04-tweet-screening.R       |Script to further narrow the scope of the search query and limit the tweets to those that matched the following two combinations: "nonindigenous", "introduced", or "naturalized" and NIS common names. |
|R  |R script |05-tweet-cleansing.R       |Script to manually remove irrelevant terms such as media names. |
|R  |R script |06-tweet-tokenization.R    |Script for morphological analysis of text in the tweet. |
|R  |R script |07-token-cleansing.R       |Script for XXXX |
|R  |R script |08-token-cutting.R         |Script to exclude tweets with an extremely low (less than 0.001% of all tweets) or high (more than 15% of all tweets) frequency of occurrence among all tweets. |
|R  |R script |09-stopword-basic.R        |Script for XXXX |
|R  |R script |10-stopword-general.R      |Script for XXXX |
|R  |R script |11-stopword-original.R     |Script for XXXX |
|R  |R script |12-lda-trial-examination.R |Script for XXXX |
|R  |R script |13-lda-tuning-automatic.R  |Script for XXXX |
|R  |R script |14-lda-tuning-manual.R     |Script for XXXX |
|R  |R script |15-lda-model-inference.R   |Script for XXXX |
|R  |R script |16-lda-topic-overview.R    |Script for XXXX |
|R  |R script |17-tweet-ias-occurrence.R  |Script for XXXX |
|R  |R script |18-count-topic-assoc.R     |Script for XXXX |
|R  |R script |19-topic-dissimilarity.R   |Script for XXXX |
|fig |figure | | |
|fig |figure | | |
|fig |figure | | |
|fig |figure | | |
|fig-manual |figure | | |
|fig-manual |figure | | |
|fig-suppl |figure | | |
|fig-suppl |figure | | |
|fig-suppl |figure | | |
|fig-suppl |figure | | |
|fig-suppl |figure | | |
|fig |figure |Fig-S01 |Results of the ldatuning in the ldatuning package |
|fig |figure |Fig-S02 |WordClouds presenting Japanese terms by size in their occurrence probability in each topic |
|fig |figure |Fig-S03 |Occurring temporal trends of each topic between XXXX and XXXX |
|fig |figure |Fig-03 |Rank change of topics through three periods (XXXX — XXXX, XXXX — XXXX and XXXX — XXXX) |
|fig |figure |Fig-04 |Top 30 frequently occurred IAS in the tweets |
|fig |figure |Fig-S04 |Top 30 frequently occurred species in the each biological group in the tweets (1 — 8) |
|fig |figure |Fig-05 |Boxplot comparing biological group |
|fig |figure |Fig-S05 |Boxplot comparing vertebrate |
|fig |figure |Fig-S06 |Boxplot comparing big group |
|fig |figure |Fig-06 |Heatmap showing topic distribution over aggregated tweets mentioned IAS (with dendrogram) |
|fig |figure |Fig-07 |Heatmap showing topic distribution over top 30 frequently occurred IAS (without dendrogram) |
|fig-manual |figure |Fig-01 |Graphical abstract of the flow of analysis design |
|fig-manual |figure |Fig-02 |Graphical model of latent Dirichlet allocation applied to big tweets data |
|table |Spreadsheet |Table-01 |All identified topics from the LDA inference, their given name and the top 10 frequently occurring terms composing each topic (all terms were translated into English from Japanese). |
|table |Spreadsheet |Table-S01 |All identified topics from the LDA inference, their given name and the top 10 frequently occurring terms composing each topic (in Japanese). |
|table |Spreadsheet |Table-S02 |All invasive alien species (IAS) searched all over the tweets to quantify their occurrence in them. -> hittedとその数は列作ってわかるようにする。 |

```
─ Session info ─
 setting  value
 version  R version 4.3.1 (2023-06-16)
 os       macOS Sonoma 14.0
 system   x86_64, darwin20
 ui       RStudio
 language (EN)
 collate  en_US.UTF-8
 ctype    en_US.UTF-8
 tz       Asia/Tokyo
 date     2023-12-06
 rstudio  2023.06.2+561 Mountain Hydrangea (desktop)
 pandoc   3.1.1 @ /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/ (via rmarkdown)

─ Packages ─
 package           * version date (UTC) lib source
 abind               1.4-5   2016-07-21 [1] CRAN (R 4.3.0)
 backports           1.4.1   2021-12-13 [1] CRAN (R 4.3.0)
 bit                 4.0.5   2022-11-15 [1] CRAN (R 4.3.0)
 bit64               4.0.5   2020-08-30 [1] CRAN (R 4.3.0)
 broom               1.0.5   2023-06-09 [1] CRAN (R 4.3.0)
 car                 3.1-2   2023-03-30 [1] CRAN (R 4.3.0)
 carData             3.0-5   2022-01-06 [1] CRAN (R 4.3.0)
 cli                 3.6.1   2023-03-23 [1] CRAN (R 4.3.0)
 cluster             2.1.4   2022-08-22 [1] CRAN (R 4.3.1)
 colorspace          2.1-0   2023-01-23 [1] CRAN (R 4.3.0)
 crayon              1.5.2   2022-09-29 [1] CRAN (R 4.3.0)
 crul                1.4.0   2023-05-17 [1] CRAN (R 4.3.0)
 curl                5.0.2   2023-08-14 [1] CRAN (R 4.3.0)
 dichromat           2.0-0.1 2022-05-02 [1] CRAN (R 4.3.0)
 digest              0.6.33  2023-07-07 [1] CRAN (R 4.3.0)
 dplyr             * 1.1.3   2023-09-03 [1] CRAN (R 4.3.0)
 ellipsis            0.3.2   2021-04-29 [1] CRAN (R 4.3.0)
 evaluate            0.21    2023-05-05 [1] CRAN (R 4.3.0)
 extrafont           0.19    2023-01-18 [1] CRAN (R 4.3.0)
 extrafontdb         1.0     2012-06-11 [1] CRAN (R 4.3.0)
 fansi               1.0.4   2023-01-22 [1] CRAN (R 4.3.0)
 farver              2.1.1   2022-07-06 [1] CRAN (R 4.3.0)
 fastmap             1.1.1   2023-02-24 [1] CRAN (R 4.3.0)
 fontBitstreamVera   0.1.1   2017-02-01 [1] CRAN (R 4.3.0)
 fontLiberation      0.1.0   2016-10-15 [1] CRAN (R 4.3.0)
 fontquiver          0.2.1   2017-02-01 [1] CRAN (R 4.3.0)
 forcats           * 1.0.0   2023-01-29 [1] CRAN (R 4.3.0)
 gdtools             0.3.3   2023-03-27 [1] CRAN (R 4.3.0)
 generics            0.1.3   2022-07-05 [1] CRAN (R 4.3.0)
 gfonts              0.2.0   2023-01-08 [1] CRAN (R 4.3.0)
 ggplot2           * 3.4.3   2023-08-14 [1] CRAN (R 4.3.0)
 glue                1.6.2   2022-02-24 [1] CRAN (R 4.3.0)
 gtable              0.3.4   2023-08-21 [1] CRAN (R 4.3.0)
 hms                 1.1.3   2023-03-21 [1] CRAN (R 4.3.0)
 hrbrthemes        * 0.8.0   2020-03-06 [1] CRAN (R 4.3.0)
 htmltools           0.5.6   2023-08-10 [1] CRAN (R 4.3.0)
 httpcode            0.3.0   2020-04-10 [1] CRAN (R 4.3.0)
 httpuv              1.6.11  2023-05-11 [1] CRAN (R 4.3.0)
 janeaustenr         1.0.0   2022-08-26 [1] CRAN (R 4.3.0)
 jsonlite            1.8.7   2023-06-29 [1] CRAN (R 4.3.0)
 knitr               1.43    2023-05-25 [1] CRAN (R 4.3.0)
 labeling            0.4.3   2023-08-29 [1] CRAN (R 4.3.0)
 later               1.3.1   2023-05-02 [1] CRAN (R 4.3.0)
 lattice           * 0.21-8  2023-04-05 [1] CRAN (R 4.3.1)
 lifecycle           1.0.3   2022-10-07 [1] CRAN (R 4.3.0)
 lubridate         * 1.9.2   2023-02-10 [1] CRAN (R 4.3.0)
 magrittr          * 2.0.3   2022-03-30 [1] CRAN (R 4.3.0)
 mapproj             1.2.11  2023-01-12 [1] CRAN (R 4.3.0)
 maps                3.4.1   2022-10-30 [1] CRAN (R 4.3.0)
 MASS                7.3-60  2023-05-04 [1] CRAN (R 4.3.1)
 Matrix              1.6-1   2023-08-14 [1] CRAN (R 4.3.0)
 mgcv                1.8-42  2023-03-02 [1] CRAN (R 4.3.1)
 mime                0.12    2021-09-28 [1] CRAN (R 4.3.0)
 modeltools          0.2-23  2020-03-05 [1] CRAN (R 4.3.0)
 munsell             0.5.0   2018-06-12 [1] CRAN (R 4.3.0)
 nlme                3.1-162 2023-01-31 [1] CRAN (R 4.3.1)
 NLP                 0.2-1   2020-10-14 [1] CRAN (R 4.3.0)
 pacman              0.5.1   2019-03-11 [1] CRAN (R 4.3.0)
 pals              * 1.8     2023-08-23 [1] CRAN (R 4.3.0)
 patchwork         * 1.1.3   2023-08-14 [1] CRAN (R 4.3.0)
 permute           * 0.9-7   2022-01-27 [1] CRAN (R 4.3.0)
 pillar              1.9.0   2023-03-22 [1] CRAN (R 4.3.0)
 pkgconfig           2.0.3   2019-09-22 [1] CRAN (R 4.3.0)
 plyr                1.8.8   2022-11-11 [1] CRAN (R 4.3.0)
 promises            1.2.1   2023-08-10 [1] CRAN (R 4.3.0)
 purrr             * 1.0.2   2023-08-10 [1] CRAN (R 4.3.0)
 R6                  2.5.1   2021-08-19 [1] CRAN (R 4.3.0)
 Rcpp                1.0.11  2023-07-06 [1] CRAN (R 4.3.0)
 readr             * 2.1.4   2023-02-10 [1] CRAN (R 4.3.0)
 reshape2          * 1.4.4   2020-04-09 [1] CRAN (R 4.3.0)
 rlang               1.1.1   2023-04-28 [1] CRAN (R 4.3.0)
 rmarkdown           2.24    2023-08-14 [1] CRAN (R 4.3.0)
 rstatix           * 0.7.2   2023-02-01 [1] CRAN (R 4.3.0)
 rstudioapi          0.15.0  2023-07-07 [1] CRAN (R 4.3.0)
 Rttf2pt1            1.3.12  2023-01-22 [1] CRAN (R 4.3.0)
 scales              1.2.1   2022-08-20 [1] CRAN (R 4.3.0)
 sessioninfo         1.2.2   2021-12-06 [1] CRAN (R 4.3.0)
 shiny               1.7.5   2023-08-12 [1] CRAN (R 4.3.0)
 slam                0.1-50  2022-01-08 [1] CRAN (R 4.3.0)
 SnowballC           0.7.1   2023-04-25 [1] CRAN (R 4.3.0)
 stringi             1.7.12  2023-01-11 [1] CRAN (R 4.3.0)
 stringr           * 1.5.0   2022-12-02 [1] CRAN (R 4.3.0)
 systemfonts         1.0.4   2022-02-11 [1] CRAN (R 4.3.0)
 tibble            * 3.2.1   2023-03-20 [1] CRAN (R 4.3.0)
 tidyr             * 1.3.0   2023-01-24 [1] CRAN (R 4.3.0)
 tidyselect          1.2.0   2022-10-10 [1] CRAN (R 4.3.0)
 tidytext            0.4.1   2023-01-07 [1] CRAN (R 4.3.0)
 tidyverse         * 2.0.0   2023-02-22 [1] CRAN (R 4.3.0)
 timechange          0.2.0   2023-01-11 [1] CRAN (R 4.3.0)
 tm                  0.7-11  2023-02-05 [1] CRAN (R 4.3.0)
 tokenizers          0.3.0   2022-12-22 [1] CRAN (R 4.3.0)
 topicmodels         0.2-14  2023-03-31 [1] CRAN (R 4.3.0)
 tzdb                0.4.0   2023-05-12 [1] CRAN (R 4.3.0)
 utf8                1.2.3   2023-01-31 [1] CRAN (R 4.3.0)
 vctrs               0.6.3   2023-06-14 [1] CRAN (R 4.3.0)
 vegan             * 2.6-4   2022-10-11 [1] CRAN (R 4.3.0)
 vroom               1.6.3   2023-04-28 [1] CRAN (R 4.3.0)
 withr               2.5.0   2022-03-03 [1] CRAN (R 4.3.0)
 writexl             1.4.2   2023-01-06 [1] CRAN (R 4.3.0)
 xfun                0.40    2023-08-09 [1] CRAN (R 4.3.0)
 xml2                1.3.5   2023-07-06 [1] CRAN (R 4.3.0)
 xtable              1.8-4   2019-04-21 [1] CRAN (R 4.3.0)
 ```

