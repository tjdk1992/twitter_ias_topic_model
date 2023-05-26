# Quantitative assessment of relative topics and occurrence of invasive alien species in the Twitter

**Author**: Daiki Tomojiri & Kosuke Takaya (2023)

**Publication**: _Conservation Biology_ (current status: under analysis)

**Keywords**: invasive alien species, conservation culturomics, twitter, text mining, topic modelling

## Abstract
Twitterは、スマートフォンを持っていればあらゆる個人が意見を発信できるSNSである。

## Analysis Design
data collection -

data preprocessing -

## Contents of the Repository

| Directory | Type | File Name | Description |
|:---|:---|:---|:---|
|data-raw |Spreadsheet |all-japan-wild-bird.xlsx |3 |
|data-raw |Spreadsheet |bird-biol-ecol-trait.xlsx |3 |
|data-raw |Spreadsheet |bird-list-exist-wikipedia.xlsx |3 |
|data-raw |Spreadsheet |all-japan-wild-bird.xlsx |3 |
|data-raw |Spreadsheet |bird-redlist-category.csv |3 |
|data |Spreadsheet |all-bird-wp-created.csv |3 |
|data |Spreadsheet |all-bird-wpv-analysis.csv |3 |
|data |Spreadsheet |all-bird-wpv.csv |3 |
|data |Spreadsheet |dat-main-analysis.csv |3 |
|R |R script |01-tweet-data-collection.R |script for XXXX |
|R |R script |02-data-preparation.R |script for XXXX |
|R |R script |03-general-preprocessing.R |script for XXXX |
|R |R script |04-ias-occurrence-search.R |script for XXXX |
|R |R script |05-ias-occrrence-analysis.R |script for XXXX |
|R |R script |06-lda-preprocessing.R |script for XXXX |
|R |R script |07-original-stopword-examination.R |script for XXXX |
|R |R script |08-lda-param-tuning-automatic.R |script for XXXX |
|R |R script |09-lda-param-tuning-manual.R |script for XXXX |
|R |R script |10-lda-model-inference.R |script for XXXX |
|R |R script |10-lda-posterior-analysis.R |script for XXXX |
|R |R script |11-lda-posterior-analysis(effective_ver).R |script for XXXX |
|R |R script |12-ias-occur-lda-modeling.R |script for XXXX |
|R |R script |13-publish-figure-creation.R |script for XXXX |
|R |R script |14-publish-table-creation.R |script for XXXX |
|fig |figure |Table-01 |Mathematical notation throughout this study |
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
