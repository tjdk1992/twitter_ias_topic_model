# Aspects of public attention on popular nonindigenous species, as determined by a comprehensive assessment of Japanese social media

**Author**: Daiki Tomojiri & Kosuke Takaya (2023)

**Publication**: _Biological Conservation_ (current status: under submission)

**Keywords**: invasive alien species, conservation culturomics, twitter, text mining, topic modelling

## Abstract
Invasion culturomics is an emerging field of study that utilizes digital data existing on the Internet to reveal the human dimension of nonindigenous species (NIS) invasions. Although hypothetical approaches have been used to examine explanatory variables that predict the amount of public attention by using proxies, it has been difficult to observe direct associations between these variables. Here, we aimed to deepen our understanding of the relationship between people and NIS by analyzing the content of texts about NIS on social media, and by clarifying the context and aspects to which public attention is directed. Specifically, we quantified tweets about NIS to identify popular NIS that attract a lot of public attention on Twitter. We also identified hidden topics in which NIS names occurred by applying topic modeling to tweets, and we investigated the topic distribution over popular NIS. A relatively small number of species were selected as popular NIS for further analysis, and 25 hidden topics were identified from all the tweets used in the analysis. The topic distribution over popular NIS had three patterns across taxonomic groups: (1) biased among topics but consistent within taxonomic groups; (2) relatively even among topics and consistent within taxonomic groups; and (3) not consistent within taxonomic groups and with biases differing among species. These findings can provide important insights into the formulation of a better strategy for NIS management by approaching the social and human dimensions of NIS invasion; such a strategy would include information dissemination, environmental education, and management campaigns. Our methodological framework can be used to address other important environmental topics that are attracting public attention and in which the objects have similar compositions (e.g., species of disease pathogens or types of marine debris).

## Contents of the Repository

| Directory | Type | File Name | Description |
|:---|:---|:---|:---|
|R  |R script |01-nis-list-compilation.R  |script for XXXX |
|R  |R script |02-tweet-collection.R      |script for XXXX |
|R  |R script |03-tweet-filtering.R       |script for XXXX |
|R  |R script |04-tweet-screening.R  |script for XXXX |
|R  |R script |05-tweet-cleansing.R  |script for XXXX |
|R  |R script |06-tweet-tokenization.R |script for XXXX |
|R  |R script |07-token-cleansing.R  |script for XXXX |
|R  |R script |08-token-cutting.R  |script for XXXX |
|R  |R script |09-stopword-basic.R |script for XXXX |
|R  |R script |10-stopword-general.R |script for XXXX |
|R  |R script |11-stopword-original.R  |script for XXXX |
|R  |R script |12-lda-trial-examination.R  |script for XXXX |
|R  |R script |13-lda-tuning-automatic.R |script for XXXX |
|R  |R script |14-lda-tuning-manual.R  |script for XXXX |
|R  |R script |15-lda-model-inference.R  |script for XXXX |
|R  |R script |16-lda-topic-overview.R |script for XXXX |
|R  |R script |17-tweet-ias-occurrence.R |script for XXXX |
|R  |R script |18-count-topic-assoc.R  |script for XXXX |
|R  |R script |19-topic-dissimilarity.R  |script for XXXX |
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

