# R script for the analysis in:
# Tomojiri, D., Takaya, K. (2022) Quantitative assessment of relative topics
# and occurrence of invasive alien species in the Twitter.
# Submitted to "Conservation Biology"
#
# R Script 01: Retrieving tweets related to invasive alien species
#
# Author: Daiki Tomojiri
#
# Outline of the script:
## Step 1. Estimate the maximum number of tweeets / year
## Step 2. Retrieve all tweets from 2008-01-01 to 2022-12-31

# Setup -----------------------------------------------------------------------

# Load packages
library(tidyverse)           # for data manipulation
library(readxl)
library(stringi)

# Data
dat_ias_ja <- read_excel("data-raw/20220217_侵入生物DB_解析対象_最終版.xlsx",
                         sheet = "解析対象")

# Data manipulation -----------------------------------------------------------

# Check the structure
str(dat_ias_ja)

# Rename columns and create katakana
dat_ias_ja <- dat_ias_ja %>% 
  mutate(
    num_code1 = c(rep("0", 9), rep("", count(dat_ias_ja)-9)),
    num_code2 = c(rep("0", 99) , rep("", count(dat_ias_ja)-99))
    ) %>% 
  transmute(
    code_ias = str_c("IAS", num_code1, num_code2, row_number()),
    group_biol = Taxon,
    taxon = 高次分類群,
    name_ja = 和名,
    KATAKANA = カタカナ名,
    name_ja_katakana = stri_trans_general(name_ja, "Fullwidth-Halfwidth"),
    katakana = stri_trans_general(KATAKANA, "Fullwidth-Halfwidth"),
    name_sp = 学名,
    reg1 = 法的扱い,
    reg2 = 外来生物法
    )

# Save ------------------------------------------------------------------------

# Write data
write_csv(dat_ias_ja, "data/basic-ias-info.csv")
