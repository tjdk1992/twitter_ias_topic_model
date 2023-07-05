#-----------------------------------------------------------------------------#
# Script Name: 01-ias-info-compilation                                        #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script prepare the data frame containing IAS information             #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Package
pacman::p_load(tidyverse,
               readxl,
               stringi
               )

# Data
dat_ias_ja <- read_excel("resource/20220217_侵入生物DB_解析対象_最終版.xlsx",
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

# Write data
write_csv(dat_ias_ja, "data/basic-ias-info.csv")
