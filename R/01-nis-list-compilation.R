#-----------------------------------------------------------------------------#
# Script Name: 01-nis-list-compilation.R                                      #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Package
pacman::p_load(tidyverse,
               readxl,
               stringi,
               writexl
)

# Data
dat_ias_ja <- read_excel("resource/20220217_IAS-DB-finalized.xlsx",
                         sheet = "target")

# Data manipulation -----------------------------------------------------------

# Check the structure
str(dat_ias_ja)

# Rename columns and create katakana
dat_ias_ja <- dat_ias_ja %>% 
  mutate(num_code1 = c(rep("0", 9), rep("", count(dat_ias_ja)-9)),
         num_code2 = c(rep("0", 99) , rep("", count(dat_ias_ja)-99))) %>% 
  transmute(code_ias = str_c("IAS", num_code1, num_code2, row_number()),
            group_biol = str_replace_all(Taxon,
                                         c("\\(insect\\)" = "",
                                           "\\(other\\)" = "")),
            taxon = 高次分類群,
            name_ja = 和名,
            KATAKANA = カタカナ名,
            name_ja_katakana = stri_trans_general(name_ja, "Fullwidth-Halfwidth"),
            katakana = stri_trans_general(KATAKANA, "Fullwidth-Halfwidth"),
            name_sp = 学名,
            reg1 = 法的扱い,
            reg2 = 外来生物法)

# Write data
write_csv(dat_ias_ja, "data/basic-ias-info.csv")

# For publication
dat_ias_ja <- read_csv("data/basic-ias-info.csv")

dat_ias_distinct <- dat_ias_ja %>% 
  transmute(name_ja, KATAKANA) %>% 
  group_by(name_ja) %>% 
  mutate(id_pivot = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = id_pivot, 
              values_from = KATAKANA)

dat_ias_distinct[is.na(dat_ias_distinct)] <- ""

dat_ias_distinct <- dat_ias_distinct %>% 
  mutate(name_ja,
         name_ref = str_c(`1`, `2`, `3`, `4`, `5`, sep = ","),
         name_ref = str_remove_all(name_ref, ",*$"),
         name_ref = str_replace_all(name_ref, ",", ", ")) %>% 
  dplyr::select(name_ja, name_ref)

table_ias_ja <- dat_ias_ja %>% 
  dplyr::select(group_biol, name_ja, name_sp) %>% 
  distinct() %>% 
  left_join(dat_ias_distinct, by = "name_ja") %>% 
  transmute(`Biological Group` = group_biol,
            Species = name_sp,
            `Japanese name` = name_ja,
            `Referred name` = name_ref)

# Write in xlsx format
write_xlsx(table_ias_ja, "table-suppl/table-ias-ja-list.csv")
