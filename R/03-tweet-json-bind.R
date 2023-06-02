#-----------------------------------------------------------------------------#
# Script Name: 03-tweet-json-bind                                             #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script binds JSON-format tweets and converts it into data frame.     #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Packages
pacman::p_load(tidyverse,
               readxl,
               lubridate,
               academictwitteR,
               stringi)

# Bind JSON into data frame ---------------------------------------------------

# Prepare essential data
path_dir_doc <- "data-raw/doc-tweet-ias"
dataframe_paths <- list.files(path = path_dir_doc, full.names = T)

# tweetのJSONをdata.frameにbinding
tweet_df_binded <- data.frame()
for (i in 1:length(dataframe_paths)) {
  t_tweet_tidy_binded <- data.frame() # 仮のデータフレーム
  path_dir_ias <- str_c(dataframe_paths[i], "/") # bindするJSONのパス
  # bind_tweets()でJSONをbindしてdfへ
  t_tweet_df_binded <- try(bind_tweets(data_path = path_dir_ias),
                           silent = FALSE)
  # エラー対応
  if (nrow(t_tweet_df_binded)!= 0 & class(t_tweet_df_binded) != "try-error") {
    # listとdfで入れ子になっているreference_tweetsを取り出す
    t_referenced_tweets <- data.frame() # 仮のデータフレーム
    t_list <- data.frame() # 仮のデータフレーム
    for (j in 1:length(t_tweet_df_binded$text)) {
      if (is.null(unlist(t_tweet_df_binded$referenced_tweets[[j]]))) {
        # 空の場合は各列を明示的に作成してNAを入れる。
        t_list <- data.frame(type = NA, id_ref = NA)
      } else {
        # 中身がある場合はtypeとidを取り出す
        t_list <- data.frame(
          type = unlist(t_tweet_df_binded$referenced_tweets[[j]][1], 
                        use.names = FALSE),
          id_ref = unlist(t_tweet_df_binded$referenced_tweets[[j]][2], 
                          use.names = FALSE))
        # 稀にquotedかつretweetedのように複数行あるので、それを1行にまとめる
        if (nrow(t_list) != 1) {
          t_list <- data.frame(
            type = paste(t_list$type, collapse = "-"),
            id_ref = paste(t_list$id_ref, collapse = "-"))
        }
      }
      t_referenced_tweets <- rbind(t_referenced_tweets, t_list)
    }
    t_public_metrics <- bind_rows(t_tweet_df_binded$public_metrics)
    t_tweet_df_binded <- t_tweet_df_binded %>% 
      transmute(id, author_id, conversation_id, created_at, lang, text,
                MyFile = dataframe_paths[i]) %>% 
      cbind(t_public_metrics, t_referenced_tweets)
    tweet_df_binded <- bind_rows(tweet_df_binded, t_tweet_df_binded) 
  }
}

# Write data
write_csv(tweet_df_binded, "data/tweet_01-binded.csv")

# Bind JSON into tidy tibble --------------------------------------------------

# Not run.

###############################################################################
# output_format = "tidy"では、以下のエラー出るのでコードは参考
## Error in `dplyr::rename()`:
##   ! Problem while evaluating `tidyselect::all_of(pkicol)`.
## Caused by error in `tidyselect::all_of()`:
##   ! Can't subset elements that don't exist.
## ✖ Element `id` doesn't exist.
###############################################################################

# # Prepare essential data
# path_dir_doc <- "data-raw/doc-tweet-ias"
# dataframe_paths <- list.files(path = path_dir_doc, full.names = T)
# 
# # Binding JSON
# tweet_tidy_binded <- tibble()
# for (i in 1:length(dataframe_paths)) {
#   t_tweet_tidy_binded <- tibble()
#   path_dir_ias <- str_c(dataframe_paths[i], "/")
#   t_tweet_tidy_binded <- try(bind_tweets(data_path = path_dir_ias,
#                                          output_format = "tidy"),
#                              silent = FALSE)
#   if (class(t_tweet_tidy_binded)[[1]] != "try-error") {
#     t_tweet_tidy_binded$sourceFile <- dataframe_paths[i]
#     tweet_tidy_binded <- bind_rows(tweet_tidy_binded, t_tweet_tidy_binded)
#   }
# }
# 
# # Rename column of IAS code
# tweet_tidy_binded <- tweet_tidy_binded %>% 
#   mutate(code_ias = str_replace_all(sourceFile,
#                                     "data-raw/doc-tweet-ias/doc-tweet-",
#                                     "")) %>% 
#   dplyr::select(-sourceFile)
# 
# # Write data
# write_csv(tweet_tidy_binded, "data/tweet-binded-tidy.csv")
