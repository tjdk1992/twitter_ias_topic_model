#-----------------------------------------------------------------------------#
# Script Name: 02-tweet-retrieve                                              #
#                                                                             #
# Author: Daiki Tomojiri                                                      #
# Email: tomojiri.daiki@gmail.com                                             #
#                                                                             #
# This R script retrieve tweet data via twitter academic API v2.              #
#-----------------------------------------------------------------------------#

# Setup -----------------------------------------------------------------------

# Initialization
rm(list = ls())
gc(); gc();

# Package
pacman::p_load(tidyverse,       # for data manipulation
               academictwitteR  # for handling twitter
               )

# Data
dat_ias_ja <- read_csv("data/basic-ias-info.csv")

# Retrieve tweets -------------------------------------------------------------

# Check the list of IAS
str(dat_ias_ja) # KATAKANAが検索対象のTermである。

# Prepare bearer token
bearer_token <- "<ADD MY BEARER TOKEN>" # This is a secret sequence.

# Prepare search query
ias_ja <- dat_ias_ja$KATAKANA
query_common <- "(移入 OR 帰化 OR 外来 OR 侵入)"
list_ias_tweet <- list()
for (i in 1:length(ias_ja)) {
  ias_all_ja <- unlist(dat_ias_ja$KATAKANA, use.name = FALSE)
  ias_all_code <- unlist(dat_ias_ja$code_ias, use.name = FALSE)
  name_ias <- ias_all_ja[i]
  code_ias <- ias_all_code[i]
  t_query <- str_c(query_common, " ", name_ias)
  t_path <- str_c("data-raw/doc-tweet-ias/doc-tweet-", code_ias)
  tweet_ias <- 
    get_all_tweets(query = t_query,
                   start_tweets = "2008-01-01T00:00:00Z",
                   end_tweets = "2023-05-01T00:00:00Z",
                   bearer_token = bearer_token,
                   data_path = t_path,
                   bind_tweets = TRUE,
                   n = 1000000) # 本番は10,000,000
  list_ias_tweet[[i]] <- tweet_ias
}

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

# Export data -----------------------------------------------------------------

# Write in csv format
tweet_df_binded %>% 
  mutate(id_raw = row_number()) %>% 
  write_csv("data/tweet-01_binded.csv")
