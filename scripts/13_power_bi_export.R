# ============================================================================
# Q13: Power BI Data Export
# ============================================================================

library(dplyr)

cat("Exporting data for Power BI...\n")

dir.create("data/powerbi", recursive = TRUE, showWarnings = FALSE)

# 1. Term frequencies
load("data/processed/q6_tdm.RData")
term_freq_export <- data.frame(
  term = names(term_freq),
  frequency = as.numeric(term_freq)
) %>% arrange(desc(frequency)) %>% head(50)

write.csv(term_freq_export, "data/powerbi/term_frequencies.csv", row.names = FALSE)
cat("✓ term_frequencies.csv\n")

# 2. Sentiment data
if (file.exists("data/processed/q10_sentiment.RData")) {
  load("data/processed/q10_sentiment.RData")
  
  sentiment_export <- bind_rows(
    yt_data %>% select(platform = video_id, text, polarity, 
                       positive, negative, joy, trust) %>%
      mutate(platform = "YouTube") %>% head(1000),
    reddit_data %>% select(platform = thread_subreddit, comment, polarity,
                           positive, negative, joy, trust) %>%
      rename(text = comment) %>% mutate(platform = "Reddit") %>% head(1000)
  )
  
  write.csv(sentiment_export, "data/powerbi/sentiment_data.csv", row.names = FALSE)
  cat("✓ sentiment_data.csv\n")
}

# 3. Topic data
if (file.exists("data/processed/q12_lda_topics.RData")) {
  load("data/processed/q12_lda_topics.RData")
  
  topic_export <- top_terms %>%
    group_by(topic) %>%
    summarise(top_terms = paste(head(term, 5), collapse = ", "),
              avg_beta = mean(beta))
  
  write.csv(topic_export, "data/powerbi/topics.csv", row.names = FALSE)
  cat("✓ topics.csv\n")
}

# 4. Engagement data
load("data/raw/reddit_raw.RData")
engagement_export <- reddit_comments %>%
  mutate(score_numeric = as.numeric(score),
         comment_length = nchar(comment)) %>%
  select(author, score = score_numeric, comment_length, 
         subreddit = thread_subreddit) %>%
  filter(!is.na(score)) %>%
  head(2000)

write.csv(engagement_export, "data/powerbi/reddit_engagement.csv", row.names = FALSE)
cat("✓ reddit_engagement.csv\n")

# 5. Centrality data (if exists)
if (file.exists("data/processed/q8_centrality.RData")) {
  load("data/processed/q8_centrality.RData")
  if (exists("centrality_df")) {
    write.csv(centrality_df, "data/powerbi/artist_centrality.csv", row.names = FALSE)
    cat("✓ artist_centrality.csv\n")
  }
}

# 6. Bigram network data
if (file.exists("data/processed/q7_semantic_network.RData")) {
  load("data/processed/q7_semantic_network.RData")
  
  if (exists("bigrams")) {
    write.csv(bigrams, "data/powerbi/bigrams.csv", row.names = FALSE)
    cat("✓ bigrams.csv\n")
  }
  
  if (exists("top10_pagerank_df")) {
    write.csv(top10_pagerank_df, "data/powerbi/bigram_pagerank.csv", row.names = FALSE)
    cat("✓ bigram_pagerank.csv\n")
  }
}

cat("\nAll data exported to data/powerbi/\n")
cat("\nNext steps:\n")
cat("1. Open Power BI Desktop\n")
cat("2. Get Data > Text/CSV > Import all CSV files\n")
cat("3. Create 2 dashboard pages with 3+ charts each\n")
cat("4. Export as .pbix and take screenshots\n")