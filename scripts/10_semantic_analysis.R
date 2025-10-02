# ============================================================================
# Q10: Sentiment Analysis
# ============================================================================
library(syuzhet)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

load("data/raw/youtube_raw.RData")
load("data/raw/reddit_raw.RData")

cat("Starting sentiment analysis...\n")

# YouTube sentiment
cat("\nAnalyzing YouTube comments...\n")
yt_text <- youtube_data %>%
  filter(!is.na(text), nchar(text) > 10) %>%
  mutate(text_clean = as.character(text))

yt_sentiment <- get_nrc_sentiment(yt_text$text_clean)
yt_data <- cbind(yt_text, yt_sentiment) %>%
  mutate(polarity = positive - negative)

# Reddit sentiment
cat("Analyzing Reddit comments...\n")
reddit_text <- reddit_comments %>%
  filter(!is.na(comment), nchar(comment) > 10) %>%
  mutate(comment_clean = as.character(comment))

reddit_sentiment <- get_nrc_sentiment(reddit_text$comment_clean)
reddit_data <- cbind(reddit_text, reddit_sentiment) %>%
  mutate(polarity = positive - negative)

# Summaries
yt_summary <- yt_data %>%
  summarise(
    platform = "YouTube",
    total_comments = n(),
    positive = sum(positive),
    negative = sum(negative),
    joy = sum(joy),
    trust = sum(trust),
    anticipation = sum(anticipation),
    sadness = sum(sadness),
    anger = sum(anger),
    fear = sum(fear),
    surprise = sum(surprise),
    disgust = sum(disgust),
    avg_polarity = mean(polarity, na.rm = TRUE)
  )

reddit_summary <- reddit_data %>%
  summarise(
    platform = "Reddit",
    total_comments = n(),
    positive = sum(positive),
    negative = sum(negative),
    joy = sum(joy),
    trust = sum(trust),
    anticipation = sum(anticipation),
    sadness = sum(sadness),
    anger = sum(anger),
    fear = sum(fear),
    surprise = sum(surprise),
    disgust = sum(disgust),
    avg_polarity = mean(polarity, na.rm = TRUE)
  )

combined_summary <- bind_rows(yt_summary, reddit_summary)

cat("\n=== SENTIMENT SUMMARY ===\n")
print(combined_summary)

save(yt_data, reddit_data, combined_summary,
     file = "data/processed/q10_sentiment.RData")
