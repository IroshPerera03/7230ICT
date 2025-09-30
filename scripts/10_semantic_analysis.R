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

# Emotion distribution
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)

png("report/figures/q10_emotion_comparison.png", 
    width = 1000, height = 600, res = 150)
combined_emotions <- combined_summary %>%
  select(platform, joy, trust, anticipation, sadness, anger, fear, surprise, disgust) %>%
  pivot_longer(cols = -platform, names_to = "emotion", values_to = "count")

ggplot(combined_emotions, aes(x = emotion, y = count, fill = platform)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "coral")) +
  labs(title = "Emotion Distribution: YouTube vs Reddit",
       subtitle = "Ed Sheeran Discussions",
       y = "Total Count", x = "Emotion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# Sentiment timeline (YouTube has timestamps)
if ("published_at" %in% colnames(yt_data)) {
  yt_data$date <- as.Date(yt_data$published_at)
  
  png("report/figures/q10_sentiment_timeline.png", 
      width = 1200, height = 600, res = 150)
  yt_data %>%
    group_by(date) %>%
    summarise(avg_polarity = mean(polarity, na.rm = TRUE),
              n = n()) %>%
    filter(n >= 3) %>%  # At least 3 comments per day
    ggplot(aes(x = date, y = avg_polarity)) +
    geom_line(color = "steelblue", size = 1) +
    geom_smooth(method = "loess", se = TRUE, color = "coral") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = "Sentiment Polarity Over Time (YouTube)",
         subtitle = "Ed Sheeran Comments",
         x = "Date", y = "Average Polarity")
  dev.off()
}

# Polarity distribution
png("report/figures/q10_polarity_distribution.png", 
    width = 1000, height = 600, res = 150)
bind_rows(
  yt_data %>% select(polarity) %>% mutate(platform = "YouTube"),
  reddit_data %>% select(polarity) %>% mutate(platform = "Reddit")
) %>%
  ggplot(aes(x = polarity, fill = platform)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("coral", "steelblue")) +
  labs(title = "Sentiment Polarity Distribution",
       x = "Polarity (Negative ← → Positive)",
       y = "Number of Comments")
dev.off()

save(yt_data, reddit_data, combined_summary,
     file = "data/processed/q10_sentiment.RData")
