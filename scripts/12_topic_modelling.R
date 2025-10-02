# ============================================================================
# Q12: LDA Topic Modeling
# ============================================================================
library(topicmodels)
library(tidytext)
library(tm)
library(dplyr)
library(ggplot2)

load("data/processed/q6_tdm.RData")

cat("Preparing data for topic modeling...\n")

# Convert TDM to DTM
dtm <- as.DocumentTermMatrix(tdm)

# Remove sparse terms
dtm_clean <- removeSparseTerms(dtm, 0.98)

# Remove empty documents
row_sums <- apply(dtm_clean, 1, sum)
dtm_clean <- dtm_clean[row_sums > 0, ]

cat("DTM dimensions:", nrow(dtm_clean), "docs x", ncol(dtm_clean), "terms\n")

# Run LDA with k=7
set.seed(42)
k <- 7
cat("\nRunning LDA with k =", k, "topics...\n")

lda_model <- LDA(dtm_clean, k = k, control = list(seed = 42))

cat("LDA complete\n")

# Extract topics
lda_topics <- tidy(lda_model, matrix = "beta")

# Top 10 terms per topic
top_terms <- lda_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

cat("\n=== TOP TERMS BY TOPIC ===\n")
print(top_terms)

# Topic proportions
doc_topics <- tidy(lda_model, matrix = "gamma")

topic_proportions <- doc_topics %>%
  group_by(topic) %>%
  summarise(avg_gamma = mean(gamma)) %>%
  arrange(desc(avg_gamma))

cat("\n=== TOPIC PROPORTIONS ===\n")
print(topic_proportions)

topic_labels <- data.frame(
  topic = 1:k,
  label = paste("Topic", 1:k, "- [Interpret based on top terms]")
)


save(lda_model, lda_topics, top_terms, doc_topics, 
     topic_proportions, topic_labels,
     file = "data/processed/q12_lda_topics.RData")