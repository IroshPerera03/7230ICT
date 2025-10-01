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

# Visualization
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)

png("report/figures/q12_topic_terms.png", 
    width = 1400, height = 1000, res = 150)
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +
  theme_minimal() +
  labs(title = "LDA Topic Modeling: Ed Sheeran Discussions",
       subtitle = paste("k =", k, "topics"),
       x = "", y = "Beta (term probability)")
dev.off()

# Topic proportions
doc_topics <- tidy(lda_model, matrix = "gamma")

topic_proportions <- doc_topics %>%
  group_by(topic) %>%
  summarise(avg_gamma = mean(gamma)) %>%
  arrange(desc(avg_gamma))

cat("\n=== TOPIC PROPORTIONS ===\n")
print(topic_proportions)

png("report/figures/q12_topic_distribution.png", 
    width = 900, height = 600, res = 150)
ggplot(topic_proportions, aes(x = factor(topic), y = avg_gamma, 
                              fill = factor(topic))) +
  geom_col() +
  theme_minimal() +
  labs(title = "Average Topic Distribution",
       x = "Topic", y = "Average Proportion") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set3")
dev.off()

topic_labels <- data.frame(
  topic = 1:k,
  label = paste("Topic", 1:k, "- [Interpret based on top terms]")
)

cat("\nInterpret topics based on top terms and update labels in report\n")

save(lda_model, lda_topics, top_terms, doc_topics, 
     topic_proportions, topic_labels,
     file = "data/processed/q12_lda_topics.RData")