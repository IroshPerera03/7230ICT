# ============================================================================
# Q7: Semantic (Bigram) Network Analysis
# ============================================================================

library(tidytext)
library(igraph)
library(ggraph)
library(dplyr)
library(ggplot2)
library(tidyr)

cat("Loading preprocessed data from Q6...\n")
load("data/processed/q6_tdm.RData")
cat("Loaded", length(all_text_clean), "cleaned documents\n")

# 1. CREATE BIGRAMS --------------------------------------------------------
cat("\nCreating bigrams...\n")

text_df <- data.frame(
  text = all_text_clean,
  stringsAsFactors = FALSE
) %>% 
  filter(!is.na(text), nchar(text) > 15)  # Filter very short texts

# Tokenize into bigrams
bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ", remove = FALSE) %>%
  filter(
    # Remove stopwords (including custom from Q6)
    !word1 %in% c(stop_words$word, custom_stops),
    !word2 %in% c(stop_words$word, custom_stops),
    # Remove NA
    !is.na(word1), !is.na(word2),
    # Minimum length 3 characters
    nchar(word1) >= 3, nchar(word2) >= 3,
    # Not pure numbers
    !grepl("^[0-9]+$", word1),
    !grepl("^[0-9]+$", word2)
  ) %>%
  count(word1, word2, sort = TRUE)

cat("Total unique bigrams:", nrow(bigrams), "\n")

# 2. CREATE NETWORK --------------------------------------------------------
# Filter: keep bigrams that appear at least 5 times
bigram_graph <- bigrams %>%
  filter(n >= 5) %>%
  graph_from_data_frame(directed = TRUE)

cat("Network nodes:", vcount(bigram_graph), "\n")
cat("Network edges:", ecount(bigram_graph), "\n")

# 3. CALCULATE PAGERANK ----------------------------------------------------
bigram_pagerank <- page_rank(bigram_graph)$vector
top10_pagerank <- sort(bigram_pagerank, decreasing = TRUE)[1:10]

cat("\n=== Q7 RESULTS: Top 10 Terms by PageRank ===\n")
print(top10_pagerank)

# 4. COMPARE WITH Q6 FREQUENCY RANKING -------------------------------------
comparison_df <- data.frame(
  Rank = 1:10,
  Frequency_Based_Q6 = names(top10_terms),
  PageRank_Based_Q7 = names(top10_pagerank)
)

cat("\n=== COMPARISON: Frequency (Q6) vs PageRank (Q7) ===\n")
print(comparison_df)

# 5. SAVE -------------------------------------------------------
save(
  bigrams,
  bigram_graph,
  bigram_pagerank,
  top10_pagerank,
  comparison_df,
  top10_pagerank_df,
  file = "data/processed/q7_semantic_network.RData"
)

cat("\nQ7 results saved to: data/processed/q7_semantic_network.RData\n")

# 7. ANALYSIS SUMMARY ------------------------------------------------------
cat("\n========================================\n")
cat("Q7 ANALYSIS SUMMARY\n")
cat("========================================\n")
cat("Bigrams analyzed:", nrow(bigrams), "\n")
cat("Network size:", vcount(bigram_graph), "nodes,", ecount(bigram_graph), "edges\n")
