# ============================================================================
# Q6: Text Preprocessing and Term-Document Matrix Analysis
# Ed Sheeran Social Media Analytics
# ============================================================================

library(tm)
library(tidytext)
library(dplyr)
library(ggplot2)
library(stringr)

# Load raw data
load("data/raw/youtube_raw.RData")
load("data/raw/reddit_raw.RData")

cat("Starting text preprocessing...\n")

# 1. AGGRESSIVE TEXT CLEANING -----------------------------------------------
clean_text <- function(text) {
  text %>%
    as.character() %>%
    # Remove HTML tags
    str_remove_all("<.*?>") %>%
    # Remove HTML entities (&amp; &quot; &nbsp; etc.)
    str_remove_all("&[a-z]+;|&#[0-9]+;") %>%
    # Remove URLs
    str_remove_all("http\\S+|www\\.\\S+") %>%
    # Remove email addresses
    str_remove_all("\\S+@\\S+") %>%
    # Remove non-English characters (keep only ASCII)
    str_remove_all("[^\x01-\x7F]+") %>%
    # Remove standalone numbers
    str_remove_all("\\b\\d+\\b") %>%
    # Remove extra whitespace
    str_squish()
}

# Combine and clean all text
all_text_raw <- c(youtube_data$text, reddit_comments$comment)
all_text_clean <- clean_text(all_text_raw)

# Remove empty/very short texts
all_text_clean <- all_text_clean[nchar(all_text_clean) > 10]

cat("Cleaned", length(all_text_clean), "documents\n")

# 2. CREATE CORPUS AND APPLY TM PREPROCESSING -------------------------------
corpus <- VCorpus(VectorSource(all_text_clean))

# Custom stopwords - aggressive filtering
custom_stops <- c(
  "ed", "sheeran", "edsheeran", 
  "song", "songs", "music", "musical",
  "br", "amp", "quot", "href", "gt", "lt", "nbsp",
  "https", "http", "www"
)

# Preprocessing pipeline
corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, custom_stops) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)

cat("Corpus preprocessing complete\n")

# 3. CREATE TERM-DOCUMENT MATRIX -------------------------------------------
tdm <- TermDocumentMatrix(corpus_clean)

cat("TDM dimensions:", dim(tdm)[1], "terms x", dim(tdm)[2], "documents\n")

# 4. EXTRACT TOP 10 TERMS --------------------------------------------------
term_freq <- rowSums(as.matrix(tdm))
term_freq_sorted <- sort(term_freq, decreasing = TRUE)

# Additional filter: minimum 3 characters, not pure noise
meaningful_terms <- term_freq_sorted[
  nchar(names(term_freq_sorted)) >= 3 &
    !grepl("^[^a-z]+$", names(term_freq_sorted))  # Must contain letters
]

top10_terms <- head(meaningful_terms, 10)

cat("\n=== Q6 RESULTS: Top 10 Most Frequent Terms (Stemmed) ===\n")
print(top10_terms)

# Create dataframe for visualization
top10_df <- data.frame(
  term = names(top10_terms),
  frequency = as.numeric(top10_terms),
  row.names = NULL
)

# 5. VISUALIZATION ---------------------------------------------------------
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)

png("report/figures/q6_top10_terms.png", width = 900, height = 600, res = 150)
ggplot(top10_df, aes(x = reorder(term, frequency), y = frequency)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(aes(label = frequency), hjust = -0.2, size = 4) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  labs(
    title = "Top 10 Most Frequent Terms",
    subtitle = "Ed Sheeran Discussions (YouTube + Reddit) | After preprocessing & stemming",
    x = "Term (Stemmed)",
    y = "Frequency"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40")
  )
dev.off()

cat("\nVisualization saved to: report/figures/q6_top10_terms.png\n")

# 6. SAVE PROCESSED DATA ---------------------------------------------------
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# Save for Q7 to use
save(
  all_text_clean,           # Cleaned text (for Q7 bigrams)
  corpus_clean,             # Cleaned corpus
  tdm,                      # Term-document matrix
  term_freq,                # All term frequencies
  top10_terms,              # Top 10 terms
  top10_df,                 # Top 10 dataframe
  custom_stops,             # Custom stopwords used
  file = "data/processed/q6_tdm.RData"
)

cat("\nProcessed data saved to: data/processed/q6_tdm.RData\n")
