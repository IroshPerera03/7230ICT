# ============================================================================
# Q8: Centrality Analysis - Artist Co-mention Network
# ============================================================================
library(igraph)
library(dplyr)
library(ggplot2)
library(tidyr)

load("data/raw/youtube_raw.RData")
load("data/raw/reddit_raw.RData")

cat("Starting centrality analysis...\n")
# Combine and clean text
all_text <- tolower(c(youtube_data$text, reddit_comments$comment))

# List of artists to track
artists_list <- c("ed sheeran","taylor swift","shawn mendes",
  "john mayer","james blunt","damien rice","passenger","bruno mars",
  "coldplay","one direction","harry styles","niall horan","justin bieber",
  "charlie puth","sam smith")

cat("Tracking", length(artists_list), "artists\n")

# Find artist mentions
mentions <- data.frame(
  doc_id = seq_along(all_text),
  text = all_text,
  stringsAsFactors = FALSE
)

# For each artist, find which documents mention them
mention_list <- list()
for (artist in artists_list) {
  docs_with_artist <- which(grepl(artist, mentions$text, fixed = TRUE))
  if (length(docs_with_artist) > 0) {
    mention_list[[artist]] <- data.frame(doc_id = docs_with_artist,
                                         artist = artist,
                                         stringsAsFactors = FALSE)
  }
}

mentions_df <- bind_rows(mention_list)

cat("Found",
    nrow(mentions_df),
    "artist mentions in",
    length(unique(mentions_df$doc_id)),
    "documents\n")

# Create co-mention edges
edges <- mentions_df %>%
  inner_join(mentions_df, by = "doc_id", relationship = "many-to-many") %>%
  filter(artist.x < artist.y) %>%
  count(artist.x, artist.y, name = "weight") %>%
  rename(from = artist.x, to = artist.y)

cat("Created", nrow(edges), "co-mention edges\n")

if (nrow(edges) == 0) {
  cat("\n⚠ WARNING: No co-mentions found.\n")
  cat("Comments focus only on Ed Sheeran without comparing to other artists.\n\n")
  
  # Show mention counts instead
  artist_mentions <- mentions_df %>%
    count(artist, sort = TRUE, name = "mentions")
  
  cat("=== ARTIST MENTION FREQUENCY ===\n")
  print(artist_mentions)
  
  save(artist_mentions, mentions_df, file = "data/processed/q8_artist_mentions.RData")
} else {
  # Build network
  artist_network <- graph_from_data_frame(edges, directed = FALSE)
  
  cat("Network:",
      vcount(artist_network),
      "nodes,",
      ecount(artist_network),
      "edges\n")
  
  # Calculate centralities
  degree_cent <- degree(artist_network)
  between_cent <- betweenness(artist_network, normalized = TRUE)
  close_cent <- closeness(artist_network, normalized = TRUE)
  
  centrality_df <- data.frame(
    Artist = V(artist_network)$name,
    Degree = degree_cent,
    Betweenness = round(between_cent, 4),
    Closeness = round(close_cent, 4)
  ) %>% arrange(desc(Degree))
  
  cat("\n=== CENTRALITY RESULTS ===\n")
  print(centrality_df)
  
  # Ed Sheeran's scores
  if ("ed sheeran" %in% centrality_df$Artist) {
    ed_scores <- centrality_df %>% filter(Artist == "ed sheeran")
    cat("\n=== Ed Sheeran's Centrality ===\n")
    print(ed_scores)
  }
  
  save(artist_network, centrality_df, ed_scores, edges, file = "data/processed/q8_centrality.RData")
}
