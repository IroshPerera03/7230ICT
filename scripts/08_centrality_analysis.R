# ============================================================================
# Q8: Centrality Analysis - Artist Co-mention Network
# ============================================================================

library(igraph)
library(dplyr)
library(ggplot2)
library(tidyr)  # ADD THIS

load("data/raw/youtube_raw.RData")
load("data/raw/reddit_raw.RData")

cat("Starting centrality analysis...\n")

# Combine and clean text
all_text <- tolower(c(youtube_data$text, reddit_comments$comment))

# List of artists to track
artists_list <- c(
  "ed sheeran",
  "taylor swift",
  "shawn mendes",
  "john mayer",
  "james blunt",
  "damien rice",
  "passenger",
  "bruno mars",
  "coldplay",
  "one direction",
  "harry styles",
  "niall horan",
  "justin bieber",
  "charlie puth",
  "sam smith"
)

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
    mention_list[[artist]] <- data.frame(
      doc_id = docs_with_artist,
      artist = artist,
      stringsAsFactors = FALSE
    )
  }
}

mentions_df <- bind_rows(mention_list)

cat("Found", nrow(mentions_df), "artist mentions in", 
    length(unique(mentions_df$doc_id)), "documents\n")

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
  
  # Save and create simple visualization
  dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)
  
  png("report/figures/q8_artist_mentions.png", width = 900, height = 600, res = 150)
  ggplot(artist_mentions, aes(x = reorder(artist, mentions), y = mentions)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Artist Mentions in Ed Sheeran Discussions",
         x = "Artist", y = "Number of Mentions")
  dev.off()
  
  save(artist_mentions, mentions_df, 
       file = "data/processed/q8_artist_mentions.RData")
  
  cat("\n✓ Q8 Complete (mention frequency only)\n")
  cat("\nFor Q8 report: Explain that co-mention network could not be\n")
  cat("constructed because comments discuss Ed Sheeran in isolation.\n")
  cat("Present mention frequency analysis instead.\n")
  
} else {
  
  # Build network
  artist_network <- graph_from_data_frame(edges, directed = FALSE)
  artist_network <- simplify(artist_network, edge.attr.comb = "sum")
  
  cat("Network:", vcount(artist_network), "nodes,", ecount(artist_network), "edges\n")
  
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
  
  # Visualizations
  dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)
  
  png("report/figures/q8_centrality_comparison.png", 
      width = 1200, height = 400, res = 150)
  par(mfrow = c(1, 3), mar = c(10, 4, 3, 2))
  
  top10_deg <- head(centrality_df %>% arrange(desc(Degree)), 10)
  barplot(top10_deg$Degree, 
          names.arg = top10_deg$Artist,
          las = 2, cex.names = 0.6, 
          main = "Degree Centrality", 
          col = "steelblue",
          ylab = "Connections")
  
  top10_bet <- head(centrality_df %>% arrange(desc(Betweenness)), 10)
  barplot(top10_bet$Betweenness,
          names.arg = top10_bet$Artist,
          las = 2, cex.names = 0.6, 
          main = "Betweenness Centrality", 
          col = "coral",
          ylab = "Bridge Score")
  
  top10_clo <- head(centrality_df %>% arrange(desc(Closeness)), 10)
  barplot(top10_clo$Closeness,
          names.arg = top10_clo$Artist,
          las = 2, cex.names = 0.6, 
          main = "Closeness Centrality", 
          col = "seagreen",
          ylab = "Proximity")
  dev.off()
  
  # Network plot
  png("report/figures/q8_artist_network.png", 
      width = 1000, height = 1000, res = 150)
  set.seed(42)
  plot(artist_network,
       vertex.size = degree_cent * 5,
       vertex.label.cex = 0.8,
       vertex.color = ifelse(V(artist_network)$name == "ed sheeran", 
                             "red", "lightblue"),
       edge.width = E(artist_network)$weight / 2,
       edge.color = "gray70",
       layout = layout_with_fr(artist_network),
       main = "Artist Co-Mention Network")
  legend("topright", legend = c("Ed Sheeran", "Others"), 
         fill = c("red", "lightblue"))
  dev.off()
  
  save(artist_network, centrality_df, ed_scores, edges,
       file = "data/processed/q8_centrality.RData")
}