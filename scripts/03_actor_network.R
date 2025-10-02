# ============================================================================
# Q3: Top 5 Actors by Page Rank
# ============================================================================

library(igraph)
library(tidygraph)
library(ggraph)
library(dplyr)

# --- Load data ---
load("data/raw/youtube_raw.RData")
load("data/raw/reddit_raw.RData")

# ===== YOUTUBE ACTOR NETWORK =====
# Build co-commenting edges: authors on same video
youtube_edges <- youtube_data %>%
  filter(!is.na(author)) %>%
  select(video_id, from = author) %>%
  inner_join(youtube_data %>% filter(!is.na(author)) %>% select(video_id, to = author),
             by = "video_id") %>%
  filter(from != to) %>%
  distinct(from, to)

cat("YouTube edge count:", nrow(youtube_edges), "\n")

yt_graph <- graph_from_data_frame(youtube_edges, directed = FALSE)

yt_pagerank <- page_rank(yt_graph)$vector
yt_top5 <- sort(yt_pagerank, decreasing = TRUE)[1:5]

cat("\nYouTube Top 5 Actors by PageRank:\n")
print(yt_top5)

# ===== REDDIT ACTOR NETWORK =====
reddit_edges <- reddit_comments %>%
  filter(!is.na(author)) %>%
  select(thread_title, from = author) %>%
  inner_join(reddit_comments %>% filter(!is.na(author)) %>% select(thread_title, to = author),
             by = "thread_title") %>%
  filter(from != to) %>%
  distinct(from, to)

cat("Reddit edge count:", nrow(reddit_edges), "\n")

reddit_graph <- graph_from_data_frame(reddit_edges, directed = FALSE)

reddit_pagerank <- page_rank(reddit_graph)$vector
reddit_top5 <- sort(reddit_pagerank, decreasing = TRUE)[1:5]

cat("\nReddit Top 5 Actors by PageRank:\n")
print(reddit_top5)

dir.create("data/processed",
           recursive = TRUE,
           showWarnings = FALSE)
# ===== SAVE =====
save(yt_graph,
     yt_pagerank,
     yt_top5,
     reddit_graph,
     reddit_pagerank,
     reddit_top5,
     file = "data/processed/q3_actor_networks.RData")
