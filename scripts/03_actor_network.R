library(igraph)
library(tidygraph)
library(ggraph)
library(dplyr)

# --- Load data ---
load("data/raw/youtube_raw.RData")
load("data/raw/reddit_raw.RData")       

# ===== YOUTUBE ACTOR NETWORK =====
cat("YouTube columns:\n")
print(colnames(youtube_data))

# Build co-commenting edges: authors on same video
youtube_edges <- youtube_data %>%
  filter(!is.na(author)) %>%
  select(video_id, from = author) %>%
  inner_join(
    youtube_data %>% filter(!is.na(author)) %>% select(video_id, to = author),
    by = "video_id"
  ) %>%
  filter(from != to) %>%
  distinct(from, to)

cat("YouTube edge count:", nrow(youtube_edges), "\n")

yt_graph <- graph_from_data_frame(youtube_edges, directed = FALSE)

yt_pagerank <- page_rank(yt_graph)$vector
yt_top5 <- sort(yt_pagerank, decreasing = TRUE)[1:5]

cat("\n📺 YouTube Top 5 Actors by PageRank:\n")
print(yt_top5)

# ===== REDDIT ACTOR NETWORK =====
cat("\nReddit columns:\n")
print(colnames(reddit_comments))

reddit_edges <- reddit_comments %>%
  filter(!is.na(author)) %>%
  select(thread_title, from = author) %>%
  inner_join(
    reddit_comments %>% filter(!is.na(author)) %>% select(thread_title, to = author),
    by = "thread_title"
  ) %>%
  filter(from != to) %>%
  distinct(from, to)

cat("Reddit edge count:", nrow(reddit_edges), "\n")

reddit_graph <- graph_from_data_frame(reddit_edges, directed = FALSE)

reddit_pagerank <- page_rank(reddit_graph)$vector
reddit_top5 <- sort(reddit_pagerank, decreasing = TRUE)[1:5]

cat("\n👽 Reddit Top 5 Actors by PageRank:\n")
print(reddit_top5)

# ===== VISUALIZATION =====
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)

# YouTube graph
png("report/figures/q3_youtube_top_actors.png", width = 800, height = 600, res = 150)
yt_graph %>%
  as_tbl_graph() %>%
  mutate(pagerank = page_rank(yt_graph)$vector) %>%
  activate(nodes) %>%
  top_n(20, pagerank) %>%
  ggraph(layout = "fr") +
  geom_edge_link(alpha = 0.2) +
  geom_node_point(aes(size = pagerank), color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  labs(title = "YouTube Actor Network - Top 20 by PageRank")
dev.off()

# Reddit graph
png("report/figures/q3_reddit_top_actors.png", width = 800, height = 600, res = 150)
reddit_graph %>%
  as_tbl_graph() %>%
  mutate(pagerank = page_rank(reddit_graph)$vector) %>%
  activate(nodes) %>%
  top_n(20, pagerank) %>%
  ggraph(layout = "fr") +
  geom_edge_link(alpha = 0.2) +
  geom_node_point(aes(size = pagerank), color = "coral") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  labs(title = "Reddit Actor Network - Top 20 by PageRank")
dev.off()

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
# ===== SAVE =====
save(
  yt_graph, yt_pagerank, yt_top5,
  reddit_graph, reddit_pagerank, reddit_top5,
  file = "data/processed/q3_actor_networks.RData"
)
