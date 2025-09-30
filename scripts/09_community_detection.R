# ============================================================================
# Q9: Community Detection
# ============================================================================

library(igraph)
library(dplyr)

cat("Loading Q8 network data...\n")

# Check if network exists
if (!file.exists("data/processed/q8_centrality.RData")) {
  cat("ERROR: Run Q8 first to create artist network\n")
  quit(save = "no")
}

load("data/processed/q8_centrality.RData")

# Check if network has enough nodes
if (!exists("artist_network") || vcount(artist_network) < 3) {
  cat("Network too small for community detection.\n")
  cat("Need at least 3 connected artists.\n")
  cat("Skip Q9 or explain in report why it's not applicable.\n")
  
  # Save empty result
  save(list = character(0), file = "data/processed/q9_communities.RData")
  quit(save = "no")
}

cat("Network has", vcount(artist_network), "nodes\n")

# Girvan-Newman
cat("\nRunning Girvan-Newman...\n")
gn_communities <- cluster_edge_betweenness(artist_network)
gn_membership <- membership(gn_communities)
gn_modularity <- modularity(gn_communities)

cat("Found", max(gn_membership), "communities\n")
cat("Modularity:", round(gn_modularity, 3), "\n")

# Louvain
cat("\nRunning Louvain...\n")
louvain_communities <- cluster_louvain(artist_network)
louvain_membership <- membership(louvain_communities)
louvain_modularity <- modularity(louvain_communities)

cat("Found", max(louvain_membership), "communities\n")
cat("Modularity:", round(louvain_modularity, 3), "\n")

# Community membership
gn_df <- data.frame(
  Artist = V(artist_network)$name,
  Community = gn_membership
) %>% arrange(Community, Artist)

louvain_df <- data.frame(
  Artist = V(artist_network)$name,
  Community = louvain_membership
) %>% arrange(Community, Artist)

cat("\n=== GIRVAN-NEWMAN COMMUNITIES ===\n")
print(gn_df)

cat("\n=== LOUVAIN COMMUNITIES ===\n")
print(louvain_df)

# Ed Sheeran's community
if ("ed sheeran" %in% V(artist_network)$name) {
  ed_gn <- gn_membership[V(artist_network)$name == "ed sheeran"]
  ed_louvain <- louvain_membership[V(artist_network)$name == "ed sheeran"]
  
  cat("\nEd Sheeran in GN community:", ed_gn, "\n")
  cat("Members:\n")
  print(gn_df %>% filter(Community == ed_gn))
  
  cat("\nEd Sheeran in Louvain community:", ed_louvain, "\n")
  cat("Members:\n")
  print(louvain_df %>% filter(Community == ed_louvain))
}

# Visualizations
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)

png("report/figures/q9_communities_gn.png", width = 1000, height = 1000, res = 150)
set.seed(42)
plot(gn_communities, artist_network,
     vertex.label.cex = 0.8,
     vertex.size = 12,
     main = "Girvan-Newman Communities")
dev.off()

png("report/figures/q9_communities_louvain.png", width = 1000, height = 1000, res = 150)
set.seed(42)
plot(louvain_communities, artist_network,
     vertex.label.cex = 0.8,
     vertex.size = 12,
     main = "Louvain Communities")
dev.off()

save(gn_communities, louvain_communities, 
     gn_membership, louvain_membership,
     gn_df, louvain_df, 
     gn_modularity, louvain_modularity,
     file = "data/processed/q9_communities.RData")

cat("\nQ9 Complete.\n")
