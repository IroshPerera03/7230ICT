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

save(gn_communities, louvain_communities, 
     gn_membership, louvain_membership,
     gn_df, louvain_df, 
     gn_modularity, louvain_modularity,
     file = "data/processed/q9_communities.RData")

cat("\nQ9 Complete.\n")
