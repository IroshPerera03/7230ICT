# ============================================================================
# Q4: Unique Actors Count
# ============================================================================

library(dplyr)

load("data/raw/youtube_raw.RData")
load("data/raw/reddit_raw.RData")

# YouTube unique actors
yt_unique <- youtube_data %>%
  filter(!is.na(author)) %>%
  pull(author) %>%
  n_distinct()

# Reddit unique actors
reddit_unique <- reddit_comments %>%
  filter(!is.na(author)) %>%
  pull(author) %>%
  n_distinct()

# Combined
all_actors <- c(youtube_data$author, reddit_comments$author) %>% unique() %>% na.omit()

total_unique <- length(all_actors)

# Create summary
unique_summary <- data.frame(
  Platform = c("YouTube", "Reddit", "Combined"),
  Unique_Actors = c(yt_unique, reddit_unique, total_unique),
  Total_Comments = c(
    nrow(youtube_data),
    nrow(reddit_comments),
    nrow(youtube_data) + nrow(reddit_comments)
  )
) %>%
  mutate(Comments_per_Actor = Total_Comments / Unique_Actors)

print(unique_summary)

# Save
save(unique_summary, yt_unique, reddit_unique, total_unique, file = "data/processed/q4_unique_actors.RData")
