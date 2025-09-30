library(RedditExtractoR)
library(tidyverse)

# Ed Sheeran search terms
search_terms <- c(
  "Ed Sheeran",
  "Ed Sheeran Perfect",
  "Ed Sheeran Shape of You",
  "Ed Sheeran concert",
  "Ed Sheeran new album"
)

# Relevant subreddits for Ed Sheeran
subreddits <- c(
  "EdSheeran"          # Dedicated Ed Sheeran subreddit
)

# Find threads from specific subreddits
all_threads <- data.frame()

for (sub in subreddits) {
  for (term in search_terms) {
    message("Searching '", term, "' in r/", sub)
    
    threads <- tryCatch({
      find_thread_urls(
        keywords = term,
        subreddit = sub,
        period = "year"
      )
    }, error = function(e) {
      message("  Error: ", e$message)
      NULL
    })
    
    if (!is.null(threads) && nrow(threads) > 0) {
      all_threads <- bind_rows(all_threads, threads)
      message("  Found: ", nrow(threads), " threads")
    }
    
    Sys.sleep(2)
  }
}

# Remove duplicates and sort by comment count
all_threads <- all_threads %>% 
  distinct(url, .keep_all = TRUE) %>%
  arrange(desc(comments))

message("\nFound ", nrow(all_threads), " unique threads")

# Save threads
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
save(all_threads, file = "data/raw/reddit_threads.RData")
write_csv(all_threads, "data/raw/reddit_threads.csv")

# Show top threads
message("\nTop 10 threads by comment count:")
print(all_threads %>% select(title, comments, subreddit) %>% head(10))

# Get comments from top 30 threads
top_threads <- head(all_threads, 30)

all_comments <- list()

for (i in 1:nrow(top_threads)) {
  message("\n[", i, "/", nrow(top_threads), "] r/", top_threads$subreddit[i])
  message("  Title: ", substr(top_threads$title[i], 1, 60))
  
  comments <- tryCatch({
    result <- get_thread_content(top_threads$url[i])
    if (!is.null(result$comments)) {
      result$comments$thread_title <- top_threads$title[i]
      result$comments$thread_text <- top_threads$text[i]
      result$comments$thread_subreddit <- top_threads$subreddit[i]
      result$comments %>% mutate(across(everything(), as.character))
    } else {
      NULL
    }
  }, error = function(e) {
    message("  Error: ", e$message)
    NULL
  })
  
  if (!is.null(comments)) {
    all_comments[[i]] <- comments
    message("  Success: ", nrow(comments), " comments")
  }
  
  Sys.sleep(5)
}

# Combine and save
reddit_comments <- bind_rows(all_comments)

save(reddit_comments, all_threads, file = "data/raw/reddit_raw.RData")
write_csv(reddit_comments, "data/raw/reddit_comments.csv")

# Summary by subreddit
subreddit_summary <- reddit_comments %>%
  count(thread_subreddit, sort = TRUE)

cat("\nTotal:", nrow(reddit_comments), "comments from", 
    n_distinct(reddit_comments$thread_title), "threads\n")
cat("\nComments by subreddit:\n")
print(subreddit_summary)