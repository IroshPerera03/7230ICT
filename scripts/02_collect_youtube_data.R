library(httr)
library(jsonlite)
library(tidyverse)

API_KEY <- "AIzaSyB4LAx5FMsgWepUjI7KlWb5AGSPqMsiD-I"

get_youtube_comments <- function(video_id, api_key, max_results = 100) {
  base_url <- "https://www.googleapis.com/youtube/v3/commentThreads"
  all_comments <- data.frame()
  next_page_token <- NULL
  
  repeat {
    # Build request
    response <- GET(
      url = base_url,
      query = list(
        part = "snippet",
        videoId = video_id,
        maxResults = min(max_results, 100),
        key = api_key,
        pageToken = next_page_token
      )
    )
    
    # Check for errors
    if (status_code(response) != 200) {
      warning("Failed to fetch comments for video: ", video_id)
      return(NULL)
    }
    
    # Parse response
    content <- content(response, as = "parsed")
    
    # Extract comments
    if (length(content$items) > 0) {
      comments <- map_dfr(content$items, function(item) {
        snippet <- item$snippet$topLevelComment$snippet
        data.frame(
          video_id = video_id,
          comment_id = item$snippet$topLevelComment$id,
          author = snippet$authorDisplayName,
          text = snippet$textDisplay,
          likes = snippet$likeCount,
          published_at = snippet$publishedAt,
          stringsAsFactors = FALSE
        )
      })
      
      all_comments <- bind_rows(all_comments, comments)
    }
    
    if (nrow(all_comments) >= max_results || is.null(content$nextPageToken)) {
      break
    }
    
    next_page_token <- content$nextPageToken
  }
  
  return(all_comments)
}

video_ids <- c("JGwWNGJdvx8","2Vv-BfVoq4g","lp-EO5I60KA","B4IY8Vqr9Gs","KxgqbOuXbs0")
all_comments <- list()

for (i in seq_along(video_ids)) {
  video_id <- video_ids[i]
  message("Fetching comments for video ", i, "/", length(video_ids), ": ", video_id)
  comments <- get_youtube_comments(video_id, API_KEY, max_results = 1000)
  if (!is.null(comments)) {
    all_comments[[i]] <- comments
    message("  Success: ", nrow(comments), " comments retrieved")
  } else {
    message("  Failed to retrieve comments")
  }
  
  Sys.sleep(20)
}

# Combine all dataframes
youtube_data <- bind_rows(all_comments)

# Save
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
save(youtube_data, video_ids, file = "data/raw/youtube_raw.RData")
write_csv(youtube_data, "data/raw/youtube_comments.csv")

cat("\nTotal comments collected:", nrow(youtube_data), "\n")