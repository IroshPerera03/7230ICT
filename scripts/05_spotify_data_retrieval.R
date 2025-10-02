# ============================================================================
# Q5: Spotify Data Retrieval (Improved)
# ============================================================================

library(spotifyr)
library(dplyr)
library(ggplot2)
library(purrr)

Sys.setenv(SPOTIFY_CLIENT_ID = 'a15b036b49ed4d6e976b1ad286f45181')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'ca02d6d8bbc14eac97a3614717922204')
access_token <- get_spotify_access_token()

# Artist search
artist_name <- "Ed Sheeran"
artist_search <- search_spotify(artist_name, type = "artist")
artist_id <- artist_search$id[1]

# Get artist info
artist_info <- get_artist(artist_id)

# Only include official studio albums
artist_albums <- get_artist_albums(artist_id, include_groups = "album") %>%
  filter(album_type == "album") %>%
  select(name, release_date, total_tracks) %>%
  distinct(name, .keep_all = TRUE) %>%
  arrange(release_date)

total_songs <- sum(artist_albums$total_tracks)

top_tracks <- get_artist_top_tracks(artist_id) %>%
  select(name, popularity)

avg_popularity <- mean(top_tracks$popularity)

genres <- paste(artist_info$genres, collapse = ", ")

spotify_summary <- list(
  artist_name = artist_info$name,
  followers = artist_info$followers$total,
  popularity = artist_info$popularity,
  genres = genres,
  total_albums = nrow(artist_albums),
  album_details = artist_albums,
  total_songs = total_songs,
  top_tracks_count = nrow(top_tracks),
  top_tracks = top_tracks,
  avg_top_track_popularity = avg_popularity
)

print(spotify_summary)

# Save
save(
  artist_info, artist_albums, top_tracks, spotify_summary,
  file = "data/processed/q5_spotify_data.RData"
)