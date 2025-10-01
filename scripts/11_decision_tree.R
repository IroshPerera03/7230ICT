# ============================================================================
# Q11: Decision Tree for Engagement Prediction
# ============================================================================

library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

load("data/raw/reddit_raw.RData")
load("data/processed/q10_sentiment.RData")  # Get sentiment data

# Merge sentiment with reddit data
reddit_ml <- reddit_data %>%
  mutate(
    score_numeric = as.numeric(score),
    median_score = median(score_numeric, na.rm = TRUE)
  ) %>%
  filter(!is.na(score_numeric), score_numeric != median_score) %>%  # Remove median values
  mutate(
    high_engagement = factor(ifelse(score_numeric > median_score, "High", "Low")),
    # More features
    comment_length = nchar(comment),
    word_count = lengths(strsplit(as.character(comment), "\\s+")),
    has_question = as.numeric(grepl("\\?", comment)),
    has_exclamation = as.numeric(grepl("!", comment)),
    char_per_word = comment_length / pmax(word_count, 1),
    # Add sentiment features
    sentiment_score = polarity,
    is_positive = as.numeric(polarity > 0),
    joy_level = joy,
    trust_level = trust
  ) %>%
  select(high_engagement, comment_length, word_count, has_question, 
         has_exclamation, char_per_word, sentiment_score, 
         is_positive, joy_level, trust_level) %>%
  na.omit()

cat("Dataset:", nrow(reddit_ml), "comments\n")
table(reddit_ml$high_engagement)

# Downsample majority class to balance
set.seed(42)
high_data <- reddit_ml %>% filter(high_engagement == "High")
low_data <- reddit_ml %>% filter(high_engagement == "Low") %>%
  sample_n(nrow(high_data))  # Match size of High class

reddit_ml_balanced <- bind_rows(high_data, low_data)
cat("Balanced dataset:", nrow(reddit_ml_balanced), "comments\n")
table(reddit_ml_balanced$high_engagement)

# Train/test split
train_index <- createDataPartition(reddit_ml_balanced$high_engagement, p = 0.8, list = FALSE)
train_data <- reddit_ml_balanced[train_index, ]
test_data <- reddit_ml_balanced[-train_index, ]

# Build tree with adjusted parameters
tree_model <- rpart(
  high_engagement ~ .,
  data = train_data,
  method = "class",
  control = rpart.control(cp = 0.005, minsplit = 20, maxdepth = 6)
)

# Predictions
predictions <- predict(tree_model, test_data, type = "class")
conf_matrix <- confusionMatrix(predictions, test_data$high_engagement)

print(conf_matrix)

# Feature importance (if available)
if (length(tree_model$variable.importance) > 0) {
  importance_df <- data.frame(
    Feature = names(tree_model$variable.importance),
    Importance = tree_model$variable.importance
  ) %>% arrange(desc(Importance))
  
  png("report/figures/q11_feature_importance.png", 
      width = 900, height = 600, res = 150)
  ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Feature Importance for Engagement Prediction",
         x = "Feature", y = "Importance Score")
  dev.off()
  
  cat("Feature importance plot saved\n")
  print(importance_df)
} else {
  cat("No variable importance available (tree too simple)\n")
  importance_df <- NULL
}

# Save everything
save(tree_model, predictions, conf_matrix, performance, importance_df,
     file = "data/processed/q11_decision_tree.RData")

cat("\nQ11 Complete.\n")