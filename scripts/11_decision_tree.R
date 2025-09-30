# ============================================================================
# Q11: Decision Tree for Engagement Prediction
# ============================================================================

library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

load("data/raw/reddit_raw.RData")

cat("Preparing decision tree data...\n")

# Prepare features
reddit_ml <- reddit_comments %>%
  filter(!is.na(score)) %>%
  mutate(
    score_numeric = as.numeric(score),
    median_score = median(score_numeric, na.rm = TRUE),
    high_engagement = factor(ifelse(score_numeric > median_score, "High", "Low")),
    # Features
    comment_length = nchar(comment),
    word_count = lengths(strsplit(as.character(comment), "\\s+")),
    has_question = grepl("\\?", comment),
    has_exclamation = grepl("!", comment),
    char_per_word = comment_length / pmax(word_count, 1)
  ) %>%
  select(high_engagement, comment_length, word_count, 
         has_question, has_exclamation, char_per_word) %>%
  na.omit()

cat("Dataset:", nrow(reddit_ml), "comments\n")
cat("Median score:", unique(reddit_ml$median_score)[1], "\n")
cat("High engagement:", sum(reddit_ml$high_engagement == "High"), "\n")
cat("Low engagement:", sum(reddit_ml$high_engagement == "Low"), "\n")

# Train/test split
set.seed(42)
train_index <- createDataPartition(reddit_ml$high_engagement, p = 0.8, list = FALSE)
train_data <- reddit_ml[train_index, ]
test_data <- reddit_ml[-train_index, ]

# Build tree
tree_model <- rpart(
  high_engagement ~ .,
  data = train_data,
  method = "class",
  control = rpart.control(cp = 0.01, minsplit = 30, maxdepth = 5)
)

cat("\nDecision tree built\n")

# Visualize
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)

png("report/figures/q11_decision_tree.png", 
    width = 1200, height = 900, res = 150)
rpart.plot(tree_model, 
           main = "Decision Tree: Reddit Comment Engagement",
           extra = 104,
           box.palette = "RdYlGn",
           shadow.col = "gray",
           cex = 0.8)
dev.off()

# Predictions
predictions <- predict(tree_model, test_data, type = "class")

# Confusion matrix
conf_matrix <- confusionMatrix(predictions, test_data$high_engagement)

cat("\n=== MODEL PERFORMANCE ===\n")
print(conf_matrix)

performance <- data.frame(
  Metric = c("Accuracy", "Kappa", "Sensitivity", "Specificity"),
  Value = round(c(
    conf_matrix$overall['Accuracy'],
    conf_matrix$overall['Kappa'],
    conf_matrix$byClass['Sensitivity'],
    conf_matrix$byClass['Specificity']
  ), 3)
)

print(performance)

# Feature importance
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
         x = "Feature", y = "Importance")
  dev.off()
} else {
  importance_df <- NULL
  cat("No variable importance data available\n")
}

save(tree_model, predictions, conf_matrix, performance, importance_df,
     file = "data/processed/q11_decision_tree.RData")
