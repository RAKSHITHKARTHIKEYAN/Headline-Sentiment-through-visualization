install.packages(c("tidyverse", "tm", "textclean", "tidytext", "textdata", "sentimentr","patchwork"))

library(patchwork)
library(tidyverse)
library(tm)
library(textclean)
library(tidytext)
library(textdata)
library(sentimentr)

# load data
data <- read.csv("MN-DS-news-classification.csv")
head(data)

# Clean headlines and content (basic preprocessing)
data$headline_clean <- tolower(data$title) %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()

data$content_clean <- tolower(data$content) %>%
  removePunctuation() %>%
  removeNumbers() %>%
  stripWhitespace()


# Function to classify headlines as 'Question', 'Sensationalist', or 'Declarative'
classify_headline_extended <- function(headline) {
  headline <- tolower(headline)  # Convert to lowercase to standardize
  
  # Rule 1: Question-Based Headlines
  if (grepl("\\?", headline)) {
    return("Question")  # Headlines with a '?' are considered questions
  }
  
  # Rule 2: Sensationalist Headlines (with an extended list of emotional expressions and clickbait phrases)
  sensational_keywords <- c("shocking", "explosive", "unbelievable", "must-read", 
                            "crazy", "revealed", "surprising", "amazing", "you won't believe", 
                            "scandal", "rumor", "leaked", "jaw-dropping", "mind-blowing", 
                            "bizarre", "truth about", "the untold story", "the real reason", 
                            "you’ll never guess")
  
  # Check for any sensational keywords or clickbait patterns in the headline
  if (any(sapply(sensational_keywords, function(x) grepl(x, headline)))) {
    return("Sensationalist")  # Headlines with sensational keywords
  }
  
  # Rule 3: Declarative Headlines
  return("Declarative")  # Everything else is treated as declarative
}

head(data)

# Apply classification function to the dataset
data$headline_type <- sapply(data$title, classify_headline_extended)

# View the first few rows of the dataset with classified headlines
head(data)


# Analyze the distribution of headline types
headline_distribution <- table(data$headline_type)
print("Headline Type Distribution:")
print(headline_distribution)


# Load necessary libraries
library(caret)
library(e1071)

# Data inspection
print(dim(data))  # Check dimensions
print(colnames(data))  # Ensure necessary columns exist

str(data)
# Feature creation

# Install syuzhet for sentiment analysis
#install.packages("syuzhet")
library(syuzhet)

# Compute sentiment scores for the content column
data$sentiment_score <- get_sentiment(data$title, method = "afinn")

features <- data[, c("headline_type", "sentiment_score", "content_clean"), drop = FALSE]
features <- na.omit(features)  # Remove missing rows

# Verify features
print(dim(features))  # Should have non-zero rows and columns
print(head(features))  # Inspect first rows



# Map headline types to numeric values
data$headline_type_numeric <- as.numeric(factor(data$headline_type, 
                                                levels = c("Declarative", "Sensationalist", "Question")))


# Target variable
target <- data$headline_type_numeric

str(data)


# Calculate average sentiment score for each headline type
sentiment_vs_headline <- aggregate(sentiment_score ~ headline_type, data = data, FUN = mean)

# Scatter plot with regression line
library(ggplot2)

plot1 <- ggplot(data, aes(x = headline_type, y = sentiment_score, color = headline_type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  labs(title = "a) Sentiment Polarity by Headline Type",
       x = "Headline Type", y = "Sentiment Polarity") +
  theme_minimal()


# Convert date to year
data$year <- format(as.Date(data$date), "%Y")

plot2 <- ggplot(data, aes(x = sentiment_score, fill = year)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(
    title = "b) Distribution of Sentiment Scores Over the Years",
    x = "Sentimental Polarity",
    y = "Density",
    fill = "Year"
  )


# Extract sensationalist headlines and tokenize words
library(tidytext)
sensational_keywords <- c("shocking", "amazing", "unbelievable", "crazy", "revealed")
sensational_data <- data[grepl(paste(sensational_keywords, collapse = "|"), data$title), ]



library(tidyverse)
library(igraph)

# Sample data: List of words for simplicity
words <- c("crime", "law", "justice", "murder", "charged", "suspect", "trial", "arrested")


ggplot(data, aes(x = sentiment_score, fill = headline_type)) +
  geom_histogram(bins = 30, position = "dodge", alpha = 0.7) +
  labs(title = "Frequency of Sentiment Scores by Headline Type",
       x = "Sentiment Score", y = "Frequency") +
  theme_minimal()



# Categorize sentiment scores
data$sentiment_category <- cut(
  data$sentiment_score,
  breaks = c(-Inf, -2, 2, Inf),
  labels = c("Negative", "Neutral", "Positive")
)

# Create stacked bar chart

plot3 <- 
  
  ggplot(data, aes(x = headline_type, fill = sentiment_category)) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_fill_manual(values = c("Negative" = "red", "Neutral" = "gray", "Positive" = "blue")) +
  labs(title = "c) Distribution of Sentiment Categories by Headline Type",
       x = "Headline Type", y = "Proportion",
       fill = "Sentiment Category") +
  theme_minimal()




# Extract sensationalist headlines and tokenize words
library(tidytext)
sensational_keywords <- c("shocking", "amazing", "unbelievable", "crazy", "revealed")
sensational_data <- data[grepl(paste(sensational_keywords, collapse = "|"), data$title), ]

# Tokenize sensationalist headlines
tokens <- sensational_data %>%
  unnest_tokens(word, title) %>%
  filter(word %in% sensational_keywords)

# Create co-occurrence matrix
library(widyr)

# Extract month from the date
data$month <- format(as.Date(data$date), "%m")

# Filter sensationalist headlines
sensational_data <- data[data$headline_type == "Sensationalist", ]

# Count headlines by month
monthly_counts <- sensational_data %>%
  group_by(month) %>%
  summarise(count = n())


plot4 <- ggplot(monthly_counts, aes(x = month, y = count, fill = month)) +
  geom_bar(stat = "identity") +
  coord_polar() +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(
    title = "d) Seasonal Trends of the Sensationalist Headlines",
    x = "Month",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, vjust = 1, hjust = 1),  # Adjust axis labels
    legend.position = "right",  # Keep the vertical legend on the right
    legend.title = element_text(size = 8),  # Smaller legend title
    legend.text = element_text(size = 6),   # Smaller legend text
    legend.key.size = unit(0.4, "cm"),      # Compress legend keys (boxes)
    legend.spacing.y = unit(0.1, "cm"),     # Reduce spacing between legend items
    plot.title = element_text(hjust = 0.5, margin = margin(b = 10)),  # Adjust title spacing
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)  # Add small margin to the right
  )


combined_plot <- (plot1 | plot2) / (plot3 | plot4)

# Print the combined plot
print(combined_plot)



