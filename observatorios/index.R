# Load libraries
library(tidyverse)
library(tidytext)
library(wordcloud)
library(tm)
library(jsonlite)
library(widyr)
library(topicmodels)
library(igraph)
library(ggraph)
library(cluster)
library(factoextra)

# Set working directory
setwd("~/Downloads")

# Load JSON
data <- fromJSON("observatories.json")

# Inspect
glimpse(data)

# ----------------------------------------------
# I. ANALYSIS OF THE OBSERVATORIOS' NAMES
# ----------------------------------------------


# Split titles into words
title_df <- data %>%
  select(name) %>%
  unnest_tokens(word, name)

# Stopwords
data("stop_words")
spanish_stop <- tibble(word = stopwords("spanish"))
extra_stop <- tibble(word = c("NULL", "null", "observatorio","así","observatori", 
                              "http", "https", "da", "de", "la", "los",
                              "castilla", "mancha", "vasco", "galego", "español", "valenciana",
                              "madrid", "españa", "navarra", "comunidad", "galicia", 
                              "regional", "servicios", "municipal", "extremadura", "aragonés",
                              "andalucía", "comunitat", "canario", "estatal", "nacional", "asturias", 
                              "murcia", "león", "canario", "sociales", "región", 
                              "sistema", "pública", "local", "ciudad", "canarias", 
                              "andaluz", "información", "servicio", "urbano", "urbana", 
                              "provincia","baleares","balears",  "alicante","sevilla","ayuntamiento","cantabria", "territorial","barcelona","bizkaia", "provincia","cadena", "aragón", "alcalá"))

# Clean text
cleaned_titles <- title_df %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(spanish_stop, by = "word") %>%
  anti_join(extra_stop, by = "word") %>%
  filter(!str_detect(word, "^\\d+$"))  # remove numbers

# Optional: remove overly common words (appear in >80% of titles)

# Clean text 2
doc_freq <- cleaned_text %>%
  group_by(word) %>%
  summarize(doc_count = n_distinct(name)) %>%
  ungroup() %>%
  filter(doc_count / n_distinct(cleaned_text$name) < 0.8)  # keep words in <80% docs


cleaned_titles2 <- cleaned_titles %>% semi_join(doc_freq, by="word")

# -----------------------
# 1. Word frequency
# -----------------------
title_word_counts <- cleaned_titles2 %>%
  count(word, sort = TRUE)

# Top 20 words
title_word_counts %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Words in Titles", x = NULL, y = "Frequency")

# Wordcloud
set.seed(123)
wordcloud(words = title_word_counts$word,
          freq = title_word_counts$n,
          max.words = 50,
          random.order = FALSE,
          scale = c(4, 0.5),
          colors = brewer.pal(8, "Dark2"))


# -----------------------
# Bigrams from titles
# -----------------------

# Create bigrams from 'name' instead of 'description'
bigrams <- data %>%
  unnest_tokens(bigram, name, token = "ngrams", n = 2)

# Separate into two words
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Define stopwords and custom words to remove
data("stop_words")
spanish_stop <- tibble(word = stopwords("spanish"))
extra_stopwords <- tibble(word = c("NULL", "null", "observatorio","así","observatori", 
                                   "http", "https", "da", "de", "la", "los",
                                   "castilla", "mancha", "vasco", "galego", "español", "valenciana",
                                   "madrid", "españa", "navarra", "comunidad", "galicia", 
                                   "regional", "servicios", "municipal", "extremadura", "aragonés",
                                   "andalucía", "comunitat", "canario", "estatal", "nacional", "asturias", 
                                   "murcia", "león", "canario", "sociales", "región", 
                                   "sistema", "pública", "local", "ciudad", "canarias", 
                                   "andaluz", "información", "servicio", "urbano", "urbana", 
                                   "provincia", "isles", "balears","vaciamadrid", "barcelona","territorial", "provincia","cadena", "aragón", "alcalá"))

all_stopwords <- bind_rows(stop_words, spanish_stop, extra_stopwords)

# Remove bigrams where either word is a stopword or NA
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% all_stopwords$word,
         !word2 %in% all_stopwords$word,
         !is.na(word1),
         !is.na(word2))

# Recombine into bigrams
bigrams_clean <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# Count and sort
bigram_counts <- bigrams_clean %>%
  count(bigram, sort = TRUE)

# Show top 20 bigrams
head(bigram_counts, 20)

# Plot top 15 bigrams
bigram_counts %>%
  slice_max(n, n = 14) %>%
  ggplot(aes(reorder(bigram, n), n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 15 Bigrams in Titles (cleaned)",
       x = NULL, y = "Frequency")



# ----------------------------------------------
# II. ANALYSIS OF THE DESCRIPTIONS
# ----------------------------------------------

# Ensure 'description' is character
data <- data %>%
  mutate(description = as.character(description))

# Split into words
text_df <- data %>%
  select(name, description) %>%
  unnest_tokens(word, description)

# Stopwords
data("stop_words")
spanish_stop <- tibble(word = stopwords("spanish"))
extra_stop <- tibble(word = c("NULL", "null", "observatorio","así","observatori", "http", "https", "da", "de", "la", "los"))

# Clean text
cleaned_text <- text_df %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(spanish_stop, by = "word") %>%
  anti_join(extra_stop, by = "word") %>%
  filter(!str_detect(word, "^\\d+$"))  # remove numbers

# Clean text 2
doc_freq <- cleaned_text %>%
  group_by(word) %>%
  summarize(doc_count = n_distinct(name)) %>%
  ungroup() %>%
  filter(doc_count / n_distinct(cleaned_text$name) < 0.8)  # keep words in <80% docs

cleaned_text2 <- cleaned_text %>% semi_join(doc_freq, by="word")


# -----------------------
# 1. Word frequency
# -----------------------
word_counts <- cleaned_text2 %>%
  count(word, sort = TRUE)

# Top 20 words
word_counts %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Words", x = NULL, y = "Frequency")



set.seed(123)
wordcloud(words = word_counts$word,
          freq = word_counts$n,
          max.words = 50,
          random.order = FALSE,
          scale =c(4,0.5),
          colors = brewer.pal(8, "Dark2"))



# -----------------------
# 2. Bigrams
# -----------------------

library(tidyverse)
library(tidytext)

# Create bigrams
bigrams <- data %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2)

# Separate into two words
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Define stopwords and custom words to remove
data("stop_words")
spanish_stop <- tibble(word = stopwords("spanish"))
extra_stopwords <- tibble(word = c("NULL", "null", "observatorio", "http", "https", "da", "illes", "balears"))

all_stopwords <- bind_rows(stop_words, spanish_stop, extra_stopwords)

# Remove bigrams where either word is a stopword or NA
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% all_stopwords$word,
         !word2 %in% all_stopwords$word,
         !is.na(word1),
         !is.na(word2))

# Recombine into bigrams
bigrams_clean <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# Count and sort
bigram_counts <- bigrams_clean %>%
  count(bigram, sort = TRUE)

# Show top bigrams
head(bigram_counts, 20)

# Plot top 15 bigrams
bigram_counts %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(reorder(bigram, n), n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 15 Bigrams (cleaned)",
       x = NULL, y = "Frequency")



 

# -----------------------
# 4. Sentiment analysis
# -----------------------
bing_sentiment <- cleaned_text2 %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment, sort = TRUE)

# Plot sentiment
ggplot(bing_sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  labs(title = "Sentiment Analysis", x = NULL, y = "Count")





# -----------------------
# 5. Clustering observatorios
# -----------------------
# Create document-term matrix
dtm <- cleaned_text2 %>%
  count(name, word) %>%
  cast_dtm(name, word, n)

# Compute distance matrix
dist_matrix <- dist(as.matrix(dtm))

# Hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Hierarchical Clustering of Observatorios", xlab = "", sub = "")

# Optional: cut tree into clusters
clusters <- cutree(hc, k = 4)  # 4 clusters, adjust as needed
table(clusters)

# Add cluster info to original data
clustered_data <- data.frame(name = names(clusters), cluster = clusters)
