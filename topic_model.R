library(tidyverse)
library(readr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(ldatuning)

# Assuming previous steps are executed to get tokenized_data

# Convert tokenized data to a DFM for LDA
dtm <- tokenized_data %>%
  count(file_name, word) %>%
  cast_dtm(document = file_name, term = word, value = n)

# Use ldatuning to find the optimal number of topics
topic_range <- seq(2, 10, by = 1)
fitting_metrics <- FindTopicsNumber(
  dtm,
  topics = topic_range,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = 2, # Adjust based on your system's capabilities
  verbose = TRUE
)

# Plotting the metrics to help decide on the number of topics
FindTopicsNumber_plot(fitting_metrics)

# Assuming num_topics is chosen based on the plot (which looks like 5)
num_topics <- 5  # Placeholder for the chosen number of topics based on ldatuning results

# Run LDA fit using the data and the chosen number of topics
lda_fit <- LDA(dtm, k = num_topics, control = list(seed = 1234))

# Compute the probabilities of each topic for each document
topic_probabilities <- posterior(lda_fit)$topics

# Plotting the beta values for each topic
beta <- tidy(lda_fit, matrix = "beta")

library(ggplot2)
library(dplyr)

# Adjust the selection of top terms to ensure top 10 are selected within each topic
top_terms <- beta %>%
  group_by(topic) %>%
  top_n(50, beta) %>%
  ungroup() %>%
  arrange(topic, desc(beta))

# Plot with facets for each topic
ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = topic)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_y") +
  labs(x = "Term", y = "Beta") +
  theme_minimal() +
  theme(legend.position = "none") # Hide legend as 'topic' is used for faceting

#Use ChatGPT to figure out labels.....

# Assuming topic_probabilities is already defined as shown
# Create a vector of topic labels
topic_labels <- c("Time_Versioning", "AI_Development", "Machine_Learning_Process", "Tech_Discussions", "General_Technology")

# Convert topic_probabilities to a dataframe if it's not already
topic_probabilities_df <- as.data.frame(topic_probabilities)

# Add a column for the most likely topic for each document
topic_probabilities_df$most_likely_topic <- apply(topic_probabilities_df, 1, function(x) which.max(as.numeric(x)))

# Map the numeric topic to its label
topic_probabilities_df$topic_label <- sapply(topic_probabilities_df$most_likely_topic, function(x) topic_labels[x])

# Take a look at the updated dataframe
head(topic_probabilities_df)

# Define your topic labels
topic_labels <- c("Time_Versioning", "AI_Development", "Machine_Learning_Process", "Tech_Discussions", "General_Technology")

# Assuming 'topic_probabilities' is your dataframe
# Ensure it's in the dataframe format
topic_probabilities_df <- as.data.frame(topic_probabilities)

# Rename the columns based on the topic_labels
# First, construct new column names with labels
new_col_names <- sapply(colnames(topic_probabilities_df), function(x) {
  label_index <- as.numeric(gsub("X", "", x)) # Remove "X" if present and convert to numeric
  if(!is.na(label_index) && label_index <= length(topic_labels)) {
    return(topic_labels[label_index])
  } else {
    return(x) # Return original if not a topic column
  }
})

# Assign the new column names to the dataframe
colnames(topic_probabilities_df) <- new_col_names

# Display the updated dataframe to verify changes
head(topic_probabilities_df)

# Check if "Document" column exists and remove the ".txt" extension
topic_probabilities_df$Document <- rownames(topic_probabilities_df)

# Convert the 'Document' column to a factor to ensure the order in the plot
topic_probabilities_df$Document <- factor(topic_probabilities_df$Document, levels = unique(topic_probabilities_df$Document))



# Read the new document
new_article_path <- "data_camp_article.txt"
new_article_text <- read_file(new_article_path)

# Preprocess the text to match the preprocessing done on the original data
# (assuming tokenization and removal of stop words were done before creating the DTM)
new_article_tokens <- new_article_text %>%
  as_tibble() %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  mutate(document = "data_camp_article")

# Create a DTM for the new article using the same terms as the original DTM
new_dtm <- new_article_tokens %>%
  cast_dtm(document, word, n)

# Match the new DTM with the original DTM's columns
new_dtm <- DocumentTermMatrix(new_dtm, control = list(dictionary = Terms(dtm)))

# Compute the topic probabilities for the new article
new_article_probabilities <- posterior(lda_fit, new_dtm)$topics

# Add the new article's probabilities to the topic_probabilities_df
new_probs_df <- as.data.frame(new_article_probabilities)
colnames(new_probs_df) <- topic_labels

# Combine with the original topic probabilities dataframe
topic_probabilities_df <- bind_rows(topic_probabilities_df, new_probs_df)

# Display the updated dataframe to verify changes
head(topic_probabilities_df)

# Now, pivot longer, excluding the 'most_likely_topic' and 'topic_label' columns
topic_probabilities_long <- topic_probabilities_df %>%
  pivot_longer(cols = -Document, names_to = "Topic", values_to = "Probability")

# Sort the documents by name and then convert to a factor to ensure the order in the plot
topic_probabilities_long <- topic_probabilities_long %>%
  mutate(Document = fct_inorder(Document))

# Plotting each document's topic probability as separate horizontal bars
ggplot(topic_probabilities_long, aes(x = Document, y = Probability, fill = Topic)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(color = "black", size = 0.1), # Add horizontal lines with 1px width
        panel.grid.minor.y = element_blank(), # Remove minor horizontal grid lines
        panel.grid.major.x = element_blank(), # Remove vertical grid lines
        panel.background = element_blank()) + # Remove panel background
  labs(x = "Document", y = "Probability", fill = "Topic", title = "Topic Probabilities per Document")

