library(tidyverse)
library(readr)
library(tidytext)

# Step 1: Read all text files into a tibble
data_folder <- "data"
file_info <- tibble(
  filename = list.files(data_folder, pattern = "\\.txt$", full.names = TRUE)
) %>%
  mutate(
    document = map(filename, read_file),
    filename = basename(filename)
  ) %>%
  rename(
    file_name = filename,
    text = document
  )

# Function to generate n-grams
generate_ngrams <- function(data, n) {
  data %>%
    unnest_tokens(input = text, output = word, token = "ngrams", n = n) %>%
    mutate(word = tolower(word))%>%
    anti_join(stop_words)
}

# Step 2: Tokenize the documents into 1-gram, 2-gram, 3-gram, 4-gram, and 5-gram
tokenized_data <- map_dfr(.x = 1:5, ~generate_ngrams(file_info, .x))

# Proceed with the same steps for document-word count and overall word count tables

# (a) Document-Word Count Table
document_word_count <- tokenized_data %>%
  anti_join(stop_words, by = "word") %>% # Remove stop words
  count(file_name, word, name = "n") %>% # Count words by document
  arrange(file_name, -n)

# (b) Overall Word Count Table
overall_word_count <- document_word_count %>%
  group_by(word) %>%
  summarise(total_count = sum(n)) %>% # Sum word counts across all documents
  arrange(-total_count)

# Output the tables (optional, for verification)
print(document_word_count)
print(overall_word_count)

write_csv(document_word_count, "document_word_count.csv")
write_csv(overall_word_count, "overall_word_count.csv")
