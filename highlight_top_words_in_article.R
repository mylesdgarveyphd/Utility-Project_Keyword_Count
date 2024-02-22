library(tidyverse)
library(tidytext)

# Assuming 'overall_word_count' is your dataframe with the top 100 keywords
# and it has the columns 'word' for the keyword and 'n' for the frequency count

# Read the article text file into a string
article_text <- read_file("data_camp_article.txt")

# Tokenize the words in the article
article_words <- article_text %>%
  as_tibble() %>%
  unnest_tokens(word, value)%>%
  count(word)

# Assuming that 'overall_word_count' is already available in your environment
# and that it contains a column named 'word' with the top 100 keywords

# Perform an inner join to find the common keywords
top100<-overall_word_count%>%slice_max(n=100,total_count)
common_keywords <- top100 %>%
  left_join((article_words), by = "word")%>%
  replace_na(list(n = 0))%>%
  mutate(weighted = total_count/sum(total_count))%>%
  mutate(weighted_dc = n/sum(n))

# View the resulting common keywords
common_keywords


#Color code them for Latex:

# R code to convert the "weighted_dc" values to a corresponding color hex code on a black-red color continuum.

colors <- grDevices::colorRampPalette(c("black", "red"))(21)

# Assuming 'data' is your data frame and it has a column 'weighted_dc' with values between 0 and 1
# Normalize 'weighted_dc' to a scale of 1-100
common_keywords$color_index <- round(common_keywords$weighted_dc * 100)+1

# Use the color index to assign the corresponding hex color code
common_keywords$color_hex <- colors[common_keywords$color_index]

write_csv(common_keywords,"colored_keywords.csv")
