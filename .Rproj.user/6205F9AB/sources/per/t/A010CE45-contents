library(tidyverse)
library(tokenizers)
library(tm)

fcb_data <- read_csv('data/FCB.csv') 

fcb_data <- fcb_data %>% 
  select(-X1)


table(fcb_data$label)


# all_tokens <- NULL
# for (line in seq(1:length(fcb_data$text))) {
#   
#   tokens <- tokenize_words(fcb_data$text)[[line]]
#   all_tokens <- c(all_tokens, tokens)
# }
write_csv(tibble(all_tokens), 'data/all_tokens.csv')


all_tokens <- read_csv('data/all_tokens.csv')
total_word_count <- length(all_tokens)
distinct_word_count <- n_distinct(all_tokens)


word_count <- tibble(words = names(table(all_tokens)), count = table(all_tokens))

word_count <- word_count %>% 
  mutate(perc_total = round(count / total_word_count, 4)
         ,perc_distinct = round(count / distinct_word_count, 4)) %>% 
  filter(!words %in% stopwords('english')) %>%
  arrange(desc(count))
