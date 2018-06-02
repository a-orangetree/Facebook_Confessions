library(tidyverse)
library(lubridate)



########### Import Data ################


us_data <- read_csv('data/us.csv') %>% 
  mutate(country = 'united_states')

uk_data <- read_csv('data/uk.csv') %>% 
  mutate(country = 'united_kingdom')

canada_data <- read_csv('data/canada.csv') %>% 
  mutate(country = 'canada')

i_words <- c("\\s*i\\s+", "i've", "i'm", "i'll", "i'd")
u_words <- c("you\\s", "you've", "you're", "you'll", "you'd", "\\su\\s", "u've", "u're", "u'll", "u'd")

i_words <- str_c(i_words, collapse = "|")
u_words <- str_c(u_words, collapse = "|")

combined_data <- bind_rows(us_data, uk_data, canada_data) %>% 
  rename('id' = 'X1') %>% 
  mutate(id = as.character(id)
         ,year = year(time)
         ,month = month(time)
         ,day = day(time)
         ,hour = hour(time)
         ,message = str_to_lower(message)
         ,i_count = str_count(message, i_words) 
         ,u_count = str_count(message, u_words)
         ,num_words = lengths(gregexpr("\\W+", message))
         ,num_characters = str_length(message)
         ,characters_per_word = num_characters / num_words) %>% 
  select(-post_id, -time)

            