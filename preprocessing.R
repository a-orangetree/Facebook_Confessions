library(tidyverse)
library(lubridate)
library(tm)


########### Import Data ################
set.seed(123456)

us_data <- read_csv('data/us_new.csv') %>% 
  mutate(country = 'united_states') %>% 
  filter(!is.na(university)) %>% 
  rename('region' = 'Region')

uk_data <- read_csv('data/uk_original.csv') %>% 
  mutate(country = 'united_kingdom'
         ,state = 'united_kingdom'
         ,region = 'united_kingdom')

canada_data <- read_csv('data/canada_original.csv') %>% 
  mutate(country = 'canada'
         ,state = 'canada'
         ,region = 'canada')

i_words <- c("\\s*i\\s+", "i've", "i'm", "i'll", "i'd")
u_words <- c("you\\s", "you've", "you're", "you'll", "you'd", "\\su\\s", "u've", "u're", "u'll", "u'd")

i_words <- str_c(i_words, collapse = "|")
u_words <- str_c(u_words, collapse = "|")

combined_data <- bind_rows(us_data, uk_data, canada_data) %>% 
  rename('id' = 'X1') %>% 
  mutate(id = as.integer(seq(1, dim(.)[1], 1))
         ,year = year(time)
         ,month = month(time)
         ,day = day(time)
         ,hour = hour(time)
         ,message = str_to_lower(message)
         ,num_words = lengths(gregexpr("\\W+", message)) + 1
         ,characters_per_word = str_length(message) / num_words
         ,i_count = str_count(message, i_words)
         ,i_perc = str_count(message, i_words) / num_words 
         ,u_count = str_count(message, u_words)
         ,u_perc = str_count(message, u_words) / num_words
         ,iu_ratio = i_count / (u_count + 1)
         ,stopword_count = str_count(message, stopwords())
         ,stopword_perc = str_count(message, stopwords()) / num_words
         ,year = factor(year)
         ,month = factor(month)
         ,day = factor(day)
         ,hour = factor(hour)
         ,state = factor(state)
         ,region = factor(region)) %>% 
  select(-post_id, -time)

# Clean junk from raw data
combined_data$university[combined_data$university == "Purdue University--West Lafayette:574281165929300posts"] <- "Purdue University"
combined_data$university[combined_data$university == "Rhodes College:418059621625397posts"] <- "Rhodes College"
combined_data$university[combined_data$university == "Rutgers, the State University of NJ:599429153419231posts"] <- "Rutgers University"
combined_data$university[combined_data$university == "Syracuse University:206226116186911posts"] <- "Syracuse University"
combined_data$university[combined_data$university == "Rice University:266887743442106posts"] <- "Rice University"
combined_data$university[combined_data$university == "Rensselaer Polytechnic Institute:168590540005034posts"] <- "Rensselaer Polytechnic Institute"
combined_data$university[combined_data$university == "Pepperdine University:1426040870946133posts"] <- "Pepperdine University"
combined_data$university[combined_data$university == "Pepperdine University:480574488664604posts"] <- "Pepperdine University"
combined_data$university[combined_data$university == "Pennsylvania State University--University Park:479597885433626posts"] <- "Pennsylvania State University--University Park"
combined_data$university[combined_data$university == "Pepperdine University:480574488664604posts"] <- "Pepperdine University"
combined_data$university[combined_data$university == "Princeton:423593751057693posts"] <- "Princeton"
combined_data$university[combined_data$university == "Smith College:656392224417505posts"] <- "Smith College"
combined_data$university[combined_data$university == "Southwestern University:154639111371519posts"] <- "Southwestern University"
combined_data$university[combined_data$university == "St. Lawrence University:578380385537855posts"] <- "St. Lawrence University" 
combined_data$university[combined_data$state == "MA"] <- "Northeast" 

combined_data$university <- factor(combined_data$university)