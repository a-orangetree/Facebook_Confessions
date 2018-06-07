library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)
library(ggthemes)
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
         # ,country2 = factor(ifelse(country == 'united_states', 1, ifelse(country == 'canada', 2, 0)))
         ,year = factor(year)
         ,month = factor(month)
         ,day = factor(day)
         ,hour = factor(hour)
         ,state = factor(state)
         ,region = factor(region)) %>% 
  select(-post_id, -time)

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


########### EDA ############



# Observations per Country
tibble(country = c('United States', 'Canada', 'United Kingdom')
      ,num_observations = c(nrow(us_data), nrow(canada_data), nrow(uk_data))) %>%
  kable(align = c('c', 'c'), format = 'html') %>% 
  kable_styling(full_width = F, position = "left")


# Observation per Region/State
total_region_state_posts <- combined_data %>% 
  group_by(region, state) %>% 
  filter(!country %in% c('canada', 'united_kingdom')) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) 

total_region_state_posts %>% 
  ggplot(aes(x = reorder(region, count), y = count, fill = state)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_fivethirtyeight()
           
 
# Observations per University
total_university_posts <- combined_data %>% 
  group_by(university) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(count > 100)
  
total_university_posts %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(university, count), y = count)) +
  geom_bar(stat = "identity", fill = 'black') +
  coord_flip() +
  theme_fivethirtyeight()



# Displays range of number of posts for universities
for_quantiles <- combined_data %>% 
  group_by(university) %>% 
  summarise(count = n())

quantile(for_quantiles$count) %>% kable()



# Observations by time of day
combined_data %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(10) %>% 
  ggplot(aes(x = year, y = count)) +
  geom_bar(stat = "identity", fill = 'black') +
  theme_fivethirtyeight()



# I-Words versus You-Words
for_iu_words <- combined_data %>%
  filter(university %in% total_university_posts$university) %>% 
  group_by(university) %>% 
  summarise(i_words = sum(i_count)
            ,u_words = sum(u_count)
            ,i_to_u_ratio = round(i_words / u_words, 2)) 

for_iu_good <- for_iu_words %>% 
  filter(i_words > 0 & u_words > 0) %>% 
  arrange(i_to_u_ratio) %>% 
  select(university, i_to_u_ratio) %>% 
  head(3) %>% 
  gather()

for_iu_bad <- for_iu_words %>% 
  filter(i_words > 0 & u_words > 0) %>% 
  arrange(desc(i_to_u_ratio)) %>% 
  select(university, i_to_u_ratio) %>% 
  head(3) %>% 
  gather()

universities_iu_good <- filter(for_iu_good, key == 'university')$value
universities_iu_bad <- filter(for_iu_bad, key == 'university')$value

graph_summary <- for_iu_good %>%
  mutate(university = rep(universities_iu_good, nrow(.)/length(universities_iu_good))) %>%
  filter(key %in% c('i_to_u_ratio'))

graph_summary2 <- for_iu_bad %>%
  mutate(university = rep(universities_iu_bad, nrow(.)/length(universities_iu_bad))) %>%
  filter(key %in% c('i_to_u_ratio'))

ggplot(graph_summary, aes(x = key, y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  facet_wrap(~ university) +
  ggtitle("Number of 'I-words' vs Number of 'U-words'") +
  theme_fivethirtyeight()

ggplot(graph_summary2, aes(x = key, y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  facet_wrap(~ university) +
  ggtitle("Number of 'I-words' vs Number of 'U-words'") +
  theme_fivethirtyeight()


######### Average Number of Words
combined_data %>% 
  filter(university %in% total_university_posts$university) %>%
  group_by(university) %>% 
  summarise(avg_words = mean(num_words)) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(university, avg_words), y = avg_words)) +
  geom_bar(stat = "identity", fill = 'black') +
  coord_flip() +
  theme_fivethirtyeight()

######### Average Number of Characters per Word
combined_data %>% 
  filter(university %in% total_university_posts$university) %>%
  group_by(university) %>% 
  summarise(avg_chars = mean(characters_per_word)) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(university, avg_chars), y = avg_chars)) +
  geom_bar(stat = "identity", fill = 'black') +
  coord_flip() +
  theme_fivethirtyeight()
  


