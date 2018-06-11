library(kableExtra)
library(knitr)
library(ggthemes)


########## Must run preprocessing.R first! ########################


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