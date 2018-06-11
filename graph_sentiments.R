library(gridExtra)

########### Must run build_sentiments.R first!! #############################

########### Must run EDA.R first!! #############################

graph_summary3 <- tidy_dtm_grouped %>% 
  filter(university %in% total_university_posts$university) %>% 
  # select(-num_likes, -num_comments, -country, -i_count, -u_count, -year, -month, -day, -iu_ratio, -characters_per_word
  #        -num_words, -avg_afinn, -avg_bing, -document, -hour, -i_perc, -u_perc, -stopword_count, -stopword_perc,
  #        -state, -region) %>% 
  select(university, anger, anticipation, disgust, fear, joy, negative, neutral, positive, sadness, surprise, trust) %>% 
  group_by(university) %>% 
  summarise(anger = log(mean(anger) + 1)
            ,anticipation = log(mean(anticipation) + 1)
            ,disgust = log(mean(disgust) + 1)
            ,fear = log(mean(fear) + 1)
            ,joy = log(mean(joy) + 1)
            ,negative = log(mean(negative) + 1)
            ,neutral = log(mean(neutral) + 1)
            ,positive = log(mean(positive) + 1)
            ,sadness = log(mean(sadness) + 1)
            ,surprise = log(mean(surprise) + 1)
            ,trust = log(mean(trust) + 1)) 

graph_summary3 <- graph_summary3 %>% gather()
universities <- filter(graph_summary3, key == 'university')$value

graph_summary4 <- graph_summary3 %>% 
  mutate(university = rep(universities, nrow(.)/length(universities))) %>% 
  filter(!key == 'university' | value == 0) %>% 
  group_by(key) %>% 
  mutate(rank = dense_rank(desc(value))) %>% 
  filter(!rank > 10)

# ggplot(graph_summary4, aes(x = university, y = value)) +
#   geom_bar(stat = 'identity', fill = 'black') +
#   coord_flip() +
#   facet_wrap(~ key) +
#   ggtitle("Category by Sentiment") +
#   theme_economist()
# 
# ggplot(graph_summary4, aes(x = key, y = value)) +
#   geom_bar(stat = 'identity', fill = 'black') +
#   coord_flip() +
#   facet_wrap(~ university) +
#   ggtitle("Category by Sentiment") +
#   theme_economist()


graph_data_anger <- filter(graph_summary4, key == 'anger') %>% mutate(value = round(as.double(value), 3))
graph_data_anticipation <- filter(graph_summary4, key == 'anticipation') %>% mutate(value = round(as.double(value), 3))
graph_data_disgust <- filter(graph_summary4, key == 'disgust') %>% mutate(value = round(as.double(value), 3))
graph_data_fear <- filter(graph_summary4, key == 'fear') %>% mutate(value = round(as.double(value), 3))
graph_data_joy <- filter(graph_summary4, key == 'joy') %>% mutate(value = round(as.double(value), 3))
graph_data_negative <- filter(graph_summary4, key == 'negative') %>% mutate(value = round(as.double(value), 3))
graph_data_neutral <- filter(graph_summary4, key == 'neutral') %>% mutate(value = round(as.double(value), 3))
graph_data_positive <- filter(graph_summary4, key == 'positive') %>% mutate(value = round(as.double(value), 3))
graph_data_sadness <- filter(graph_summary4, key == 'sadness') %>% mutate(value = round(as.double(value), 3))
graph_data_surprise <- filter(graph_summary4, key == 'surprise') %>% mutate(value = round(as.double(value), 3))
graph_data_trust <- filter(graph_summary4, key == 'trust') %>% mutate(value = round(as.double(value), 3))


########### By University ##################

plot_anger <- ggplot(graph_data_anger, aes(x = reorder(university, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'university', y = 'value') +
  theme_economist()

plot_anticipation <- ggplot(graph_data_anticipation, aes(x = reorder(university, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'university', y = 'value') +
  theme_economist()

plot_disgust <- ggplot(graph_data_disgust, aes(x = reorder(university, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'university', y = 'value') +
  theme_economist()

plot_fear <- ggplot(graph_data_fear, aes(x = reorder(university, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'university', y = 'value') +
  theme_economist()

plot_joy <- ggplot(graph_data_joy, aes(x = reorder(university, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'university', y = 'value') +
  theme_economist()

plot_negative <- ggplot(graph_data_negative, aes(x = reorder(university, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'university', y = 'value') +
  theme_economist()

plot_neutral <- ggplot(graph_data_neutral, aes(x = reorder(university, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'university', y = 'value') +
  theme_economist()

plot_positive <- ggplot(graph_data_positive, aes(x = reorder(university, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'university', y = 'value') +
  theme_economist()

plot_sadness <- ggplot(graph_data_sadness, aes(x = reorder(university, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'university', y = 'value') +
  theme_economist()

plot_surprise <- ggplot(graph_data_surprise, aes(x = reorder(university, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'university', y = 'value') +
  theme_economist()

plot_trust <- ggplot(graph_data_trust, aes(x = reorder(university, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'university', y = 'value') +
  theme_economist()

grid.arrange(plot_negative, plot_positive, nrow = 1, ncol = 2)
grid.arrange(plot_anger, plot_sadness, plot_fear, plot_disgust, nrow = 2, ncol = 2)
grid.arrange(plot_anticipation, plot_joy, plot_surprise, plot_trust, nrow = 2, ncol = 2)


############### By Country ####################

graph_summary3 <- tidy_dtm_grouped %>% 
  filter(university %in% total_university_posts$university) %>% 
  select(country, anger, anticipation, disgust, fear, joy, negative, neutral, positive, sadness, surprise, trust) %>% 
  group_by(country) %>% 
  summarise(anger = log(mean(anger) + 1)
            ,anticipation = log(mean(anticipation) + 1)
            ,disgust = log(mean(disgust) + 1)
            ,fear = log(mean(fear) + 1)
            ,joy = log(mean(joy) + 1)
            ,negative = log(mean(negative) + 1)
            ,neutral = log(mean(neutral) + 1)
            ,positive = log(mean(positive) + 1)
            ,sadness = log(mean(sadness) + 1)
            ,surprise = log(mean(surprise) + 1)
            ,trust = log(mean(trust) + 1)) 

graph_summary3 <- graph_summary3 %>% gather()
countries <- filter(graph_summary3, key == 'country')$value

graph_summary4 <- graph_summary3 %>% 
  mutate(country = rep(countries, nrow(.)/length(countries))) %>% 
  filter(!key == 'country' | value == 0) %>% 
  group_by(key) %>% 
  mutate(rank = dense_rank(desc(value)))

graph_data_anger <- filter(graph_summary4, key == 'anger') %>% mutate(value = round(as.double(value), 3))
graph_data_anticipation <- filter(graph_summary4, key == 'anticipation') %>% mutate(value = round(as.double(value), 3))
graph_data_disgust <- filter(graph_summary4, key == 'disgust') %>% mutate(value = round(as.double(value), 3))
graph_data_fear <- filter(graph_summary4, key == 'fear') %>% mutate(value = round(as.double(value), 3))
graph_data_joy <- filter(graph_summary4, key == 'joy') %>% mutate(value = round(as.double(value), 3))
graph_data_negative <- filter(graph_summary4, key == 'negative') %>% mutate(value = round(as.double(value), 3))
graph_data_neutral <- filter(graph_summary4, key == 'neutral') %>% mutate(value = round(as.double(value), 3))
graph_data_positive <- filter(graph_summary4, key == 'positive') %>% mutate(value = round(as.double(value), 3))
graph_data_sadness <- filter(graph_summary4, key == 'sadness') %>% mutate(value = round(as.double(value), 3))
graph_data_surprise <- filter(graph_summary4, key == 'surprise') %>% mutate(value = round(as.double(value), 3))
graph_data_trust <- filter(graph_summary4, key == 'trust') %>% mutate(value = round(as.double(value), 3))


plot_anger <- ggplot(graph_data_anger, aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'country', y = 'value') +
  theme_economist()

plot_anticipation <- ggplot(graph_data_anticipation, aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'country', y = 'value') +
  theme_economist()

plot_disgust <- ggplot(graph_data_disgust, aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'country', y = 'value') +
  theme_economist()

plot_fear <- ggplot(graph_data_fear, aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'country', y = 'value') +
  theme_economist()

plot_joy <- ggplot(graph_data_joy, aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'country', y = 'value') +
  theme_economist()

plot_negative <- ggplot(graph_data_negative, aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'country', y = 'value') +
  theme_economist()

plot_neutral <- ggplot(graph_data_neutral, aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'country', y = 'value') +
  theme_economist()

plot_positive <- ggplot(graph_data_positive, aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'country', y = 'value') +
  theme_economist()

plot_sadness <- ggplot(graph_data_sadness, aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'country', y = 'value') +
  theme_economist()

plot_surprise <- ggplot(graph_data_surprise, aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'country', y = 'value') +
  theme_economist()

plot_trust <- ggplot(graph_data_trust, aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  labs(x = 'country', y = 'value') +
  theme_economist()

grid.arrange(plot_negative, plot_positive, nrow = 1, ncol = 2)
grid.arrange(plot_anger, plot_sadness, plot_fear, plot_disgust, nrow = 2, ncol = 2)
grid.arrange(plot_anticipation, plot_joy, plot_surprise, plot_trust, nrow = 2, ncol = 2)