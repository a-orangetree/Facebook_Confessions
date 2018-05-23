library(ggthemes)
# theme_set(theme_economist())
# theme_set(theme_fivethirtyeight())

############

graph_summary <- summary %>% gather()
labels <- filter(graph_summary, key == 'label')$value


##########


graph_summary1 <- graph_summary %>% 
  mutate(value = round(as.double(value), 4)
    ,label = rep(labels, nrow(.)/9)) %>% 
  filter(!key %in% c('label', 'num_words', 'num_lines', 'avg_words', 'i_words', 'u_words'
                     , 'i_to_u_ratio', 'mean_afinn', 'mean_bing'))

# Plot1
ggplot(graph_summary1, aes(x = label, y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  facet_wrap(~ key) +
  theme_economist()

# Plot2
ggplot(graph_summary1, aes(x = key, y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip() +
  facet_wrap(~ label) +
  theme_economist()


########### 


graph_summary2 <- graph_summary %>% 
  mutate(value = round(as.double(value), 4)
         ,label = rep(labels, nrow(.)/9)) %>% 
  filter(key %in% c('i_words', 'u_words'))

# Plot3
ggplot(graph_summary2, aes(x = key, y = value)) +
  geom_bar(stat = 'identity', aes(fill = key)) +
  coord_flip() +
  facet_wrap(~ label) +
  theme_economist_white()


############


graph_summary3 <- graph_summary %>% 
  mutate(value = round(as.double(value), 4)
         ,label = rep(labels, nrow(.)/9)) %>% 
  filter(key %in% c('i_to_u_ratio'))

# Plot4
ggplot(graph_summary2, aes(x = reorder(label, value), y = value)) +
  geom_bar(stat = 'identity', fill = 'black') +
  coord_flip()+
  theme_fivethirtyeight()