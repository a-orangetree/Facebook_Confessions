library(tokenizers)
library(tm)
library(tidytext)
library(topicmodels)
library(gridExtra)
library(cluster)
library(fpc)


############### Create Corpus ####################


dfCorpus <- VCorpus(VectorSource(combined_data$message))
# inspect(dfCorpus[[1]])

dfCorpus2 <- tm_map(dfCorpus, stripWhitespace)
dfCorpus2 <- tm_map(dfCorpus2, removePunctuation)
dfCorpus2 <- tm_map(dfCorpus2, removeNumbers)
dfCorpus2 <- tm_map(dfCorpus2, content_transformer(tolower))
dfCorpus2 <- tm_map(dfCorpus2, removeWords, stopwords("english"))
dfCorpus2 <- tm_map(dfCorpus2, stemDocument)

myStopwords <- c('can', 'say', 'one', 'way', 'use', 'also', 'howev', 'tell', 'will', 'much'
                 ,'tend', 'even', 'like', 'particular', 'rather', 'said', 'get', 'well', 'make', 'ask', 'come'
                 ,'end', 'first', 'two', 'often', 'may', 'might', 'see', 'someth', 'thing', 'point'
                 ,'post', 'think', 'anoth', 'put', 'set', 'want', 'sure', 'kind'
                 ,'yes', 'etc',  'sinc', 'attempt', 'lack', 'seen', 'awar', 'littl', 'ever'
                 , 'what', 'just', 'that', 'let', 'dont', 'cant', 'still'
                 , 'ill', 'ive', 'yall')

dfCorpus2 <- tm_map(dfCorpus2, removeWords, myStopwords)
# inspect(dfCorpus2[[1]])

dtm <- DocumentTermMatrix(dfCorpus2)
# inspect(dtm)

dtm2 <- removeSparseTerms(dtm, 0.99)
# inspect(dtm2)

rowTotals <- apply(dtm2, 1, sum)
dtm3 <- dtm2[rowTotals > 0,]
# combined_data2 <- combined_data[rowTotals > 0,]


########## Most common words #########################

tidy(dtm3) %>% 
  group_by(term) %>% 
  summarise(count = sum(count)) %>% 
  mutate(perc = round(count / dim(tidy(dtm3))[1], 4)) %>% 
  arrange(desc(count)) %>% 
  head(10) %>% 
  kable()

########### Import Sentiment Dictionaries ################


afinn_dict <- get_sentiments("afinn") %>% rename('afinn' = 'score')

bing_dict <- get_sentiments("bing") %>% 
  rename('bing' = 'sentiment') %>% 
  mutate(bing = ifelse(bing == 'negative', 0, ifelse(bing == 'positive', 1, NA))
         ,bing = as.integer(bing))

nrc_dict <- get_sentiments("nrc") %>% rename('nrc' = 'sentiment')


########## Sentiment Analysis ##################


tidy_dtm <- tidy(dtm3)
tidy_dtm <- left_join(tidy_dtm, bing_dict, by = c('term' = 'word'))
tidy_dtm <- left_join(tidy_dtm, afinn_dict, by = c('term' = 'word'))
tidy_dtm <- left_join(tidy_dtm, nrc_dict, by = c('term' = 'word'))

tidy_dtm <- tidy_dtm %>% 
  mutate(nrc = ifelse(is.na(nrc), 'neutral', nrc)
         ,afinn = ifelse(is.na(afinn), 0, afinn)
         ,bing = ifelse(is.na(bing), 99, bing)
         ,document = as.integer(document))

tidy_dtm <- left_join(tidy_dtm, select(combined_data, -message), by = c('document' = 'id'))

tidy_dtm_nrc <- tidy_dtm %>% 
  select(document, nrc) %>% 
  group_by(document, nrc) %>% 
  summarise('count' = n()) %>% 
  spread(nrc, count, fill = 0)

tidy_dtm_bing <- tidy_dtm %>% 
  filter(bing != 99) %>% 
  group_by(document) %>% 
  summarise(avg_bing = mean(bing))

tidy_dtm_afinn <- tidy_dtm %>% 
  filter(afinn != 0) %>% 
  group_by(document) %>% 
  summarise(avg_afinn = mean(afinn))


tidy_dtm_grouped <- tidy_dtm %>% 
  select(document, num_likes, num_comments, university, country, year, month, day, hour, num_words, i_count, u_count
         , i_perc, u_perc, stopword_count, stopword_perc, iu_ratio) %>% 
  unique()

tidy_dtm_grouped <- left_join(tidy_dtm_grouped, tidy_dtm_nrc, by = c('document' = 'document'))
tidy_dtm_grouped <- left_join(tidy_dtm_grouped, tidy_dtm_afinn, by = c('document' = 'document'))
tidy_dtm_grouped <- left_join(tidy_dtm_grouped, tidy_dtm_bing, by = c('document' = 'document'))


############## Graphing Sentiments #####################

graph_summary3 <- tidy_dtm_grouped %>% 
  filter(university %in% total_university_posts$university) %>% 
  select(-num_likes, -num_comments, -country, -i_count, -u_count, -year, -month, -day, -iu_ratio,
           -num_words, -avg_afinn, -avg_bing, -document, -hour, -i_perc, -u_perc, -stopword_count, -stopword_perc) %>% 
  group_by(university) %>% 
  summarise(anger = log(mean(anger) + 1)
            ,anticipation = log(mean(anticipation) + 1)
            ,disgust = log(mean(disgust) + 1)
            ,fear = log(mean(fear) + 1)
            ,joy = log(mean(joy) + 1)
            ,negative = log(mean(negative) + 1)
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
graph_data_positive <- filter(graph_summary4, key == 'positive') %>% mutate(value = round(as.double(value), 3))
graph_data_sadness <- filter(graph_summary4, key == 'sadness') %>% mutate(value = round(as.double(value), 3))
graph_data_surprise <- filter(graph_summary4, key == 'surprise') %>% mutate(value = round(as.double(value), 3))
graph_data_trust <- filter(graph_summary4, key == 'trust') %>% mutate(value = round(as.double(value), 3))

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

############ Clustering #####################

tidy_dtm_grouped_clust <- select(tidy_dtm_grouped, -country, -university, -document, -year, -month, -day, -hour)
scaled_data <- scale(model.matrix(~ ., data = drop_na(tidy_dtm_grouped_clust))[,-1])

k_max <- length(unique(drop_na(tidy_dtm_grouped)$university)) - 1

kmeans_out <- sapply(1:20,
                     function(k){kmeans(scaled_data, k, nstart = 50, iter.max = 100)$tot.withinss})

plot_kmeans <- plot(1:20, kmeans_out
     , type = "b"
     , pch = 19
     , frame = FALSE
     , xlab="Number of clusters K"
     , ylab="Total within-clusters sum of squares")

kmean_obj <- kmeans(scaled_data, 13, nstart = 50, iter.max = 100)

# tidy_dtm_grouped_clust2 <- drop_na(tidy_dtm_grouped_clust)
# tidy_dtm_grouped_clust2$cluster <- kmean_obj$cluster

plotcluster(drop_na(tidy_dtm_grouped_clust), kmean_obj$cluster)


########## PCA ########################


pca_results <- princomp(scaled_data)
summary(pca_results) 
# pca_results$scores

biplot(pca_results)


stop()
########### Topic Modeling ##################

