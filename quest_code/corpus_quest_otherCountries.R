library(tm)
library(tidytext)
library(topicmodels)
library(tidyverse)
library(lubridate)

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
  mutate(id = as.integer(seq(1, dim(.)[1], 1))
         ,year = year(time)
         ,month = month(time)
         ,day = day(time)
         ,hour = hour(time)
         ,message = str_to_lower(message)
         ,num_words = lengths(gregexpr("\\W+", message))
         ,characters_per_word = str_length(message) / num_words
         ,i_count = str_count(message, i_words) / num_words 
         ,u_count = str_count(message, u_words) / num_words)%>% 
  select(-post_id, -time)

##########################

# Observations per University
total_university_posts <- combined_data %>% 
  group_by(university) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(count > 100)

########################

dfCorpus <- VCorpus(VectorSource(combined_data$message))

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

dtm <- DocumentTermMatrix(dfCorpus2)
dtm2 <- removeSparseTerms(dtm, 0.99)

rowTotals <- apply(dtm2, 1, sum)
dtm3 <- dtm2[rowTotals > 0,]


########### Import Sentiment Dictionaries ################


afinn_dict <- get_sentiments("afinn") %>% rename('afinn' = 'score')

bing_dict <- get_sentiments("bing") %>% 
  rename('bing' = 'sentiment') %>% 
  mutate(bing = ifelse(bing == 'negative', 0, ifelse(bing == 'positive', 1, NA))
         ,bing = as.integer(bing))

nrc_dict <- get_sentiments("nrc") %>% rename('nrc' = 'sentiment')


#################################################

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
  select(document, num_likes, num_comments, university, country, year, month, day, hour, num_words, i_count, u_count) %>% 
  unique()

tidy_dtm_grouped <- left_join(tidy_dtm_grouped, tidy_dtm_nrc, by = c('document' = 'document'))
tidy_dtm_grouped <- left_join(tidy_dtm_grouped, tidy_dtm_afinn, by = c('document' = 'document'))
tidy_dtm_grouped <- left_join(tidy_dtm_grouped, tidy_dtm_bing, by = c('document' = 'document'))

graph_summary_kmeans <- tidy_dtm_grouped %>% 
  filter(university %in% total_university_posts$university) %>% 
  select(-document)


################# Clustering ################


data_scaled <- scale(model.matrix(~ ., drop_na(graph_summary_kmeans)))

kmeans_out <- sapply(1:45, 
                     function(k){kmeans(data_scaled, k, nstart = 100, iter.max = 100)$tot.withinss})

plot(1:k_max, kmeans_out
     , type = "b"
     , pch = 19
     , frame = FALSE
     , xlab="Number of clusters K"
     , ylab="Total within-clusters sum of squares")

write_csv(kmeans_out, 'data/kmeans_out.csv')

