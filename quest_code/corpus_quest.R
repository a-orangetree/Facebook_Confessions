library(tm)
library(tidytext)
library(topicmodels)
library(imager)

fcb_data <- read_csv('data/FCB.csv') 


dfCorpus2 <- VCorpus(VectorSource(fcb_data$text)) 

############ Pre-process #######################

dfCorpus2 <- tm_map(dfCorpus2, stripWhitespace)
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


################# Create Document-Term-Matrix #####################


dtm <- DocumentTermMatrix(dfCorpus2)
dtm2 <- removeSparseTerms(dtm, 0.99)

rowTotals <- apply(dtm2, 1, sum)
dtm3 <- dtm2[rowTotals > 0,]
fcb_data2 <- fcb_data[rowTotals > 0,]


############## Sentiment Analysis (run sentiment_analysis.R first!) #################


afinn_dict <- get_sentiments("afinn") %>% rename('afinn' = 'score')

bing_dict <- get_sentiments("bing") %>% 
  rename('bing' = 'sentiment') %>% 
  mutate(bing = ifelse(bing == 'negative', 0, ifelse(bing == 'positive', 1, NA))
         ,bing = as.integer(bing))

nrc_dict <- get_sentiments("nrc") %>% rename('nrc' = 'sentiment')


tidy_dtm <- tidy(dtm3)
tidy_dtm <- left_join(tidy_dtm, bing_dict, by = c('term' = 'word'))
tidy_dtm <- left_join(tidy_dtm, afinn_dict, by = c('term' = 'word'))
tidy_dtm <- left_join(tidy_dtm, nrc_dict, by = c('term' = 'word'))

tidy_dtm_orig <- tidy_dtm

tidy_dtm <- tidy_dtm %>% 
  mutate(nrc = ifelse(is.na(nrc), 'neutral', nrc)
         ,afinn = ifelse(is.na(afinn), 0, afinn)
         ,bing = ifelse(is.na(bing), 99, bing))

fcb_data_for_dtm <- fcb_data %>% 
  mutate(id = as.character(id)) %>% 
  select(id, label, total_words, num_stopwords, u_word_count, i_word_count)

tidy_dtm <- left_join(tidy_dtm, fcb_data_for_dtm, by = c('document' = 'id'))

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
  select(document, label, total_words, num_stopwords, u_word_count, i_word_count) %>% 
  unique()

tidy_dtm_grouped <- left_join(tidy_dtm_grouped, tidy_dtm_nrc, by = c('document' = 'document'))
tidy_dtm_grouped <- left_join(tidy_dtm_grouped, tidy_dtm_afinn, by = c('document' = 'document'))
tidy_dtm_grouped <- left_join(tidy_dtm_grouped, tidy_dtm_bing, by = c('document' = 'document'))


############# K-Means Clustering #####################


data_scaled <- scale(select(drop_na(tidy_dtm_grouped), -document, -label)) %>% as.matrix()

k_max <- 100

kmeans_out <- sapply(1:k_max, 
                     function(k){kmeans(data_scaled, k, nstart = 100, iter.max = k_max)$tot.withinss})

plot(1:k_max, kmeans_out
     , type = "b"
     , pch = 19
     , frame = FALSE
     , xlab="Number of clusters K"
     , ylab="Total within-clusters sum of squares")


