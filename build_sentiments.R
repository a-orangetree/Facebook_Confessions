library(tokenizers)
library(tidytext)
library(knitr)


########## Must run preprocessing.R first! ########################


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


########## Create Final Sentiment Data Structure ##################


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
         , i_perc, u_perc, stopword_count, stopword_perc, iu_ratio, state, region, characters_per_word) %>% 
  unique()

tidy_dtm_grouped <- left_join(tidy_dtm_grouped, tidy_dtm_nrc, by = c('document' = 'document'))
tidy_dtm_grouped <- left_join(tidy_dtm_grouped, tidy_dtm_afinn, by = c('document' = 'document'))
tidy_dtm_grouped <- left_join(tidy_dtm_grouped, tidy_dtm_bing, by = c('document' = 'document'))