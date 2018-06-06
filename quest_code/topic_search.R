library(tidyverse)
library(tokenizers)
library(tm)
library(tidytext)
library(topicmodels)
library(ldatuning)


######## Import data ################


fcb_data <- read_csv('data/FCB.csv') 

fcb_data <- fcb_data %>%
  filter(!text == 'NA' & !is.na(text)) %>% 
  select(-X1) %>% 
  mutate(label = str_replace(label, 'Mental Health', 'Mental_Health')
         ,label = str_replace(label, 'Money/Financial', 'Money_Financial')
         ,label = str_replace(label, 'Race/Protected Groups', 'Race_ProtectedGroups'))

(number_lines <- count(fcb_data, label) %>% 
    rename('num_lines' = 'n'))



##########################

dfCorpus2 <- VCorpus(VectorSource(fcb_data$text))

dfCorpus2 <- tm_map(dfCorpus2, stripWhitespace)
dfCorpus2 <- tm_map(dfCorpus2, removePunctuation)
dfCorpus2 <- tm_map(dfCorpus2, removeNumbers)
dfCorpus2 <- tm_map(dfCorpus2, content_transformer(tolower))
dfCorpus2 <- tm_map(dfCorpus2, removeWords, stopwords("english"))
dfCorpus2 <- tm_map(dfCorpus2, stemDocument)

myStopwords <- c('can', 'say', 'one', 'way', 'use', 'also', 'howev', 'tell', 'will', 'much', 'take'
                 ,'tend', 'even', 'like', 'particular', 'rather', 'said', 'get', 'well', 'make', 'ask', 'come'
                 ,'end', 'first', 'two', 'help', 'often', 'may', 'might', 'see', 'someth', 'thing', 'point'
                 ,'post', 'look', 'right', 'now', 'think', 'anoth', 'put', 'set', 'new', 'want', 'sure', 'kind'
                 ,'larg', 'yes', 'day', 'etc', 'quit', 'sinc', 'attempt', 'lack', 'seen', 'awar', 'littl', 'ever'
                 ,'moreov', 'though', 'found', 'abl', 'enough', 'far', 'earli', 'away', 'achiev', 'draw', 'last'
                 ,'brief', 'bit', 'entir', 'lot', 'wish', 'what', 'just', 'that', 'let', 'dont', 'cant', 'still')

dfCorpus2 <- tm_map(dfCorpus2, removeWords, myStopwords)

dtm <- DocumentTermMatrix(dfCorpus2)
dtm2 <- removeSparseTerms(dtm, 0.98)

# Removes documents with no words
rowTotals <- apply(dtm2, 1, sum)
dtm3 <- dtm2[rowTotals > 0,]

topic_search <- FindTopicsNumber(dtm3,
                                 topics = seq(5, 50, 5),
                                 metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                                 method = "Gibbs",
                                 control = list(seed = 77),
                                 verbose = TRUE)

write_csv(topic_search, file = 'data/topic_search.csv')