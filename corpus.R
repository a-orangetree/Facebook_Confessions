library(tm)
library(tidytext)
library(topicmodels)
library(ggthemes)
library(knitr)
library(ldatuning)


dfCorpus2 <- VCorpus(VectorSource(fcb_data$text)) 
# inspect(dfCorpus2[1:10])
# inspect(dfCorpus2[[2]])


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
# inspect(dtm)

# Displays terms occuring at least x times
# findFreqTerms(dtm, 100)

# Displays words associated with particular terms
# findAssocs(dtm, "sex", 0.49)

# Removes sparse terms
dtm2 <- removeSparseTerms(dtm, 0.989)
inspect(dtm2)

# Removes documents with no words
rowTotals <- apply(dtm2, 1, sum)
dtm3 <- dtm2[rowTotals > 0,]
fcb_data2 <- fcb_data[rowTotals > 0,]

dim(dtm3)
dim(fcb_data2)

# Messing around
# test <- as.matrix(dtm2)
# test[1,1]

################## Create LDA ########################

# topic_search <- FindTopicsNumber(dtm3,
#                             topics = seq(5, 50, 5),
#                             metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#                             method = "Gibbs",
#                             control = list(seed = 77),
#                             verbose = TRUE)
topic_search <- read.csv('data/topic_search.csv')
FindTopicsNumber_plot(topic_search)

# burnin <- 4000
# iter <- 2000
# thin <- 500
# seed <- list(2003, 5, 63, 100001, 765)
# nstart <- 5
# k <- 100

# lda_out <- LDA(dtm, k, method = 'Gibbs'
#                , control = list(nstart = nstart, seed = seed, burnin = burnin, iter = iter, thin = thin))

lda_out <- LDA(dtm3, 35)

# save(lda_out, file = 'data/lda_100.object')
# lda_out <- load('data/lda_10.object')
# lda_out100 <- load('data/lda_100.object')

# Posteriors will help with predictions
# posterior(lda_out)
# posterior(lda_out, select(us_data, message))

# View each document's topic
topics <- topics(lda_out)


if (length(topics) == length(fcb_data2$text)) { 
  
  fcb_data2$topic = topics
  
  fcb_data2 <- fcb_data2 %>% group_by(label)
  
  labeled_breakdown <- table(filter(fcb_data2, label != 'None')$label, filter(fcb_data2, label != 'None')$topic)
  labeled_breakdown <- as_tibble(labeled_breakdown) %>% 
    mutate(rank = dense_rank(-n)) %>% 
    arrange(Var1, desc(n))
  
  max_ranks <- labeled_breakdown %>% group_by(Var1) %>% summarise(distinct_ranks = min(rank), number_of_lines = sum(n))
  max_ranks <- max_ranks %>% left_join(labeled_breakdown, by = c('Var1' = 'Var1', 'distinct_ranks' = 'rank')) 
  max_ranks <- max_ranks %>% mutate(perc_in_top_group = n / number_of_lines) %>% arrange(desc(perc_in_top_group))
  print(max_ranks)
  
  none_breakdown <- table(filter(fcb_data2, label == 'None')$label, filter(fcb_data2, label == 'None')$topic)
  none_breakdown <- as_tibble(none_breakdown) %>% arrange(Var1, desc(n))
  print(none_breakdown)
}


# filter(fcb_data2, label == 'Sex' & topic == 79)
write(fcb_data2, '~/Desktop/fcb_data2.csv')

# write_csv(arrange(fcb_data, topic), '~/Desktop/fcb_data_withTopics.csv')

# View the probabilities of each topic
as.tibble(lda_out@gamma)

# Create word probabilities for each document
lda_word_probs <- tidy(lda_out, matrix = 'beta')


# Displays word probabilities
lda_top_terms <- lda_word_probs %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

lda_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  filter(topic %in% c(79, 92, 35, 72)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() 

# Displays the probability of each topic for each document
lda_topic_probs <- tidy(lda_out, matrix = "gamma")