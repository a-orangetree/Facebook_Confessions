library(tm)
library(tidytext)
library(topicmodels)
library(ggthemes)


dfCorpus2 <- VCorpus(VectorSource(fcb_data$text)) # words
# inspect(dfCorpus2[1:10])
# inspect(dfCorpus2[[2]])


############ Pre-process #######################

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


################# Create Document-Term-Matrix #####################


dtm <- DocumentTermMatrix(dfCorpus2)
inspect(dtm)

# Displays most frequent terms
findFreqTerms(dtm, 500)

# Displays words associated with particular terms
findAssocs(dtm, "tamu", 0.3)

# Removes documents with no words
# rowTotals <- apply(dtm, 1, sum)
# dtm2 <- dtm[rowTotals > 0,]


################## Create LDA ########################


burnin <- 4000
iter <- 2000
thin <- 500
seed <- list(2003, 5, 63, 100001, 765)
nstart <- 5
k <- 100

# lda_out <- LDA(dtm, k, method = 'Gibbs'
#                , control = list(nstart = nstart, seed = seed, burnin = burnin, iter = iter, thin = thin))

lda_out <- LDA(dtm, 5)

posterior(lda_out)

# save(lda_out, file = 'data/lda_100.object')
lda_out <- load('data/lda_10.object')
lda_out100 <- load('data/lda_100.object')

# View each document's topic
topics <- topics(lda_out)

if (length(topics) == length(fcb_data$text)) { 
  
  fcb_data$topic = topics 
  
  fcb_data <- fcb_data %>% group_by(label)
  
  labeled_breakdown <- table(filter(fcb_data, label != 'None')$label, filter(fcb_data, label != 'None')$topic)
  labeled_breakdown <- as_tibble(labeled_breakdown) %>% 
    mutate(rank = dense_rank(-n)) %>% 
    arrange(Var1, desc(n))
  
  max_ranks <- labeled_breakdown %>% 
    group_by(Var1) %>% 
    summarise(distinct_ranks = min(rank)
              ,number_of_lines = sum(n))
  
  max_ranks <- max_ranks %>% 
    left_join(labeled_breakdown, by = c('Var1' = 'Var1', 'distinct_ranks' = 'rank')) 
  
  max_ranks <- max_ranks %>% mutate(perc_in_top_group = n / number_of_lines) %>% 
    arrange(desc(perc_in_top_group))
  
  none_breakdown <- table(filter(fcb_data, label == 'None')$label, filter(fcb_data, label == 'None')$topic)
  none_breakdown <- as_tibble(none_breakdown) %>% arrange(Var1, desc(n))
}


# write_csv(arrange(fcb_data, topic), '~/Desktop/fcb_data_withTopics.csv')

# View the probabilities of each topic
as.tibble(lda_out@gamma)

# Create word probabilities for each document
lda_word_probs <- tidy(lda_out, matrix = 'beta')

# Displays word probabilities
lda_top_terms <- lda_word_probs %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

lda_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() 

# Displays the probability of each topic for each document
lda_topic_probs <- tidy(lda_out, matrix = "gamma")