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
# tidy(dtm)

# Displays terms occuring at least x times
# findFreqTerms(dtm, 100)

# Displays words associated with particular terms
# findAssocs(dtm, "sex", 0.49)

# Removes sparse terms
dtm2 <- removeSparseTerms(dtm, 0.99)
# inspect(dtm2)

# Removes documents with no words
rowTotals <- apply(dtm2, 1, sum)
dtm3 <- dtm2[rowTotals > 0,]
fcb_data2 <- fcb_data[rowTotals > 0,]


############## Sentiment Analysis (run sentiment_analysis.R first!) #################


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


######## Hierarchical Clustering ############


data_scaled <- scale(select(tidy_dtm_grouped, -document, -label))

# Complete Linkage
hierarchical_cluster_complete <- hclust(dist(data_scaled), method = 'complete')
plot(hierarchical_cluster_complete, main = "Complete Linkage", xlab = "" , sub = "")

cutree(hierarchical_cluster_complete, 3)

# Average Linkage
hierarchical_cluster_avg <- hclust(dist(data_scaled), method = 'average')
plot(hierarchical_cluster_avg, main = "Average Linkage", xlab = "" , sub = "")

cutree(hierarchical_cluster_avg, 3)


############# K-Means Clustering #####################


data_scaled <- scale(select(drop_na(tidy_dtm_grouped), -document, -label)) %>% as.matrix()

# k_max <- nrow(drop_na(tidy_dtm_grouped)) - 1
k_max <- 40

kmeans_out <- sapply(1:k_max, 
                     function(k){kmeans(data_scaled, k, nstart = 20, iter.max = k_max)$tot.withinss})

plot(1:k_max, kmeans_out
     , type = "b"
     , pch = 19
     , frame = FALSE
     , xlab="Number of clusters K"
     , ylab="Total within-clusters sum of squares")


############## Plot Sentiment Analysis #################

tidy_dtm <- tidy_dtm_orig

tidy_dtm %>%
  group_by(label) %>%
  count(term, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(label = factor(label, levels = unique(label)),
         text_order = nrow(.):1) %>%
  ggplot(aes(reorder(term, text_order), n, fill = label)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ label, scales = "free_y") +
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")

# calculate percent of word use across all labels
total_pct <- tidy_dtm %>%
  count(term) %>%
  transmute(term, all_words = n / sum(n))

# calculate percent of word use within each label
frequency <- tidy_dtm %>%
  count(label, term) %>%
  mutate(label_words = n / sum(n)) %>%
  left_join(total_pct) %>%
  arrange(desc(label_words)) %>%
  ungroup()

# Words that are close to the line in these plots have similar frequencies across all the novels. 
# Words that are far from the line are words that are found more in one set of texts than another. 
# Furthermore, words standing out above the line are common across the series but not within that book,
# whereas words below the line are common in that particular book but not across the series.
ggplot(frequency, aes(x = label_words, y = all_words, color = abs(all_words - label_words))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = term), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~ label, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Taboo Labels", x = NULL)

# Correlation of words between labels
frequency %>%
  group_by(label) %>%
  summarize(correlation = cor(label_words, all_words),
            p_value = as.double(cor.test(label_words, all_words)$p.value)) %>% 
  kable()


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