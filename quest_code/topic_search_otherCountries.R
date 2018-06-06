library(tidyverse)
library(tokenizers)
library(tm)
library(tidytext)
library(topicmodels)
library(ldatuning)


########### Import Data ################


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

combined_data$university[combined_data$university == "Purdue University--West Lafayette:574281165929300posts"] <- "Purdue University"
combined_data$university[combined_data$university == "Rhodes College:418059621625397posts"] <- "Rhodes College"
combined_data$university[combined_data$university == "Rutgers, the State University of NJ:599429153419231posts"] <- "Rutgers University"


######################


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


topic_search <- FindTopicsNumber(dtm3,
                                 topics = seq(2, 100, 2),
                                 metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                                 method = "Gibbs",
                                 control = list(seed = 77),
                                 verbose = TRUE)

write_csv(topic_search, file = 'data/topic_search.csv')


##################################