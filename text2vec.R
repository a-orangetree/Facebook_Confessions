library(text2vec)
library(data.table)
library(tokenizers)

set.seed(1234)

train_data <- sample_frac(fcb_data, .75)
test_data <- anti_join(fcb_data, train_data)

tokens <- word_tokenizer(train_data) %>% 
  tolower()

it_train <- itoken(train_data$text
                  ,progressbar = FALSE)

vocab <- create_vocabulary(it_train)

dtm_train <- create_dtm(it_train, vocab_vectorizer(vocab))
