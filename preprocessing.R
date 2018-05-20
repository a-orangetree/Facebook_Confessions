library(tidyverse)
library(tokenizers)
library(tm)


######## Import data ################


fcb_data <- read_csv('data/FCB.csv') 

fcb_data <- fcb_data %>% 
  select(-X1) %>% 
  mutate(label = str_replace(label, 'Mental Health', 'Mental_Health')
         ,label = str_replace(label, 'Money/Financial', 'Money_Financial')
         ,label = str_replace(label, 'Race/Protected Groups', 'Race_ProtectedGroups'))

table(fcb_data$label)


####### Builds a Bag of Words ################


# all_tokens <- NULL
# for (line in seq(1:length(fcb_data$text))) {
# 
#   tokens <- tokenize_words(fcb_data$text)[[line]]
#   all_tokens <- c(all_tokens, tokens)
# }
# write_csv(tibble(all_tokens), 'data/all_tokens.csv')
all_tokens <- read_csv('data/all_tokens.csv')

########## Builds a Bag of Words for each Label ##############


None <- tibble()
Sex <- tibble()
Mental_Health <- tibble()
Money_Financial <- tibble()
Medical <- tibble()
Drugs <- tibble()
Race_ProtectedGroups <- tibble()
Excretions <- tibble()
Academics <- tibble()
Death <- tibble()


for (line in seq(1:length(fcb_data$text))) {

  tokens <- tibble(t = tokenize_words(fcb_data$text)[[line]]) %>%
    filter(!t %in% stopwords('english')) #%>%
    # mutate(t = stemDocument(t))

  label <- fcb_data$label[line]

  if (label == 'None') { None <- c(None, tokens) }
  else if (label == 'Sex') { Sex <- c(Sex, tokens) }
  else if (label == 'Mental_Health') { Mental_Health <- c(Mental_Health, tokens) }
  else if (label == 'Money_Financial') { Money_Financial <- c(Money_Financial, tokens) }
  else if (label == 'Medical') { Medical <- c(Medical, tokens) }
  else if (label == 'Drugs') { Drugs <- c(Drugs, tokens) }
  else if (label == 'Race_ProtectedGroups') { Race_ProtectedGroups <- c(Race_ProtectedGroups, tokens) }
  else if (label == 'Excretions') {Excretions <- c(Excretions, tokens) }
  else if (label == 'Academics') { Academics <- c(Academics, tokens) }
  else if (label == 'Death') { Death <- c(Death, tokens) }
 }


None <- tibble(unlist(None))
Sex <- tibble(unlist(Sex))
Mental_Health <- tibble(unlist(Mental_Health))
Medical <- tibble(unlist(Medical))
Money_Financial <- tibble(unlist(Money_Financial))
Drugs <- tibble(unlist(Drugs))
Race_ProtectedGroups <- tibble(unlist(Race_ProtectedGroups))
Excretions <- tibble(unlist(Excretions))
Academics <- tibble(unlist(Academics))
Death <- tibble(unlist(Death))


write_csv(None, 'data/None_words.csv')
write_csv(Sex, 'data/Sex_words.csv')
write_csv(Mental_Health, 'data/Mental_words.csv')
write_csv(Money_Financial, 'data/Money_words.csv')
write_csv(Medical, 'data/Medical_words.csv')
write_csv(Drugs, 'data/Drugs_words.csv')
write_csv(Race_ProtectedGroups, 'data/Race_words.csv')
write_csv(Excretions, 'data/Excretions_words.csv')
write_csv(Academics, 'data/Academics_words.csv')
write_csv(Death, 'data/Death_words.csv')


None <- read_csv('data/None_words.csv', col_names = F)
Sex <- read_csv('data/Sex_words.csv', col_names = F)
Mental_Health <- read_csv('data/Mental_words.csv', col_names = F)
Money_Financial <- read_csv('data/Money_words.csv', col_names = F)
Medical <- read_csv('data/Medical_words.csv', col_names = F)
Drugs <- read_csv('data/Drugs_words.csv', col_names = F)
Race_ProtectedGroups <- read_csv('data/Race_words.csv', col_names = F)
Excretions <- read_csv('data/Excretions_words.csv', col_names = F)
Academics <- read_csv('data/Academics_words.csv', col_names = F)
Death <- read_csv('data/Death_words.csv', col_names = F)


######## Create Word Counts for EACH Label #################


word_count <- tibble(words = names(table(all_tokens)), count = table(all_tokens)) %>% 
  mutate(perc_total = round(count / length(.), 4)
         ,perc_distinct = round(count / n_distinct(words), 4)) %>% 
  filter(!words %in% stopwords('english')) %>%
  arrange(desc(count))


None <- tibble(None_words = names(table(None)), count = table(None)) %>% 
  mutate(perc_total = round(count / dim(.)[1], 4)
         ,perc_distinct = round(count / n_distinct(None_words), 4)) %>% 
  arrange(desc(count))

Sex <- tibble(Sex_words = names(table(Sex)), count = table(Sex)) %>% 
  mutate(perc_total = round(count / dim(.)[1], 4)
         ,perc_distinct = round(count / n_distinct(Sex_words), 4)) %>% 
  arrange(desc(count))

Mental_Health <- tibble(Mental_words = names(table(Mental_Health)), count = table(Mental_Health)) %>% 
  mutate(perc_total = round(count / dim(.)[1], 4)
         ,perc_distinct = round(count / n_distinct(Mental_words), 4)) %>% 
  arrange(desc(count))

Money_Financial <- tibble(Money_words = names(table(Money_Financial)), count = table(Money_Financial)) %>% 
  mutate(perc_total = round(count / dim(.)[1], 4)
         ,perc_distinct = round(count / n_distinct(Money_words), 4)) %>% 
  arrange(desc(count))

Medical <- tibble(Medical_words = names(table(Medical)), count = table(Medical)) %>% 
  mutate(perc_total = round(count / dim(.)[1], 4)
         ,perc_distinct = round(count / n_distinct(Medical_words), 4)) %>% 
  arrange(desc(count))

Drugs <- tibble(Drugs_words = names(table(Drugs)), count = table(Drugs)) %>% 
  mutate(perc_total = round(count / dim(.)[1], 4)
         ,perc_distinct = round(count / n_distinct(Drugs_words), 4)) %>% 
  arrange(desc(count))

Race_ProtectedGroups <- tibble(Race_words = names(table(Race_ProtectedGroups)), count = table(Race_ProtectedGroups)) %>% 
  mutate(perc_total = round(count / dim(.)[1], 4)
         ,perc_distinct = round(count / n_distinct(Race_words), 4)) %>% 
  arrange(desc(count))

Excretions <- tibble(Excretions_words = names(table(Excretions)), count = table(Excretions)) %>% 
  mutate(perc_total = round(count / dim(.)[1], 4)
         ,perc_distinct = round(count / n_distinct(Excretions_words), 4)) %>% 
  arrange(desc(count))

Academics <- tibble(Academics_words = names(table(Academics)), count = table(Academics)) %>% 
  mutate(perc_total = round(count / dim(.)[1], 4)
         ,perc_distinct = round(count / n_distinct(Academics_words), 4)) %>% 
  arrange(desc(count))

Death <- tibble(Death_words = names(table(Death)), count = table(Death)) %>% 
  mutate(perc_total = round(count / dim(.)[1], 4)
         ,perc_distinct = round(count / n_distinct(Death_words), 4)) %>% 
  arrange(desc(count))


########### Combined Top and Bottom words #################


(top_500_words <- bind_cols(None[1:500,1]
                            , Sex[1:500,1]
                            , Mental_Health[1:500,1]
                            , Money_Financial[1:500,1]
                            , Medical[1:500,1]
                            , Drugs[1:500,1]
                            , Race_ProtectedGroups[1:500,1]
                            , Excretions[1:500,1]
                            , Academics[1:500,1]
                            , Death[1:500,1]))

# Medical does not have enough words to get to 1000
(bottom_500_words <- bind_cols(tail(None, 500)[1]
                              , tail(Sex, 500)[1]
                              , tail(Mental_Health, 500)[1]
                              , tail(Money_Financial, 500)[1]
                              , tail(Medical, 500)[1]
                              , tail(Drugs, 500)[1]
                              , tail(Race_ProtectedGroups, 500)[1]
                              , tail(Excretions, 500)[1]
                              , tail(Academics, 500)[1]
                              , tail(Death, 500)[1]))
