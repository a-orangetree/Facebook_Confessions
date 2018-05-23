library(tidyverse)
library(tokenizers)
library(tm)


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

# fcb_data %>% filter(str_detect(text, 'TAMU'))
####### Builds a Bag of Words ################


# all_tokens <- NULL
# for (line in seq(1:length(fcb_data$text))) {
# 
#   tokens <- tokenize_words(fcb_data$text)[[line]]
#   all_tokens <- c(all_tokens, tokens)
# }
# write_csv(tibble(all_tokens), 'data/all_tokens.csv')
all_tokens <- read_csv('data/all_tokens.csv')
non_stopwords <- filter(all_tokens, !all_tokens %in% stopwords('english'))

########## Builds a Bag of Words for each Label ##############


# None <- tibble()
# Sex <- tibble()
# Mental_Health <- tibble()
# Money_Financial <- tibble()
# Medical <- tibble()
# Drugs <- tibble()
# Race_ProtectedGroups <- tibble()
# # Excretions <- tibble()
# Academics <- tibble()
# Death <- tibble()
# 
# None_total_len <- 0
# Sex_total_len <- 0
# Mental_total_len <- 0
# Money_total_len <- 0
# Medical_total_len <- 0
# Drugs_total_len <- 0
# Race_total_len <- 0
# # Excretions_total_len <- 0
# Academics_total_len <- 0
# Death_total_len <- 0
# 
# ui_words <- c('you', "u", "ur", "your", "youre", "you're", "you'll", "you'd",'i', "i've", "i'd", "i'm", "i'll")
# custom_stopwords <- stopwords('english')[!stopwords('english') %in% ui_words]
# custom_stopwords <- c(custom_stopwords, 'like', 'just', 'can', 'will')
# 
# 
# for (line in seq(1:length(fcb_data$text))) {
# 
#   tokens <- tibble(t = tokenize_words(fcb_data$text)[[line]]) %>%
#     filter(!t %in% custom_stopwords) #%>%
#     # mutate(t = stemDocument(t))
# 
#   label <- fcb_data$label[line]
# 
#   if (label == 'None' | label == 'Excretions') {
#     None <- c(None, tokens)
#     None_total_len <- None_total_len + length(tokens$t)
#     }
#   else if (label == 'Sex') {
#     Sex <- c(Sex, tokens)
#     Sex_total_len <- Sex_total_len + length(tokens$t)
#     }
#   else if (label == 'Mental_Health') {
#     Mental_Health <- c(Mental_Health, tokens)
#     Mental_total_len <- Mental_total_len + length(tokens$t)
#     }
#   else if (label == 'Money_Financial') {
#     Money_Financial <- c(Money_Financial, tokens)
#     Money_total_len <- Money_total_len + length(tokens$t)
#     }
#   else if (label == 'Medical') {
#     Medical <- c(Medical, tokens)
#     Medical_total_len <- Medical_total_len + length(tokens$t)
#     }
#   else if (label == 'Drugs') {
#     Drugs <- c(Drugs, tokens)
#     Drugs_total_len <- Drugs_total_len + length(tokens$t)
#     }
#   else if (label == 'Race_ProtectedGroups') {
#     Race_ProtectedGroups <- c(Race_ProtectedGroups, tokens)
#     Race_total_len <- Race_total_len + length(tokens$t)
#     }
#   # else if (label == 'Excretions') {
#   #   Excretions <- c(Excretions, tokens)
#   #   Excretions_total_len <- Excretions_total_len + length(tokens$t)
#   #   }
#   else if (label == 'Academics') {
#     Academics <- c(Academics, tokens)
#     Academics_total_len <- Academics_total_len + length(tokens$t)
#     }
#   else if (label == 'Death') {
#     Death <- c(Death, tokens)
#     Death_total_len <- Death_total_len + length(tokens$t)
#     }
# }
# 
# 
# summary <- bind_cols(tibble(None = None_total_len)
#           ,tibble(Sex = Sex_total_len)
#           ,tibble(Mental_Health = Mental_total_len)
#           ,tibble(Money_Financial = Money_total_len)
#           ,tibble(Medical = Medical_total_len)
#           ,tibble(Drugs = Drugs_total_len)
#           ,tibble(Race_ProtectedGroups = Race_total_len)
#           # ,tibble(Excretions = Excretions_total_len)
#           ,tibble(Academics = Academics_total_len)
#           ,tibble(Death = Death_total_len)) %>%
#   gather(key = 'label', value = 'num_words') %>%
#   inner_join(number_lines, by = c('label'))
# 
# 
# summary <- summary %>%
#   mutate(avg_words = num_words / num_lines)
# 
# 
# write_csv(summary, 'data/label_summary.csv')
# 
# 
# None <- tibble(unlist(None))
# Sex <- tibble(unlist(Sex))
# Mental_Health <- tibble(unlist(Mental_Health))
# Medical <- tibble(unlist(Medical))
# Money_Financial <- tibble(unlist(Money_Financial))
# Drugs <- tibble(unlist(Drugs))
# Race_ProtectedGroups <- tibble(unlist(Race_ProtectedGroups))
# # Excretions <- tibble(unlist(Excretions))
# Academics <- tibble(unlist(Academics))
# Death <- tibble(unlist(Death))
# 
# 
# write_csv(None, 'data/None_words.csv')
# write_csv(Sex, 'data/Sex_words.csv')
# write_csv(Mental_Health, 'data/Mental_words.csv')
# write_csv(Money_Financial, 'data/Money_words.csv')
# write_csv(Medical, 'data/Medical_words.csv')
# write_csv(Drugs, 'data/Drugs_words.csv')
# write_csv(Race_ProtectedGroups, 'data/Race_words.csv')
# # write_csv(Excretions, 'data/Excretions_words.csv')
# write_csv(Academics, 'data/Academics_words.csv')
# write_csv(Death, 'data/Death_words.csv')


None <- read_csv('data/None_words.csv', col_names = F)
Sex <- read_csv('data/Sex_words.csv', col_names = F)
Mental_Health <- read_csv('data/Mental_words.csv', col_names = F)
Money_Financial <- read_csv('data/Money_words.csv', col_names = F)
Medical <- read_csv('data/Medical_words.csv', col_names = F)
Drugs <- read_csv('data/Drugs_words.csv', col_names = F)
Race_ProtectedGroups <- read_csv('data/Race_words.csv', col_names = F)
# Excretions <- read_csv('data/Excretions_words.csv', col_names = F)
Academics <- read_csv('data/Academics_words.csv', col_names = F)
Death <- read_csv('data/Death_words.csv', col_names = F)

summary <- read_csv('data/label_summary.csv')
