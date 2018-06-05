library(tidyverse)
library(tokenizers)
library(tm)
library(knitr)
library(ggthemes)
library(tidytext)
library(topicmodels)
library(ldatuning)

# Import data
fcb_data <- read_csv('data/FCB.csv') 
all_tokens <- read_csv('data/all_tokens.csv')

fcb_data <- fcb_data %>%
  filter(!text == 'NA' & !is.na(text)) %>% 
  # select(-X1) %>% 
  mutate(label = str_replace(label, 'Mental Health', 'Mental_Health')
         ,label = str_replace(label, 'Money/Financial', 'Money_Financial')
         ,label = str_replace(label, 'Race/Protected Groups', 'Race_ProtectedGroups'))

number_lines <- count(fcb_data, label) %>% rename('num_lines' = 'n')

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

None_clean <- read_csv('data/None_words_clean.csv', col_names = F)
Sex_clean <- read_csv('data/Sex_words_clean.csv', col_names = F)
Mental_Health_clean <- read_csv('data/Mental_words_clean.csv', col_names = F)
Money_Financial_clean <- read_csv('data/Money_words_clean.csv', col_names = F)
Medical_clean <- read_csv('data/Medical_words_clean.csv', col_names = F)
Drugs_clean <- read_csv('data/Drugs_words_clean.csv', col_names = F)
Race_ProtectedGroups_clean <- read_csv('data/Race_words_clean.csv', col_names = F)
Excretions_clean <- read_csv('data/Excretions_words_clean.csv', col_names = F)
Academics_clean <- read_csv('data/Academics_words_clean.csv', col_names = F)
Death_clean <- read_csv('data/Death_words_clean.csv', col_names = F)

summary <- read_csv('data/label_summary.csv') %>% 
  mutate(avg_words = round(avg_words))

kable(summary, align=c(rep('c',times=4)))


######## Create Word Counts for EACH Label #################
print('b')
word_count <- tibble(words = names(table(all_tokens)), count = table(all_tokens)) %>% 
  mutate(all_perc_total = round(count / dim(.)[1], 4)) %>% 
  filter(!words %in% stopwords('english')) %>%
  arrange(desc(count))

None_clean <- tibble(None_words = names(table(None_clean)), count = table(None_clean)) %>% 
  mutate(None_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Sex_clean <- tibble(Sex_words = names(table(Sex_clean)), count = table(Sex_clean)) %>% 
  mutate(Sex_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Mental_Health_clean <- tibble(Mental_words = names(table(Mental_Health_clean)), count = table(Mental_Health_clean)) %>% 
  mutate(Mental_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Money_Financial_clean <- tibble(Money_words = names(table(Money_Financial_clean)), count = table(Money_Financial_clean)) %>% 
  mutate(Money_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Medical_clean <- tibble(Medical_words = names(table(Medical_clean)), count = table(Medical_clean)) %>% 
  mutate(Medical_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Drugs_clean <- tibble(Drugs_words = names(table(Drugs_clean)), count = table(Drugs_clean)) %>% 
  mutate(Drugs_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Race_ProtectedGroups_clean <- tibble(Race_words = names(table(Race_ProtectedGroups_clean)), count = table(Race_ProtectedGroups_clean)) %>% 
  mutate(Race_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Excretions <- tibble(Excretions_words = names(table(Excretions)), count = table(Excretions)) %>%
  mutate(Excretions_perc_total = round(count / dim(.)[1], 4)) %>%
  arrange(desc(count))

Academics_clean <- tibble(Academics_words = names(table(Academics_clean)), count = table(Academics_clean)) %>% 
  mutate(Academics_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Death_clean <- tibble(Death_words = names(table(Death_clean)), count = table(Death_clean)) %>% 
  mutate(Death_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))


print('a')
word_count <- tibble(words = names(table(all_tokens)), count = table(all_tokens)) %>% 
  mutate(all_perc_total = round(count / dim(.)[1], 4)) %>% 
  filter(!words %in% stopwords('english')) %>%
  arrange(desc(count))

None <- tibble(None_words = names(table(None)), count = table(None)) %>% 
  mutate(None_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Sex <- tibble(Sex_words = names(table(Sex)), count = table(Sex)) %>% 
  mutate(Sex_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Mental_Health <- tibble(Mental_words = names(table(Mental_Health)), count = table(Mental_Health)) %>% 
  mutate(Mental_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Money_Financial <- tibble(Money_words = names(table(Money_Financial)), count = table(Money_Financial)) %>% 
  mutate(Money_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Medical <- tibble(Medical_words = names(table(Medical)), count = table(Medical)) %>% 
  mutate(Medical_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Drugs <- tibble(Drugs_words = names(table(Drugs)), count = table(Drugs)) %>% 
  mutate(Drugs_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Race_ProtectedGroups <- tibble(Race_words = names(table(Race_ProtectedGroups)), count = table(Race_ProtectedGroups)) %>%
  mutate(Race_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Excretions <- tibble(Excretions_words = names(table(Excretions)), count = table(Excretions)) %>%
  mutate(Excretions_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Academics <- tibble(Academics_words = names(table(Academics)), count = table(Academics)) %>% 
  mutate(Academics_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))

Death <- tibble(Death_words = names(table(Death)), count = table(Death)) %>% 
  mutate(Death_perc_total = round(count / dim(.)[1], 4)) %>% 
  arrange(desc(count))



########### i words
i_words <- c('i', "i've", "i'd", "i'm", "i'll")

None_i <- sum(filter(None, None_words %in% i_words)$None_perc_total)
Sex_i <- sum(filter(Sex, Sex_words %in% i_words)$Sex_perc_total)
Mental_i <- sum(filter(Mental_Health, Mental_words %in% i_words)$Mental_perc_total)
Money_i <- sum(filter(Money_Financial, Money_words %in% i_words)$Money_perc_total)
Medical_i <- sum(filter(Medical, Medical_words %in% i_words)$Medical_perc_total)
Drugs_i <- sum(filter(Drugs, Drugs_words %in% i_words)$Drugs_perc_total)
Race_i <- sum(filter(Race_ProtectedGroups, Race_words %in% i_words)$Race_perc_total)
Excretions_i <- sum(filter(Excretions, Excretions_words %in% i_words)$Excretions_perc_total)
Academics_i <- sum(filter(Academics, Academics_words %in% i_words)$Academics_perc_total)
Death_i <- sum(filter(Death, Death_words %in% i_words)$Death_perc_total)

summary$i_words <- c(None_i, Sex_i, Mental_i, Money_i, Medical_i, Drugs_i, Race_i, Excretions_i, Academics_i, Death_i)


########## You Words ########################
u_words <- c('you', "u", "ur", "your", "youre", "you're", "you'll", "you'd")

# unique(all_tokens) %>% arrange(all_tokens) %>% View()
None_u <- sum(filter(None, None_words %in% u_words)$None_perc_total)
Sex_u <- sum(filter(Sex, Sex_words %in% u_words)$Sex_perc_total)
Mental_u <- sum(filter(Mental_Health, Mental_words %in% u_words)$Mental_perc_total)
Money_u <- sum(filter(Money_Financial, Money_words %in% u_words)$Money_perc_total)
Medical_u <- sum(filter(Medical, Medical_words %in% u_words)$Medical_perc_total)
Drugs_u <- sum(filter(Drugs, Drugs_words %in% u_words)$Drugs_perc_total)
Race_u <- sum(filter(Race_ProtectedGroups, Race_words %in% u_words)$Race_perc_total)
Excretions_u <- sum(filter(Excretions, Excretions_words %in% u_words)$Excretions_perc_total)
Academics_u <- sum(filter(Academics, Academics_words %in% u_words)$Academics_perc_total)
Death_u <- sum(filter(Death, Death_words %in% u_words)$Death_perc_total)

summary$u_words <- c(None_u, Sex_u, Mental_u, Money_u, Medical_u, Drugs_u, Race_u, Academics_u, Death_u)

summary <- summary %>% 
  mutate(i_to_u_ratio = i_words / u_words)


####### Top Words ################
top_words <- 5
top_500_words <- bind_cols(None_clean[1:top_words,1]
                           , Sex_clean[1:top_words,1]
                           , Mental_Health_clean[1:top_words,1]
                           , Money_Financial_clean[1:top_words,1]
                           , Medical_clean[1:top_words,1]
                           , Drugs_clean[1:top_words,1]
                           , Race_ProtectedGroups_clean[1:top_words,1]
                           , Excretions[1:top_words,1]
                           , Academics_clean[1:top_words,1]
                           , Death_clean[1:top_words,1])
