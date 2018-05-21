######## Create Word Counts for EACH Label #################


word_count <- tibble(words = names(table(all_tokens)), count = table(all_tokens)) %>% 
  mutate(all_perc_total = round(count / length(.), 4)) %>% 
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


########### Combined Top and Bottom words #################


(top_500_words <- bind_cols(None[1:500,c(1,3)]
                            , Sex[1:500,c(1,3)]
                            , Mental_Health[1:500,c(1,3)]
                            , Money_Financial[1:500,c(1,3)]
                            , Medical[1:500,c(1,3)]
                            , Drugs[1:500,c(1,3)]
                            , Race_ProtectedGroups[1:500,c(1,3)]
                            , Excretions[1:500,c(1,3)]
                            , Academics[1:500,c(1,3)]
                            , Death[1:500,c(1,3)]))

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
