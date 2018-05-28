library(tidytext)


############## Sentiment Analysis ######################

#### Run Word Counts First !!!!!


afinn_dict <- get_sentiments("afinn") %>% rename('afinn' = 'score')

bing_dict <- get_sentiments("bing") %>% 
  rename('bing' = 'sentiment') %>% 
  mutate(bing = ifelse(bing == 'negative', 0, ifelse(bing == 'positive', 1, NA))
         ,bing = as.integer(bing))
  
nrc_dict <- get_sentiments("nrc") %>% rename('nrc' = 'sentiment')

None_clean <- None_clean %>% 
  left_join(afinn_dict, by = c('None_words' = 'word')) %>% 
  left_join(bing_dict, by = c('None_words' = 'word')) %>% 
  left_join(nrc_dict, by = c('None_words' = 'word'))

Sex_clean <- Sex_clean %>% 
  left_join(afinn_dict, by = c('Sex_words' = 'word')) %>% 
  left_join(bing_dict, by = c('Sex_words' = 'word')) %>% 
  left_join(nrc_dict, by = c('Sex_words' = 'word'))

Mental_Health_clean <- Mental_Health_clean %>% 
  left_join(afinn_dict, by = c('Mental_words' = 'word')) %>% 
  left_join(bing_dict, by = c('Mental_words' = 'word')) %>% 
  left_join(nrc_dict, by = c('Mental_words' = 'word'))

Money_Financial_clean <- Money_Financial_clean %>% 
  left_join(afinn_dict, by = c('Money_words' = 'word')) %>% 
  left_join(bing_dict, by = c('Money_words' = 'word')) %>% 
  left_join(nrc_dict, by = c('Money_words' = 'word'))

Medical_clean <- Medical_clean %>% 
  left_join(afinn_dict, by = c('Medical_words' = 'word')) %>% 
  left_join(bing_dict, by = c('Medical_words' = 'word')) %>% 
  left_join(nrc_dict, by = c('Medical_words' = 'word'))

Drugs_clean <- Drugs_clean %>% 
  left_join(afinn_dict, by = c('Drugs_words' = 'word')) %>% 
  left_join(bing_dict, by = c('Drugs_words' = 'word')) %>% 
  left_join(nrc_dict, by = c('Drugs_words' = 'word'))

Race_ProtectedGroups_clean <- Race_ProtectedGroups_clean %>% 
  left_join(afinn_dict, by = c('Race_words' = 'word')) %>% 
  left_join(bing_dict, by = c('Race_words' = 'word')) %>% 
  left_join(nrc_dict, by = c('Race_words' = 'word'))

Excretions_clean <- Excretions_clean %>%
  left_join(afinn_dict, by = c('Excretions_words' = 'word')) %>%
  left_join(bing_dict, by = c('Excretions_words' = 'word')) %>%
  left_join(nrc_dict, by = c('Excretions_words' = 'word'))

Academics_clean <- Academics_clean %>% 
  left_join(afinn_dict, by = c('Academics_words' = 'word')) %>% 
  left_join(bing_dict, by = c('Academics_words' = 'word')) %>% 
  left_join(nrc_dict, by = c('Academics_words' = 'word'))

Death_clean <- Death_clean %>% 
  left_join(afinn_dict, by = c('Death_words' = 'word')) %>% 
  left_join(bing_dict, by = c('Death_words' = 'word')) %>% 
  left_join(nrc_dict, by = c('Death_words' = 'word'))

word_count <- word_count %>% 
  left_join(afinn_dict, by = c('words' = 'word')) %>% 
  left_join(bing_dict, by = c('words' = 'word')) %>% 
  left_join(nrc_dict, by = c('words' = 'word'))


########### AFINN
mean_afinn <- bind_cols(select(filter(None_clean, !is.na(afinn)), None_words, afinn) %>% 
                          unique() %>% 
                          summarise(None = mean(afinn))
          ,select(filter(Sex_clean, !is.na(afinn)), Sex_words, afinn) %>% 
            unique() %>% 
            summarise(Sex = mean(afinn))
          ,select(filter(Mental_Health_clean, !is.na(afinn)), Mental_words, afinn) %>% 
            unique() %>% 
            summarise(Mental_Health = mean(afinn))
          ,select(filter(Money_Financial_clean, !is.na(afinn)), Money_words, afinn) %>% 
            unique() %>% 
            summarise(Money_Financial = mean(afinn))
          ,select(filter(Medical_clean, !is.na(afinn)), Medical_words, afinn) %>% 
            unique() %>% 
            summarise(Medical = mean(afinn))
          ,select(filter(Drugs_clean, !is.na(afinn)), Drugs_words, afinn) %>% 
            unique() %>% 
            summarise(Drugs = mean(afinn))
          ,select(filter(Race_ProtectedGroups_clean, !is.na(afinn)), Race_words, afinn) %>% 
            unique() %>% 
            summarise(Race_ProtectedGroups = mean(afinn))
          ,select(filter(Excretions_clean, !is.na(afinn)), Excretions_words, afinn) %>%
            unique() %>%
            summarise(Excretions = mean(afinn))
          ,select(filter(Academics_clean, !is.na(afinn)), Academics_words, afinn) %>% 
            unique() %>% 
            summarise(Academics = mean(afinn))
          ,select(filter(Death_clean, !is.na(afinn)), Death_words, afinn) %>% 
            unique() %>% 
            summarise(Death = mean(afinn))) %>% 
  gather(key = 'label', value = 'mean_afinn')


summary <- summary %>% 
  inner_join(mean_afinn, by = c('label'))



######## BING
mean_bing <- bind_cols(select(filter(None_clean, !is.na(bing)), None_words, bing) %>% 
                          unique() %>% 
                          summarise(None = mean(bing))
                        ,select(filter(Sex_clean, !is.na(bing)), Sex_words, bing) %>% 
                          unique() %>% 
                          summarise(Sex = mean(bing))
                        ,select(filter(Mental_Health_clean, !is.na(bing)), Mental_words, bing) %>% 
                          unique() %>% 
                          summarise(Mental_Health = mean(bing))
                        ,select(filter(Money_Financial_clean, !is.na(bing)), Money_words, bing) %>% 
                          unique() %>% 
                          summarise(Money_Financial = mean(bing))
                        ,select(filter(Medical_clean, !is.na(bing)), Medical_words, bing) %>% 
                          unique() %>% 
                          summarise(Medical = mean(bing))
                        ,select(filter(Drugs_clean, !is.na(bing)), Drugs_words, bing) %>% 
                          unique() %>% 
                          summarise(Drugs = mean(bing))
                        ,select(filter(Race_ProtectedGroups_clean, !is.na(bing)), Race_words, bing) %>% 
                          unique() %>% 
                          summarise(Race_ProtectedGroups = mean(bing))
                        ,select(filter(Excretions_clean, !is.na(bing)), Excretions_words, bing) %>%
                          unique() %>%
                          summarise(Excretions = mean(bing))
                        ,select(filter(Academics_clean, !is.na(bing)), Academics_words, bing) %>% 
                          unique() %>% 
                          summarise(Academics = mean(bing))
                        ,select(filter(Death_clean, !is.na(bing)), Death_words, bing) %>% 
                          unique() %>% 
                          summarise(Death = mean(bing))) %>% 
  gather(key = 'label', value = 'mean_bing')

summary <- summary %>% 
  inner_join(mean_bing, by = c('label'))



################# NRC #############################
None_nrc <- filter(None_clean, !is.na(nrc)) %>% count(nrc, sort = T) %>% rename('None' = 'n')
Sex_nrc <- filter(Sex_clean, !is.na(nrc)) %>% count(nrc, sort = T) %>% rename('Sex' = 'n')
Mental_nrc <- filter(Mental_Health_clean, !is.na(nrc)) %>% count(nrc, sort = T) %>% rename('Mental_Health' = 'n')
Money_nrc <- filter(Money_Financial_clean, !is.na(nrc)) %>% count(nrc, sort = T) %>% rename('Money_Financial' = 'n')
Medical_nrc <- filter(Medical_clean, !is.na(nrc)) %>% count(nrc, sort = T) %>% rename('Medical' = 'n')
Drugs_nrc <- filter(Drugs_clean, !is.na(nrc)) %>% count(nrc, sort = T) %>% rename('Drugs' = 'n')
Race_nrc <- filter(Race_ProtectedGroups_clean, !is.na(nrc)) %>% count(nrc, sort = T) %>% rename('Race_ProtectedGroups' = 'n')
Excretions_nrc <- filter(Excretions_clean, !is.na(nrc)) %>% count(nrc, sort = T) %>% rename('Excretions' = 'n')
Academics_nrc <- filter(Academics_clean, !is.na(nrc)) %>% count(nrc, sort = T) %>% rename('Academics' = 'n')
Death_nrc <- filter(Death_clean, !is.na(nrc)) %>% count(nrc, sort = T) %>% rename('Death' = 'n')


nrc_summary <- inner_join(None_nrc, Sex_nrc, by = c('nrc')) %>% 
                  left_join(Mental_nrc, by = c('nrc')) %>% 
                  left_join(Money_nrc, by = c('nrc')) %>% 
                  left_join(Medical_nrc, by = c('nrc')) %>% 
                  left_join(Drugs_nrc, by = c('nrc')) %>% 
                  left_join(Race_nrc, by = c('nrc')) %>% 
                  left_join(Excretions_nrc, by = c('nrc')) %>%
                  left_join(Academics_nrc, by = c('nrc')) %>% 
                  left_join(Death_nrc, by = c('nrc')) 


nrc_col_sums <- as_tibble(cbind('label' = names(nrc_summary)[-1]
          , 'total_nrc' = colSums(nrc_summary[,-1]))) %>% 
  mutate(total_nrc = as.integer(total_nrc))


nrc_summary <- as_tibble(cbind(label = names(nrc_summary), t(nrc_summary))) %>% 
  rename(., 'negative' = 'V1'
         ,'positive' = 'V2'
         ,'trust' = 'V3'
         ,'fear' = 'V4'
         ,'sadness' = 'V5'
         ,'anger' = 'V6'
         ,'anticipation' = 'V7'
         ,'joy' = 'V8'
         ,'disgust' = 'V9'
         ,'surprise' = 'V10') %>% 
  filter(label != 'nrc') %>% 
  mutate(negative = as.integer(negative)
         ,positive = as.integer(positive)
         ,trust = as.integer(trust)
         ,fear = as.integer(fear)
         ,sadness = as.integer(sadness)
         ,anger = as.integer(anger)
         ,anticipation = as.integer(anticipation)
         ,joy = as.integer(joy)
         ,disgust = as.integer(disgust)
         ,surprise = as.integer(surprise))


nrc_summary_perc <- as_tibble(sweep(nrc_summary[,-1] %>% as.matrix(), 1, nrc_col_sums[,-1] %>% as.matrix(), '/')) %>% 
  mutate(label = nrc_summary$label)

summary <- summary %>% 
  inner_join(nrc_summary_perc, by = c('label'))


######## Hierarchical Clustering ############


summary_scaled <- scale(summary[-1])

# Complete Linkage
hierarchical_cluster_complete <- hclust(dist(summary_scaled), method = 'complete')
plot(hierarchical_cluster_complete, main = "Complete Linkage", xlab = "" , sub = "")

cutree(hierarchical_cluster_complete, 3)

# Average Linkage
hierarchical_cluster_avg <- hclust(dist(summary_scaled), method = 'average')
plot(hierarchical_cluster_avg, main = "Average Linkage", xlab = "" , sub = "")

cutree(hierarchical_cluster_avg, 3)


############# K-Means Clustering #####################

k_max <- nrow(summary) - 1

kmeans_out <- sapply(1:k_max, 
              function(k){kmeans(summary_scaled, k, nstart = 50, iter.max = 15)$tot.withinss})

save(kmeans_out, file = 'data/kmeans_out.dat')

plot(1:k_max, kmeans_out
     , type = "b"
     , pch = 19
     , frame = FALSE
     , xlab="Number of clusters K"
     , ylab="Total within-clusters sum of squares")
