
########### Must run build_sentiments.R first!! #############################

tidy_dtm_grouped_pca <- select(tidy_dtm_grouped, neutral, positive, trust, anticipation,
                               negative, joy, surprise, disgust, anger, sadness, fear)

scaled_data <- model.matrix(~ ., data = drop_na(tidy_dtm_grouped_pca))[,-1]
# scaled_data <- scale(model.matrix(~ ., data = drop_na(tidy_dtm_grouped_pca))[,-1])

pca_results <- prcomp(scaled_data)
summary(pca_results)

######### Graph Variance of PCA
pca_results2 <- tibble(Stand_Dev = summary(pca_results)$importance[1,]
       , Variance = summary(pca_results)$importance[2,]
       , Cum_Variance = summary(pca_results)$importance[3,]
       , Principal_Component = seq(1, 11, 1))

ggplot(pca_results2, aes(x = Principal_Component, y = Variance)) +
geom_bar(stat = 'identity', fill = 'black') +
theme_fivethirtyeight() +
geom_text(aes(label = round(Variance, 2)), vjust=0)

# (pca_results$rotation)[,1:2]
pc1 <- (pca_results$rotation)[,1]
pc2 <- (pca_results$rotation)[,2]

pc1 <- tibble(variables = names(pc1), loadings = pc1)
pc2 <- tibble(variables = names(pc2), loadings = pc2)

pc1 %>% arrange(desc(loadings))
pc2 %>% arrange(desc(loadings))

biplot(pca_results, ylim = c(-.03, .03))


###################################

tidy_dtm_grouped %>% 
  group_by(country) %>% 
  summarise(count = sum(neutral)) %>% 
  ggplot(aes(x = country, y = count)) +
  geom_bar(stat = 'identity')
