library(xgboost)
library(randomForest)
library(ggthemes)


##### Must run build_sentiments.R first ################


number_of_classes <- length(unique(tidy_dtm_grouped$country))

tidy_dtm_grouped$country <- factor(ifelse(tidy_dtm_grouped$country == 'united_states', 1
                                          , ifelse(tidy_dtm_grouped$country == 'canada', 2, 0))) 


training_data <- sample_frac(drop_na(tidy_dtm_grouped), .8)
validation_data <- anti_join(drop_na(tidy_dtm_grouped), training_data)


####### Boosted Tree ############


x <- model.matrix(~ ., data = select(training_data, -country, -university, -document, -year, -month, -day, -hour, -state, -region))[,-1]
y <- select(training_data, country) %>% as.matrix()

dtrain <- xgb.DMatrix(data = x, label = y)

x_test <- model.matrix(~ ., data = select(validation_data, -country, -university, -document, -year, -month, -day, -hour, -state, -region))[,-1]


xgboost_cv <- xgb.cv(data = dtrain
                     , nrounds = 10000
                     , nfold = 10
                     , early_stopping_rounds = 10
                     , num_class = number_of_classes
                     , objective = "multi:softmax"
                     ,verbose = FALSE)

xgboost_model <- xgboost(data = dtrain
                         ,objective = "multi:softmax"
                         ,num_class = number_of_classes
                         ,nrounds = xgboost_cv$best_iteration
                         ,verbose = FALSE)

validation_data_xgb <- validation_data %>% 
  mutate(prediction = predict(xgboost_model, x_test)
         ,accuracy = ifelse(country == prediction, 1, 0))

tibble('Model' = 'Boosted Tree', 'Accuracy' = round(mean(validation_data_xgb$accuracy), 3)) %>% kable()
        

############### Random Forest ##############
        
        
rf_model <- randomForest(country ~ ., data = select(training_data, -university, -document, -year, -month, -day, -hour, -state, -region))
        
validation_data_rf <- validation_data %>% 
  mutate(prediction = predict(rf_model, validation_data)
         ,accuracy = ifelse(country == prediction, 1, 0))

tibble('Model' = 'Random Forest', 'Accuracy' = round(mean(validation_data_rf$accuracy), 3)) %>% kable()

varImpPlot(rf_model)


############## Bagging ####################


bag_model <- randomForest(country ~ .
                          , data = select(training_data, -university, -document, -year, -month, -day, -hour, -state, -region)
                          , mtry = dim(select(training_data, -university, -document, -year, -month, -day, -hour, -state, -region))[2] - 1)

validation_data_bag <- validation_data %>% 
  mutate(prediction = predict(bag_model, validation_data)
         ,accuracy = ifelse(country == prediction, 1, 0))

tibble('Model' = 'Bagging', 'Accuracy' = round(mean(validation_data_rf$accuracy), 3)) %>% kable()


########### Boxplot of Num Likes ##############

ggplot(tidy_dtm_grouped) +
  geom_boxplot(aes(x = country, y = num_likes)) +
  theme_fivethirtyeight()