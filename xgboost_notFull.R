

########## Must Run xgboost_full.R first! ##################


# Same as Full Model, but also eliminates Number of Likes, Number of Words, and Number of Comments
x <- model.matrix(~ ., data = select(training_data, -country, -university, -document, -year, -month, -day, -hour, -state,
                                     -region, -num_likes, -num_words, -num_comments))[,-1]
y <- select(training_data, country) %>% as.matrix()

dtrain <- xgb.DMatrix(data = x, label = y)

x_test <- model.matrix(~ ., data = select(validation_data, -country, -university, -document, -year, -month, -day, -hour, -state,
                                          -region, -num_likes, -num_words, -num_comments))[,-1]


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

boosted_accuracy_woNumLikes <- tibble('Model' = 'Boosted Tree', 'Accuracy' = round(mean(validation_data_xgb$accuracy), 3))


############### Random Forest ##############


rf_model <- randomForest(country ~ ., data = select(training_data, -university, -document, -year, -month, -day, -hour, -state,
                                                    -region, -num_likes, -num_words, -num_comments))

validation_data_rf <- validation_data %>% 
  mutate(prediction = predict(rf_model, validation_data)
         ,accuracy = ifelse(country == prediction, 1, 0))

rf_accuracy_woNumLikes <- tibble('Model' = 'Random Forest', 'Accuracy' = round(mean(validation_data_rf$accuracy), 3))

varImpPlot(rf_model)


############## Bagging ####################


bag_model <- randomForest(country ~ .
                          , data = select(training_data, -university, -document, -year, -month, -day, -hour, -state,
                                          -region, -num_likes, -num_words, -num_comments)
                          , mtry = dim(select(training_data, -university, -document, -year, -month, -day, -hour, -state,
                                              -region, -num_likes, -num_words, -num_comments))[2] - 1)

validation_data_bag <- validation_data %>% 
  mutate(prediction = predict(bag_model, validation_data)
         ,accuracy = ifelse(country == prediction, 1, 0))

bag_accuracy_woNumLikes <- tibble('Model' = 'Bagging', 'Accuracy' = round(mean(validation_data_rf$accuracy), 3))


###############################################

accuracies <- bind_rows(boosted_accuracy_woNumLikes, rf_accuracy_woNumLikes, bag_accuracy_woNumLikes)
accuracies %>% kable()


#############################################
#############################################
####### A Simpler Model ###################
#############################################
#############################################


x <- model.matrix(~ ., data = select(training_data, neutral, avg_afinn, iu_ratio, avg_bing, positive, trust, anticipation,
                                     negative, joy, surprise, disgust, anger, sadness, fear))[,-1]
y <- select(training_data, country) %>% as.matrix()

dtrain <- xgb.DMatrix(data = x, label = y)

x_test <- model.matrix(~ ., data = select(validation_data, neutral, avg_afinn, iu_ratio, avg_bing, positive, trust, anticipation,
                                          negative, joy, surprise, disgust, anger, sadness, fear))[,-1]

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

boosted_accuracy_woNumLikes <- tibble('Model' = 'Boosted Tree', 'Accuracy' = round(mean(validation_data_xgb$accuracy), 3))


############### Random Forest ##############


rf_model <- randomForest(country ~ ., data = select(training_data, country, neutral, avg_afinn, iu_ratio, avg_bing, positive, trust, anticipation,
                                                    negative, joy, surprise, disgust, anger, sadness, fear))

validation_data_rf <- validation_data %>% 
  mutate(prediction = predict(rf_model, validation_data)
         ,accuracy = ifelse(country == prediction, 1, 0))

rf_accuracy_woNumLikes <- tibble('Model' = 'Random Forest', 'Accuracy' = round(mean(validation_data_rf$accuracy), 3))

varImpPlot(rf_model)


############## Bagging ####################


bag_model <- randomForest(country ~ .
                          , data = select(training_data, country, neutral, avg_afinn, iu_ratio, avg_bing, positive, trust, anticipation,
                                          negative, joy, surprise, disgust, anger, sadness, fear)
                          , mtry = dim(select(training_data, country, neutral, avg_afinn, iu_ratio, avg_bing, positive, trust, anticipation,
                                              negative, joy, surprise, disgust, anger, sadness, fear))[2] - 1)

validation_data_bag <- validation_data %>% 
  mutate(prediction = predict(bag_model, validation_data)
         ,accuracy = ifelse(country == prediction, 1, 0))

bag_accuracy_woNumLikes <- tibble('Model' = 'Bagging', 'Accuracy' = round(mean(validation_data_bag$accuracy), 3))


###############################################

accuracies <- bind_rows(boosted_accuracy_woNumLikes, rf_accuracy_woNumLikes, bag_accuracy_woNumLikes)
accuracies %>% kable()


#############################################
#############################################
####### Only NRC Sentiments ###################
#############################################
#############################################


x <- model.matrix(~ ., data = select(training_data, neutral, positive, trust, anticipation,
                                     negative, joy, surprise, disgust, anger, sadness, fear))[,-1]
y <- select(training_data, country) %>% as.matrix()

dtrain <- xgb.DMatrix(data = x, label = y)

x_test <- model.matrix(~ ., data = select(validation_data, neutral, positive, trust, anticipation,
                                          negative, joy, surprise, disgust, anger, sadness, fear))[,-1]

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

validation_data_xgb2 <- validation_data %>% 
  mutate(prediction = predict(xgboost_model, x_test)
         ,accuracy = ifelse(country == prediction, 1, 0))

boosted_accuracy_woNumLikes2 <- tibble('Model' = 'Boosted Tree', 'Accuracy' = round(mean(validation_data_xgb2$accuracy), 3))


############### Random Forest ##############


rf_model <- randomForest(country ~ ., data = select(training_data, country, neutral, positive, trust, anticipation,
                                                    negative, joy, surprise, disgust, anger, sadness, fear))

validation_data_rf2 <- validation_data %>% 
  mutate(prediction = predict(rf_model, validation_data)
         ,accuracy = ifelse(country == prediction, 1, 0))

rf_accuracy_woNumLikes2 <- tibble('Model' = 'Random Forest', 'Accuracy' = round(mean(validation_data_rf2$accuracy), 3))

varImpPlot(rf_model)
# partialPlot(rf_model, model.matrix(~ ., select(training_data, country, neutral, positive, trust, anticipation,
#                                   negative, joy, surprise, disgust, anger, sadness, fear))[,-1], "neutral")

############## Bagging ####################


bag_model <- randomForest(country ~ .
                          , data = select(training_data, country, neutral, positive, trust, anticipation,
                                          negative, joy, surprise, disgust, anger, sadness, fear)
                          , mtry = dim(select(training_data, country, neutral, positive, trust, anticipation,
                                              negative, joy, surprise, disgust, anger, sadness, fear))[2] - 1)

validation_data_bag2 <- validation_data %>% 
  mutate(prediction = predict(bag_model, validation_data)
         ,accuracy = ifelse(country == prediction, 1, 0))

bag_accuracy_woNumLikes2 <- tibble('Model' = 'Bagging', 'Accuracy' = round(mean(validation_data_bag2$accuracy), 3))


###############################################


accuracies <- bind_rows(boosted_accuracy_woNumLikes2, rf_accuracy_woNumLikes2, bag_accuracy_woNumLikes2)
accuracies %>% kable()


#############################################
#############################################
####### Without Neutral ###################
#############################################
#############################################


x <- model.matrix(~ ., data = select(training_data, positive, trust, anticipation,
                                     negative, joy, surprise, disgust, anger, sadness, fear))[,-1]
y <- select(training_data, country) %>% as.matrix()

dtrain <- xgb.DMatrix(data = x, label = y)

x_test <- model.matrix(~ ., data = select(validation_data, positive, trust, anticipation,
                                          negative, joy, surprise, disgust, anger, sadness, fear))[,-1]

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

validation_data_xgb2 <- validation_data %>% 
  mutate(prediction = predict(xgboost_model, x_test)
         ,accuracy = ifelse(country == prediction, 1, 0))

boosted_accuracy_woNumLikes2 <- tibble('Model' = 'Boosted Tree', 'Accuracy' = round(mean(validation_data_xgb2$accuracy), 3))


############### Random Forest ##############


rf_model <- randomForest(country ~ ., data = select(training_data, country, positive, trust, anticipation,
                                                    negative, joy, surprise, disgust, anger, sadness, fear))

validation_data_rf2 <- validation_data %>% 
  mutate(prediction = predict(rf_model, validation_data)
         ,accuracy = ifelse(country == prediction, 1, 0))

rf_accuracy_woNumLikes2 <- tibble('Model' = 'Random Forest', 'Accuracy' = round(mean(validation_data_rf2$accuracy), 3))

varImpPlot(rf_model)
# partialPlot(rf_model, model.matrix(~ ., select(training_data, country, neutral, positive, trust, anticipation,
#                                   negative, joy, surprise, disgust, anger, sadness, fear))[,-1], "neutral")

############## Bagging ####################


bag_model <- randomForest(country ~ .
                          , data = select(training_data, country, positive, trust, anticipation,
                                          negative, joy, surprise, disgust, anger, sadness, fear)
                          , mtry = dim(select(training_data, country, positive, trust, anticipation,
                                              negative, joy, surprise, disgust, anger, sadness, fear))[2] - 1)

validation_data_bag2 <- validation_data %>% 
  mutate(prediction = predict(bag_model, validation_data)
         ,accuracy = ifelse(country == prediction, 1, 0))

bag_accuracy_woNumLikes2 <- tibble('Model' = 'Bagging', 'Accuracy' = round(mean(validation_data_bag2$accuracy), 3))


###############################################


accuracies <- bind_rows(boosted_accuracy_woNumLikes2, rf_accuracy_woNumLikes2, bag_accuracy_woNumLikes2)
accuracies %>% kable()