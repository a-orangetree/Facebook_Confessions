library(keras)
library(data.table)

# Prep data
tidy_dtm_grouped_noNa <- drop_na(tidy_dtm_grouped) 
num_unique_universities <- length(unique(tidy_dtm_grouped$university))

train_data <- sample_frac(tidy_dtm_grouped_noNa, .75)
test_data <- anti_join(tidy_dtm_grouped_noNa, train_data)

# Create train and test sets
x_train <- model.matrix(~ ., data = select(train_data, -university))[,-1]
y_train <- model.matrix(~ train_data$university)
# for(unique_value in unique(train_data$university)) {y_train[paste("university", unique_value, sep = ".")] <- ifelse(train_data$university == unique_value, 1, 0)}

x_test <- model.matrix(~ ., data = select(test_data, -university))[,-1]
y_test <- model.matrix(select(test_data, university))

# Standardize
train_mean <- apply(x_train, 2, mean)
train_std <- apply(x_train, 2, sd)

x_train <- scale(x_train, center = train_mean, scale = train_std)
x_test <- scale(x_test, center = train_mean, scale = train_std)

# Build model
build_model <- function() {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 64, activation = "relu", input_shape = dim(x_train)[[2]]) %>% 
    layer_dense(units = 64, activation = "softmax") %>% 
    layer_dense(units = num_unique_universities)
  
  model %>% compile(
    optimizer = "nadam", 
    loss = "categorical_crossentropy", 
    metrics = c("accuracy")
  )
}

k <- 5
num_epochs <- 1000
indices <- sample(1:nrow(x_train))
folds <- cut(1:length(indices), breaks = k, labels = FALSE) 
all_mse_histories <- NULL


for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- x_train[val_indices,]
  val_targets <- y_train[val_indices]
  
  partial_train_data <- x_train[-val_indices,]
  partial_train_targets <- y_train[-val_indices]
  
  model <- build_model()
  
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 1, verbose = 0
  )
  
  mse_history <- history$metrics$val_mean_squared_error
  all_mse_histories <- rbind(all_mse_histories, mse_history)
}

average_mse_history <- data.frame(
  epoch = seq(1:ncol(all_mse_histories)),
  validation_mse = apply(all_mse_histories, 2, mean)
)

ggplot(average_mse_history, aes(x = epoch, y = validation_mse)) + geom_point()

model <- build_model()

model %>% fit(x_train, y_train,
              epochs = 60, batch_size = 1, verbose = 0)


result <- model %>% evaluate(x_test, y_test)
sqrt(result$mean_squared_error)