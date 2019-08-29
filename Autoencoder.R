library(keras)
library(tensorflow)
library(ROCR)
library(autoencoder)


ntrain = sample(nrow(all.judge),floor(0.7*nrow(all.judge)),replace=FALSE)
train = all.judge[ntrain,]
test = all.judge[-ntrain,]
train <- train[,-which(names(train)=="mcq240y")]
train <- train[,-which(names(train)=="mcq240r")]
train <- train[,-which(names(train)=="mcq240k")]
train <- train[,-which(names(train)=="mcq240i")]
train <- train[,-which(names(train)=="mcq240d")]
train <- train[,-which(names(train)=="mcq230d")]
test <- test[,-which(names(test)=="mcq240y")]
test <- test[,-which(names(test)=="mcq240r")]
test <- test[,-which(names(test)=="mcq240k")]
test <- test[,-which(names(test)=="mcq240i")]
test <- test[,-which(names(test)=="mcq240d")]
test <- test[,-which(names(test)=="mcq230d")]

auto.train.x <- as.matrix(train[,3:1278])
auto.train.y <- as.matrix(trian[,2])
auto.test.x <- as.matrix(test[1:100,3:1278])
auto.test.y <- as.matrix(trian[1:100,2])

input_layer <- layer_input(shape = c(1276))

encoder <- 
  input_layer %>% 
  layer_dense(units = 500, activation = "relu") %>% 
  layer_batch_normalization() %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 300, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 150, activation = "relu") %>%
  layer_dense(units = 100)

decoder <- 
  encoder %>% 
  layer_dense(units = 150, activation = "relu") %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 300, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 500, activation = "relu") %>%
  layer_dense(units = 1276)

autoencoder.model <- keras_model(inputs = input_layer, outputs = decoder)

autoencoder.model %>% compile(
  loss='mean_squared_error',
  optimizer='adam',
  metrics = c('accuracy')
)

summary(autoencoder.model)

history <-
  autoencoder.model %>%
  keras::fit(auto.train.x,
             auto.train.x,
             epochs=20,
             shuffle=TRUE,
             validation_data= list(auto.test.x,auto.test.x)
  )

layer_name <- 'dense_28'
intermediate_layer_model <- keras_model(inputs = autoencoder.model$input,
                                        outputs = get_layer(autoencoder.model,layer_name)$output)
intermediate_output <- predict(intermediate_layer_model,auto.test.x)

cor.auto <- cor(auto.test.x,method = "spearman")
