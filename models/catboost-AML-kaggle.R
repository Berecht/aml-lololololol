setwd("~/Desktop")

library(catboost)

data <- read.csv(file="train.csv", header=TRUE, sep=",")
test_data <- read.csv(file="test.csv", header=TRUE, sep=",")

keep_id <- unique(data$id)
train_instances <- 0.8
#split data random into train and test
dt <- sample(keep_id, train_instances*length(keep_id))

train <- subset(data, id %in% dt)
val <- subset(data, !(id %in% dt))

cat_features <- c(5,6,7)

features <- data.frame(train[,c(4:length(train[1,]))])
labels <- train$is_delayed
train_pool <- catboost.load_pool(data = features, label = labels, cat_features = cat_features)
#train_pool <- catboost.load_pool(data = features, label = labels)


val_features <- data.frame(val[,c(4:length(val[1,]))])
val_labels <- val$is_delayed
val_pool <- catboost.load_pool(data = val_features, label = val_labels, cat_features = cat_features)
#val_pool <- catboost.load_pool(data = val_features, label = val_labels)


parameters <- list(loss_function='Logloss', eval_metric='AUC', iterations=500, learning_rate=0.15, depth=10, one_hot_max_size=31, l2_leaf_reg= 9)

model <- catboost.train(train_pool, test_pool = val_pool, params = parameters)

###############################
cat_features <- c(5,6,7)

features <- data.frame(data[,c(4:length(data[1,]))])
labels <- data$is_delayed
train_pool <- catboost.load_pool(data = features, label = labels, cat_features = cat_features)

model2 <- catboost.train(train_pool, params = parameters)


##############################
test <- test_data[,c(-1)]
test_pool <- catboost.load_pool(data = test, cat_features = cat_features)
prediction <- catboost.predict(model2, pool = test_pool, prediction_type = 'Probability')

test_prediction <- data.frame(test_data[,c(1)], prediction)
names(test_prediction) <- c("id", "is_delayed")
write.csv(test_prediction, file = "prediction-AML-catboost2.csv", row.names = FALSE)


featuresImportance <- catboost.get_feature_importance(model2)
