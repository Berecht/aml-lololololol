setwd("~/Desktop")

library(xgboost)

data <- read.csv(file="train.csv", header=TRUE, sep=",")
test_data <- read.csv(file="test.csv", header=TRUE, sep=",")

#keep_id <- unique(train$id)
#train_instances <- 0.8
#split data random into train and test
#dt <- sample(keep_id, train_instances*length(keep_id))

#train <- subset(data, id %in% dt)
#val <- subset(data, !(id %in% dt))

#dat<- train[,c(-1)]
dat <- data
dat$carrier <- as.factor(dat$carrier)
carrier_level <- levels(dat$carrier)
dat$carrier <- as.numeric(dat$carrier)

dat$origin <- as.factor(dat$origin)
origin_level <- levels(dat$origin)
dat$origin <- as.numeric(dat$origin)

dat$dest <- as.factor(dat$dest)
dest_level <- levels(dat$dest)
dat$dest <- as.numeric(dat$dest)

########################################
test_data$carrier <- factor(test_data$carrier, levels=carrier_level)
test_data$carrier <- as.numeric(test_data$carrier)

test_data$origin <- factor(test_data$origin , levels=origin_level)
test_data$origin  <- as.numeric(test_data$origin )

test_data$dest <- factor(test_data$dest, levels=dest_level)
test_data$dest <- as.numeric(test_data$dest)


keep_id <- unique(dat$id)
train_instances <- 0.8
#split data random into train and test
dt <- sample(keep_id, train_instances*length(keep_id))

train <- subset(dat, id %in% dt)
val <- subset(dat, !(id %in% dt))

train <- train[,c(-1)]
val <- val[,c(-1)]


train_mat <- as.matrix(train)
val_mat <- as.matrix(val)

dtrain <- xgb.DMatrix(data = train_mat[,c(2:length(train_mat[1,]))], label = train_mat[,1])
dval <- xgb.DMatrix(data = val_mat[,c(2:length(dat2[1,]))], label = val_mat[,1])

watchlist <- list(train=dtrain, test=dval)

bst <- xgb.train(data=dtrain, max.depth=5, eta=0.03, nthread = 5, nrounds=2000, watchlist=watchlist, objective = "binary:logistic", verbose=1)


train2 <- dat[,c(-1)]
train2_mat <- as.matrix(train2)
dtrain <- xgb.DMatrix(data = train2_mat[,c(2:length(train2_mat[1,]))], label = train2_mat[,1])
bstDMatrix <- xgboost(data = dtrain, max.depth = 5, eta = 0.04, nthread = 5, nrounds = 2000, objective = "binary:logistic", verbose = 1)

test_data
test <- as.matrix(test_data[,c(-1)])
#dtest <- xgb.DMatrix(data = test[,c(2:length(test[1,]))])
pred <- predict(bstDMatrix, test)

test_prediction <- data.frame(test_data[,c(1)], pred)
names(test_prediction) <- c("id", "is_delayed")
write.csv(test_prediction, file = "prediction-AML.csv", row.names = FALSE)
