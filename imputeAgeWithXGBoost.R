###########################################################################
## load libraries
library(xgboost)
library(stringr)
library(caret)
library(plyr)
library(dplyr)
library(lubridate)
source('E:/Dropbox/R/general/timestamp.R')

ndcg5 <- function(preds, dtrain) {
  
  labels <- getinfo(dtrain,"label")
  num.class = length(unique(labels))
  pred <- matrix(preds, nrow = num.class)
  top <- t(apply(pred, 2, function(y) order(y)[num.class:(num.class-4)]-1))
  
  x <- ifelse(top==labels,1,0)
  dcg <- function(y) sum((2^y - 1)/log(2:(length(y)+1), base = 2))
  ndcg <- mean(apply(x,1,dcg))
  return(list(metric = "ndcg5", value = ndcg))
}
set.seed(1)

###########################################################################
## load data with engineered features
X <- read.csv("X.csv")
y <- read.csv("y.csv")
X_test <- read.csv("X_test.csv")

## convert y to data matrix
y <- data.matrix(y)

## combine X and X_test
X_all <- rbind(X, X_test)

###########################################################################
## split off a set where age is undefined
noAgeLoc <- which(X_all$age == -1)
X_noAge <- X_all[noAgeLoc,]
X_age <- X_all[-noAgeLoc,]

###########################################################################
## assign age buckets
# ageBuckets <- seq(from=20, to=100, by=10)
ageBuckets <- c(seq(from=18, to=35, by = 3), seq(from=40, to=100, by=10))
assignAgeBucket <- function(age, ageBuckets=ageBuckets){
  ageDiff <- abs(ageBuckets - age)
  ageMinLoc <- which(ageDiff == min(ageDiff))
  ageBuckets[ageMinLoc[1]]
}
# assignAgeBucket(49, ageBuckets)
X_age$ageBucket <- sapply(X_age$age, assignAgeBucket, ageBuckets)

###########################################################################
## divide into training and validation set for age buckets
trainFraction <- 0.7
y_ab <- as.factor(X_age$ageBucket)
ageLevels <- levels(y_ab)
y_ab <- as.numeric(y_ab)-1
ageCols <- grep('^age', names(X_age), value = TRUE)
X_age_lite <- X_age[,!(names(X_age) %in% ageCols)]

trainIndex <- createDataPartition(y_ab, p = trainFraction, list = FALSE)
X_age_lite_tr <- X_age_lite[trainIndex,]
X_age_lite_val <- X_age_lite[-trainIndex,]
y_ab_tr <- as.integer(y_ab[trainIndex])
y_ab_val <- as.integer(y_ab[-trainIndex])

###########################################################################
## train age predictor
uClasses <- unique(y_ab_tr)
nClasses <- length(uClasses)
xgb_age <- xgboost(data = data.matrix((X_age_lite_tr[,-1])), #data.matrix(X_tr), #data.matrix(X_tr[,-1]), 
                    label = data.matrix(y_ab_tr), 
                    eta = 0.1,
                    max_depth = 6,
                    objective = "multi:softprob",
                    nthread = 3,
                    nrounds = 250, #originally 25
                    seed = 1,
                    eval_metric = "mlogloss", #ndcg5, #eval_metric,#ndcg5,#"mlogloss",#"merror",
                    num_class = nClasses
)

# predict age bins
pred_age <- predict(xgb_age, data.matrix(X_age_lite_val[,-1]))

# extract the N classes with highest probabilities and save to data frame
predictions <- as.data.frame(matrix(pred_age, nrow=nClasses))
pred_val <- data.frame(t(predictions))
names(pred_val) <- ageLevels



##################################################################
# pull out the top prediction for basic evaluation of model
predictions_top1 <- as.vector(apply(pred_val, 1, function(x) names(sort(x, decreasing = TRUE)[1])))

# assign labels to validation output
y_ab_val_lab <- as.factor(y_ab_val)
levels(y_ab_val_lab) <- ageLevels
y_ab_val_lab <- as.character(y_ab_val_lab)

# compute accuracy
pred_acc <- sum(predictions_top1 == y_ab_val_lab)/length(y_ab_val_lab)
print(pred_acc)
table(y_ab_val_lab)/length(label_val_fact)

# output file
ts <- timestamp()
agePredDF <- data.frame(id = X_age_lite_val$id, ageBucket = y_ab_val_lab, ageBucketPred = predictions_top1)
ageName <- paste("agePrediction_", ts, "_acc=", round(pred_acc, digits=3), ".csv", sep="")
write.csv(agePredDF, ageName)

# add in IDs
# X2 <- cbind(df_val$id, pred_val)
# names(X2)[1] <- "id"
#

###########################################################################
## predict age buckets on test data
pred_age_test <- predict(xgb_age, data.matrix(X_test[,-1]))

# extract the N classes with highest probabilities and save to data frame
pred_test <- as.data.frame(matrix(pred_age_test, nrow=nClasses))
pred_test <- data.frame(t(pred_test))
ageBucketNames <- paste("ageBucket_", ageLevels, sep="")
names(pred_test) <- ageBucketNames #ageLevels

# combine the age prediction with the test data set
X_test_age <- cbind(X_test, pred_test)
head(X_test_age)

## write to disk
ts <- timestamp()
saveName <- paste("X_test_ageBuckets_", ts, ".csv", sep="")
write.csv(X_test_age, saveName)


###########################################################################
## predict age buckets on training data
pred_age_tr <- predict(xgb_age, data.matrix(X[,-1]))

# extract the N classes with highest probabilities and save to data frame
pred_tr <- as.data.frame(matrix(pred_age_tr, nrow=nClasses))
pred_tr <- data.frame(t(pred_tr))
ageBucketNames <- paste("ageBucket_", ageLevels, sep="")
names(pred_tr) <- ageBucketNames #ageLevels

# combine the age prediction with the test data set
X_tr_age <- cbind(X, pred_tr)
head(X_tr_age)

## write to disk
ts <- timestamp()
saveName <- paste("X_tr_ageBuckets_", ts, ".csv", sep="")
write.csv(X_tr_age, saveName)

## take these data frame to XGBoost training









###########################################################################
## run cross validation on data sets
# set up the cross-validated hyper-parameter search
xgb_grid_1  <- expand.grid(
  eta = c(0.03, 0.1, 0.3),
  max_depth = c(5,6,7),#c(6, 7, 8),
  nrounds = c(50,80,100,120,150)
)

# pack the training control parameters
xgb_trcontrol_1 <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  allowParallel = TRUE
)

# randomly select some fraction of the data
# trainFraction <- 0.7
# trainIndex <- createDataPartition(y, p = trainFraction, list = FALSE)
# y1 <- y[trainIndex]
# X1 <- X[trainIndex,]


# train the model for each parameter combination in the grid, 
#   using CV to evaluate
cv <- train(
  x = as.matrix(X_age_lite_tr %>% select(-id)),
  y = as.matrix(y_ab_tr),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)

ts <- timestamp()
saveName <- paste("cv_", ts, '.csv', sep="")
write.csv(cv$results, saveName, row.names = FALSE)

ggplot(cv$results, aes(x = as.factor(eta), y = max_depth, col = Rsquared, size = Rsquared)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

plotSaveName <- paste("eta-vs-max_depth_", ts, '.png', sep = "")
ggsave(plotSaveName)