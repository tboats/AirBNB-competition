## Goal: apply cross validation to determine tuning parameters

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


###########################################################################
## merge training data with country data

# grab country labels from original data set
df_train <- read.csv("../data/train_users_2.csv")
labels2 <- df_train$country_destination

ydf <- data.frame(country_destination = labels2)
df <- cbind(ydf, X)

# load countries data
df_countries <- read.csv("../data/countries.csv")

# merge the countries data with training data
df <- merge(df, df_countries, by = "country_destination", all.x = TRUE, sort = FALSE)
df[is.na(df$distance_km),"distance_km"] <- 0
df[which(is.na(df$destination_language)),"destination_language"] <- "eng"


# save the column of interest as a label (distance_km)
label_int <- df$distance_km # intermediate label
label_lang <- df$destination_language # intermediate label

# remove all columns from countries
df <- df[, !(names(df) %in% names(df_countries))]

###########################################################################
## boosting on language
trainFraction <- 0.8
nClasses <- length(unique(label_lang))

# split into training and validation
trainIndex <- createDataPartition(label_int, p = trainFraction, list = FALSE)
df_tr <- df[trainIndex,]
df_val <- df[-trainIndex,]
label_tr <- as.integer(as.numeric(label_lang)[trainIndex]-1)
label_val <- as.integer(as.numeric(label_lang)[-trainIndex]-1)
label_val_fact <- as.character(label_lang[-trainIndex])

# convert factor to numeric
lang_levels <- levels(label_lang)
# table(label_tr)
# label_tr_num <- as.numeric(label_tr)
# table(label_tr_num)
# label_tr_fact <- factor(label_tr_num)
# levels(label_tr_fact) <- lang_levels
# table(label_tr_fact)


xgb_lang <- xgboost(data = data.matrix((df_tr[,-1])), #data.matrix(X_tr), #data.matrix(X_tr[,-1]), 
               label = data.matrix(label_tr), 
               eta = 0.1,
               max_depth = 6,
               objective = "multi:softprob",
               nthread = 3,
               nrounds = 80, #originally 25
               seed = 1,
               eval_metric = "mlogloss", #ndcg5, #eval_metric,#ndcg5,#"mlogloss",#"merror",
               num_class = nClasses
)


if (trainFraction < 1){
  pred_lang <- predict(xgb_lang, data.matrix(df_val[,-1]))
  
  # extract the N classes with highest probabilities and save to data frame
  predictions <- as.data.frame(matrix(pred_lang, nrow=nClasses))
  pred_val <- data.frame(t(predictions))
  names(pred_val) <- lang_levels
  
  # add in IDs
  X2 <- cbind(df_val$id, pred_val)
  names(X2)[1] <- "id"
  
  # predict on full data set and write to csv
  pred_lang <- predict(xgb_lang, data.matrix(df[,-1]))
  predictions <- as.data.frame(matrix(pred_lang, nrow=nClasses))
  pred_val <- data.frame(t(predictions))
  names(pred_val) <- lang_levels
  
  # add in IDs
  X2 <- cbind(df$id, pred_val)
  names(X2)[1] <- "id"
  
  # write to csv
  X2 <- join(X, X2)
  write.csv(X2, "X_lang.csv")
  
  # pull out the top prediction for basic evaluation of model
  predictions_top1 <- as.vector(apply(pred_val, 1, function(x) names(sort(x, decreasing = TRUE)[1])))
  
  # compute accuracy
  pred_acc <- sum(predictions_top1 == label_val_fact)/length(label_val_fact)
  print(pred_acc)
  table(label_val_fact)/length(label_val_fact)
}


####################################################################################
## apply training model on test data
pred_lang_test <- predict(xgb_lang, data.matrix(X_test[,-1]))

# extract the N classes with highest probabilities and save to data frame
predictions <- as.data.frame(matrix(pred_lang_test, nrow=nClasses))
pred_test <- data.frame(t(predictions))
names(pred_test) <- lang_levels

# add in IDs
X_test2 <- cbind(X_test$id, pred_test)
names(X_test2)[1] <- "id"

# write to csv
X_test2 <- join(X_test, X_test2)
write.csv(X_test2, "X_test_lang.csv")







###########################################################################
## linear regression to find distance_km
trainFraction <- 0.8
trainIndex <- createDataPartition(label_int, p = trainFraction, list = FALSE)
df_tr <- df[trainIndex,]
df_val <- df[-trainIndex,]
label_tr <- label_int[trainIndex]
label_val <- label_int[-trainIndex]


params <- list(
  eta = 0.1,
  max_depth = 6,
  objective = "reg:linear",
  nthread = 3
)

xgb_distanceReg <- xgboost(data = data.matrix(df_tr[,-1]), #data.matrix(X_tr), #data.matrix(X_tr[,-1]), 
               label = label_tr,
               params = params,
               nrounds = 80
)

## check on validation set
distancePred <- predict(xgb_distanceReg, data.matrix(df_val[,-1])) #data.matrix(X_val))#data.matrix(X_val[,-1]))
dist <- data.frame(distance = label_val, predictedDistance = distancePred)
head(dist,100)

distForeign <- filter(dist, distance > 0)
tail(distForeign,25)

# cv <- xgb.cv(data = data.matrix(df[,-1]),#data.matrix(X_tr[,-1]), 
#             label = label_int, 
#             params = params,
#             nrounds = 150,
#             prediction = TRUE,
#             nfold = 5
# )
# ts <- timestamp()
# write.csv(data.frame(cv$dt), paste("dt_distanceRegression_",ts,'.csv',sep=""))

# eta = eta,
# max_depth = max_depth, #originally 9
# nrounds = nround, #originally 25
# #subsample = subsample,#0.5,
# #colsample_bytree = colsample_bytree, #0.5
# #seed = 1,
# #eval_metric = eval_metric, #ndcg5, #eval_metric,#ndcg5,#"mlogloss",#"merror",
# objective = objective,
# lambda = lambda,
# alpha = alpha,
# gamma = gamma,
# num_class = 12,
# nthread = 3
