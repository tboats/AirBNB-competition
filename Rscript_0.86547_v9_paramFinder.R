# This R script is based on Sandro's python script, which produces a LB score of 0.8655
# This script should produce a LB score of 0.86547

# v3 integrates basic session statistics

# load libraries
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(plyr)
library(dplyr)
library(timeDate)
library(lubridate)
library(randomForest)
library(mice)
# library(DMwR)
source('L:/Ideas/kaggle_AirBNB/scriptsR/DCG.R')
source('E:/Dropbox/R/general/getHolidays.R', echo=FALSE)
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
flagImpute <- FALSE

############################################################################
# load data
df_train <- read_csv("../data/train_users_2.csv")
df_test <- read_csv("../data/test_users.csv")
labels <- df_train['country_destination']
labels2 <- df_train$country_destination
df_train_noLabels <- df_train[-grep('country_destination', colnames(df_train))]

## load features derived from sessions.csv
dfSessionStats <- read.csv("dfSessions_byUser.csv")
#dfSessionStats <- dfSessionStats[,-grep('X', colnames(dfSessionStats))]

# dfActionDetails <- read.csv("dfSessions_actionDetails.csv")
# dfDeviceStats <- read.csv("dfSessions_deviceStats.csv")
# dfActions <- read.csv("dfSessions_actions_2016-01-21_20-02-09.csv")
# dfActionTypes <- read.csv("dfSessions_actionTypes_2016-01-21_20-03-16.csv")

## restricted feature set
# dfActionDetails <- read.csv("dfSessions_allActionDetails_2016-02-06_11-01-00.csv")
# dfDeviceStats <- read.csv("dfSessions_allDeviceTypes_2016-02-06_00-02-53.csv")
# dfActions <- read.csv("dfSessions_allActions_2016-02-06_10-45-58.csv")
# dfActionTypes <- read.csv("dfSessions_actionTypes_2016-01-21_20-03-16.csv")

# full feature set
dfActionDetails <- read.csv("dfSessions_allActionDetails_2016-02-05_00-15-35.csv")
dfDeviceStats <- read.csv("dfSessions_allDeviceTypes_2016-02-06_00-02-53.csv")
dfActions <- read.csv("dfSessions_allActions_2016-02-05_09-04-24.csv")
dfActionTypes <- read.csv("dfSessions_allActionTypes_2016-02-05_09-15-17.csv")


############################################################################
## combine data sets

dfFeatures <- join(dfActionDetails, dfDeviceStats, by="user_id")
dfFeatures <- join(dfFeatures, dfActions, by="user_id")
dfFeatures <- join(dfFeatures, dfActionTypes, by="user_id")
dfFeatures <- join(dfFeatures, dfSessionStats, by="user_id")

rm(dfActionDetails, dfDeviceStats, dfActions, dfActionTypes, dfSessionStats)

# combine train and test data
df_all <- rbind(df_train_noLabels,df_test)
# combine session stats with all
df_all <- left_join(x = df_all, y = dfFeatures, by = c("id"="user_id"))

############################################################################
## clean up columns

# clean up session stats
statCols <- c("sum", "mean", "sd", "max", "min")
df_all[is.na(df_all$N), "N"] <- 0
df_all[df_all$N <= 1, statCols] <- 0
df_all[df_all$N == 2, "sd"] <- 0

# remove date_first_booking
df_all <- df_all[-c(which(colnames(df_all) %in% c('date_first_booking')))]

# replace missing values
# df_all[is.na(df_all)] <- -1

############################################################################
## engineer feature: difference between dates of timestamp_first_active and date_account_created
dateCreate <- ymd(df_all$date_account_created)
dateActive <- ymd_hms((df_all$timestamp_first_active))
dateDiff <- dateActive - dateCreate
df_all$timeDiff <- as.numeric(dateDiff, units = "secs")
# qplot(as.numeric(timeDiff, units = "secs"), data=df_all, geom="histogram")

# # why are there < 0 time differences
# timeoi <- dateDiff < 0
# dates <- data.frame(create=dateCreate, active=dateActive, diff=dateDiff)
# head(dates[timeoi,])
# dateCreate[timeoi]


############################################################################
# split date_account_created in year, month and day
dac <- as.data.frame(str_split_fixed(df_all$date_account_created, '-', 3))
df_all['dac_year'] <- as.numeric(as.character(dac[,1]))
df_all['dac_month'] <- as.numeric(as.character(dac[,2]))
df_all['dac_day'] <- as.numeric(as.character(dac[,3]))
df_all <- df_all[,-c(which(colnames(df_all) %in% c('date_account_created')))]

# split timestamp_first_active in year, month and day
#df_all[,'tfa_year'] = str_match(as.character(df_all[,'timestamp_first_active']), '^\\d{4}')[1]
df_all$tfa_year <- as.numeric(str_sub(as.character(df_all$timestamp_first_active), 1, 4))#3, 6)
df_all$tfa_month <- as.numeric(str_sub(as.character(df_all$timestamp_first_active), 5, 6))#7, 8)
df_all$tfa_day <- as.numeric(str_sub(as.character(df_all$timestamp_first_active), 7, 8))#9, 10)
df_all <- df_all[,-c(which(colnames(df_all) %in% c('timestamp_first_active')))]

############################################################################
## add holidays feature

# convert date cols to timestamp for Kaggle AirBNB data
tfas <- ymd(with(df_all, paste(tfa_year, tfa_month, tfa_day, sep="-")))
# find all the years of interest
uYears <- as.numeric(unique(df_all$tfa_year))
allHolidays <- getHolidays(uYears)
df_all$holidays <- as.numeric(tfas %in% allHolidays)

## clean Age by removing values
#df_all[df_all$age < 14 | df_all$age > 100,'age'] <- -1

# give all non-sense ages a "-1", then give them an "NA" to be imputed
df_all[is.na(df_all$age), 'age'] <- -1
df_all[df_all$age < 18 | df_all$age > 100,'age'] <- -1
df_all[df_all$age == -1,'age'] <- NA

## assign unknown genders an "NA" for imputation purposes
# df_all[df_all$gender == "-unknown-", "gender"] <- NA


############################################################################
## one-hot-encoding features
ohe_feats <- c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
dummies <- dummyVars(~ gender + signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)

############################################################################
## default imputation of missing values

# store data frame before imputation
# df_all_preImpute <- df_all_combined

# replace all NA with -1 (the default method)
df_all_defaultImpute <- df_all_combined
df_all_defaultImpute[is.na(df_all_defaultImpute)] <- -1 # give every NA a -1

if (flagImpute){
############################################################################
  ### knn imputation
  actCols <- grep("act", names(df_all_preImpute))
  devCols <- grep("dev", names(df_all_preImpute))
  otherNAcols <- grep("first", names(df_all_preImpute))
  df_knn_impute <- df_all_preImpute[1:275000,c(-1,-actCols, -devCols, -otherNAcols)]
  system.time(df_all_knn <- knnImputation(df_knn_impute, k = 2))
  
  
  ### mice imputation
  actCols <- grep("act", names(df_all_preImpute))
  devCols <- grep("dev", names(df_all_preImpute))
  otherNAcols <- grep("first", names(df_all_preImpute))
  
  
  idxSpacing <- 10000
  idxs <- seq(from=1, to=nrow(df_all_preImpute), by=idxSpacing)
  nRows <- nrow(df_all_preImpute)
  for (idx in idxs){
    # impute over 10000 point chunks
    # starting index
    start_idx <- idx
    end_idx <- idx + idxSpacing - 1
    
    # when we reach the end of the number of rows, stop
    if (end_idx > nRows){
      end_idx <- nRows
    }
    
    # impute for the segment
    df_mice_impute <- df_all_preImpute[start_idx:end_idx,c(-1,-actCols, -devCols, -otherNAcols)]
    df_all_mice <- tryCatch(
      mice(df_mice_impute, m=5, maxit=5, meth = 'pmm', seed = 0), 
      error = function(df_all_mice_impute){
        df_all_mice <- mice(df_mice_impute, m=5, maxit=5, meth = 'mean', seed = 0)
        return(df_all_mice)
      },
      finally = print(paste("done with", idx, sep=" "))
    )
    
    
    # extract the imputed values
    df_all_mice_complete <- complete(df_all_mice)
    
    if (idx == 1){
      df_mice_imputed <- df_all_mice_complete
    } else {
      df_mice_imputed <- rbind(df_mice_imputed, df_all_mice_complete)
    }
    
  }
  
  ## assign age data to imputed data frame
  df_mice <- df_all_preImpute
  df_mice$age <- round(df_mice_imputed$age)
  df_mice[is.na(df_mice)] <- -1
  
  ts <- timestamp()
  miceName <- paste("df_mice_", ts, ".csv",sep="")
  write.csv(df_mice, miceName, row.names = FALSE)
  
  #df_mice <- read.csv("df_mice_2016-02-01_08-05-09.csv")
  
  
  #df_mice_impute <- df_all_preImpute[,c(-1,-actCols, -devCols, -otherNAcols)]
  
  # system.time(df_all_mice <- mice(df_mice_impute, m=5, maxit=5, meth = 'pmm', seed = 0))
  # names(df_all_mice$imp$age)
  # df_all_mice_complete <- complete(df_all_mice)
  
  
  ### caret knn imputation
  system.time(knn_preProc <- preProcess(df_all_preImpute[, c(-1,-actCols, -devCols, -otherNAcols)], method='knnImpute', k=2))
  df_knn <- predict(knn_preProc, df_all_preImpute[, c(-1,-actCols, -devCols, -otherNAcols)])
  
  ###########################################################
  ### na.roughfix imputation
  # impute with crude method
  df_all_combined <- na.roughfix(df_all_preImpute[,-1]) #strip out IDs
  allID <- df_all_preImpute$id
  
  # recombine ids
  df_all_combined <- mutate(df_all_combined, id = allID)
  
  # move id column to beginning of data frame
  idCol <- which(names(df_all_combined) %in% 'id')
  df_all_combined <- df_all_combined[, c(idCol, (1:ncol(df_all_combined))[-idCol])]
  
  df_all_imputed <- df_all_combined
  
  # system.time(X_imputed2 <- mice(X[,2:30], m=5, maxit=50, meth='pmm', seed=500))
}
############################################################################
# remove unneeded data frames
rm(df_all_ohe, df_all_combined, df_all, dac, dfFeatures)


# split train and test
df_eval <- df_all_defaultImpute #df_mice
X <-  df_eval[df_eval$id %in% df_train$id,]
X_test <-  df_eval[df_eval$id %in% df_test$id,]
y <- recode(labels$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")

ts <- timestamp()

write.csv(X, paste("X_",ts,".csv",sep=""), row.names = FALSE)
write.csv(X_test, paste("X_test_",ts,".csv",sep=""), row.names = FALSE)
write.csv(y, paste("y_",ts,".csv",sep=""), row.names = FALSE)

##############################################################################
# ##PCA
# # first remove columns with 0 variance
# colVariances <- apply(X[,-1], 2, var)
# zeroVarCols <- names(colVariances)[colVariances==0]
# X2 <- X[,!(colnames(X_tr) %in% zeroVarCols)]
# X2 <- X2[,-1]
# X_test2 <- X_test[,!(colnames(X_tr) %in% zeroVarCols)]
# X_test2 <- X_test2[,-1]
# 
# # do PCA on training/test sets
# preProcValues <- preProcess(X2, method = c("center", "scale", "pca"))
# X_pca <- predict(preProcValues, X2)
# X_test_pca <- predict(preProcValues, X_test2)

##############################################################################
# ## remove low value attributes
# impMatrix <- read.csv('importance_matrix_2016-01-25_00-24-30.csv')
# top50names <- impMatrix[1:50,"Feature"]
# X_topAt <- X[,names(X) %in% top50names]
# X_test_topAt <- X_test[,names(X) %in% top50names]

# importance matrix derived from replacing NAs with -1
# impMat <- read.csv("importance_matrix_2016-02-01_23-10-20.csv")
# impMat <- read.csv("importance_matrix_2016-02-02_21-08-28.csv")
# # featsDF <- read.csv("topFeatures_02-02-16.csv")
# # featsDF <- read.csv("topFeatures_02-02-16_oneByOne.csv")
# featsDF <- read.csv("topFeatures_02-02-16_oneByOne_usingDateDiff.csv")
# feats <- as.character(featsDF$x)


##############################################################################
## load saved data
# X <- read.csv("X_lang.csv")
# X_test <- read.csv("X_test_lang.csv")
# y <- read.csv("y.csv")

# X <- read.csv("X_tr_ageBuckets_2016-02-09_21-35-57.csv")
# X_test <- read.csv("X_test_ageBuckets_2016-02-09_21-33-03.csv")
# y <- read.csv("y.csv")

X <- read.csv("X_2016-02-09_23-54-09.csv")
X_test <- read.csv("X_test_2016-02-09_23-54-09.csv")
y <- read.csv("y_2016-02-09_23-54-09.csv")
## convert y to data matrix
y <- data.matrix(y)

df_train <- read_csv("../data/train_users_2.csv")
labels2 <- df_train$country_destination
rm(df_train)

##############################################################################
# loop over several parameters
etas <- c(0.1) #c(0.05, 0.075, 0.1, 0.15, 0.2)#c(0.05,0.1,0.15,0.2,0.3,0.4)
max_depths <- c(6) #c(3,4,5,6,8,10) #c(5,6,7)#c(3,4,5,6,7,8,9)
nrounds <- c(120) #c(80, 100, 120, 150, 200)# c(50, 100, 150) #c(50,100,200,300)#c(25,50,100,200,1000)
subsamples <- c(0.7, 0.75)#c(0.6,0.7,0.8) #c(0.5,0.6,0.7,0.8) #c(0.3,0.5,0.7) #0.5#c(0.5,0.7,0.9,1)
colsample_bytrees <- c(0.5)#c(0.4, 0.5, 0.6) #c(0.3,0.4,0.5,0.6) #c(0.3,0.5,0.7) #0.5#c(0.5,0.7,0.9,1)
lambdas <- c(0) #c(0, 0.1, 0.3, 0.6, 1)
alphas <- c(0.9,0.75)
gammas <- c(2)
eval_metric <- "merror"#"ndcg"#"merror"
nAttribs <- -1# seq(from=105, to=85, by=-1)#c(160,80,60,40)#c(2,3,4,5,6,7,8,9,10,15,20,25,30,35,40,45,50)
objective <- "multi:softprob"#"multi:softprob"#"multi:softmax" #"multi:softprob"
trainFraction <- 0.8 # fraction of training set used for training

params <- data.frame(depth = numeric(0), nr = numeric(0), nDCG_mean = numeric(0))

# rm(bestFeats)

for (nAttrib in nAttribs){
  for (gamma in gammas){
    for (alpha in alphas){
      for (lambda in lambdas){
        for (colsample_bytree in colsample_bytrees){
          for (subsample in subsamples){
            for (eta in etas){
              for (nround in nrounds){
                for (max_depth in max_depths) {
                  
                  # split off the validation set
                  set.seed(1245)
                  
#                   yearOI <- X$dac_year %in% c(2009, 2010, 2011, 2012, 2013, 2014)
#                   X <- X[yearOI,]
#                   y <- y[yearOI]
                  
                  trainIndex <- createDataPartition(y, p = trainFraction, list = FALSE)
        #           X_tr <- X[trainIndex,] #X_topAt[trainIndex,] #X[trainIndex,]#X_pca[trainIndex,]
        #           X_val <- X[-trainIndex,] #X_topAt[-trainIndex,] #X[-trainIndex,]#X_pca[-trainIndex,]
        #           y_tr <- y[trainIndex]
        #           y_val <- y[-trainIndex]
        #           y_val_labels <- labels2[-trainIndex]
                  
                  valIndex <- 1:dim(X)[1]
                  valIndex <- valIndex[-trainIndex]
                  #valSample <- sample(valIndex, dim(trainIndex)[1]/4)
                  valSample <- valIndex
                  
                  
                  
                  # select top features in an iterative way
#                   if (nAttrib == nAttribs[1]){
#                     importance_matrix <- impMat
#                   } else {
#                     importance_matrix <- xgb.importance(colnames(X_tr[,-1]), model = xgb)
#                   }
#                   
#                   feats <- c("id",as.character(importance_matrix$Feature[1:(nAttrib)]))
#                   if (!exists("bestFeats")){
#                     bestFeats <- data.frame(order=0:nAttrib, iter=nAttrib, feats=feats)
#                   } else{
#                     featDF <- data.frame(order=0:nAttrib, iter=nAttrib, feats=feats)
#                     bestFeats <- rbind(bestFeats, featDF)
#                     write.csv(bestFeats, "bestFeatures_intermediate.csv", row.names=F)
#                   }
                  
                  
                  
                  
                  X_tr <- X[trainIndex, ] #, names(X) %in% feats  names(X) %in% feats
                  X_val <- X[valSample, ] #X[-trainIndex,] #names(X) %in% feats
                  y_tr <- y[trainIndex]
                  y_val <- y[valSample] #y[-trainIndex]
                  y_val_labels <- labels2[valSample] #labels2[-trainIndex]
                  
                  #train xgboost
                  xgb <- xgboost(data = data.matrix(X_tr[,-1]), #data.matrix(X_tr), #data.matrix(X_tr[,-1]), 
                                 label = y_tr, 
                                 eta = eta,
                                 max_depth = max_depth, #originally 9
                                 nround = nround, #originally 25
                                 subsample = subsample,#0.5,
                                 colsample_bytree = colsample_bytree, #0.5
                                 seed = 1,
                                 eval_metric = eval_metric, #ndcg5, #eval_metric,#ndcg5,#"mlogloss",#"merror",
                                 objective = objective,
                                 lambda = lambda,
                                 alpha = alpha,
                                 gamma = gamma,
                                 num_class = 12,
                                 nthread = 3
                  )
                  # do cross-validation
#                   cv <- xgb.cv(data = data.matrix(X_tr[,-1]),#data.matrix(X_tr[,-1]), 
#                                  label = y_tr, 
#                                  eta = eta,
#                                  max_depth = max_depth, #originally 9
#                                  nround = nround, #originally 25
#                                  subsample = subsample,#0.5,
#                                  colsample_bytree = colsample_bytree, #0.5
#                                  #seed = 1,
#                                  eval_metric = ndcg5,#ndcg5,#"mlogloss",#"merror",
#                                  objective = objective,
#                                lambda = lambda,
#                                alpha = alpha,
#                                gamma = gamma,
#                                  num_class = 12,
#                                  nthread = 3,
#                                 prediction = TRUE,
#                                 nfold = 5
#                   )
#                   ts <- timestamp()
#                   write.csv(data.frame(cv$dt), paste("dt_",ts,'.csv',sep=""))
                  
                  ########################################################################
                  # predict values in validation set
                  if (trainFraction < 1){
                    y_val_pred <- predict(xgb, data.matrix(X_val[,-1])) #data.matrix(X_val))#data.matrix(X_val[,-1]))
                    
                    # extract the 5 classes with highest probabilities
                    predictions <- as.data.frame(matrix(y_val_pred, nrow=12))
                    rownames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
                    predictions_top5 <- as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))
                    
                    # reshape predictions to n x 5
                    pVal <- matrix(predictions_top5, ncol = 5, byrow = TRUE)
                    pVal <- data.frame(pVal)
                    names(pVal) <- c("C1", "C2", "C3", "C4", "C5")
                    pVal$country_destination <- y_val_labels
                    evals <- (apply(pVal, 1, nDCG))
                    nDCGmean <- mean(evals)
                    print(paste("mean DCG: ", nDCGmean))
                    
                    # save parameters used in learning
                    paramDF <- data.frame(trainFraction = trainFraction,
                                          eta = eta, 
                                          depth = max_depth, 
                                          nr = nround, 
                                          subsample = subsample,
                                          colsample_bytree = colsample_bytree,
                                          eval_metric = eval_metric,
                                          objective = objective,
                                          lambda = lambda,
                                          alpha = alpha,
                                          gamma = gamma,
                                          numTopAttribs = nAttrib,
                                          nDCG_mean = nDCGmean)
                    params <- rbind(params, paramDF)
                    paramSaveName <- paste("parametersXGBoost_intermediate",".csv", sep="")
                    write.csv(params, paramSaveName, row.names = FALSE)
                  }
                  
                  
                }
              }
            }
          }
        }
      }
    }
  }
}

if (trainFraction < 1){
  ts <- timestamp()
  
  paramSaveName <- paste("parametersXGBoost_",ts, ".csv", sep="")
  write.csv(params, paramSaveName, row.names = FALSE)
}

# write.csv(bestFeats, paste("bestFeatures_",ts,".csv"), row.names=F)
########################################################################
## generate submission file
if (trainFraction == 1){
  # predict values in test set
  y_pred <- predict(xgb, data.matrix(X_test[,-1]))
  
  # extract the 5 classes with highest probabilities
  predictions <- as.data.frame(matrix(y_pred, nrow=12))
  rownames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
  predictions_top5 <- as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))
  
  # create submission 
  idx = X_test$id 
  id_mtx <- matrix(idx, 1)[rep(1,5), ] 
  ids <- c(id_mtx)
  submission <- NULL
  submission$id <- ids
  submission$country <- predictions_top5
  
  # generate submission file
  submission <- as.data.frame(submission)
  ts <- timestamp()
  submissionName <- paste("submission_", ts, ".csv", sep="")
  write.csv(submission, submissionName, quote=FALSE, row.names = FALSE)
}