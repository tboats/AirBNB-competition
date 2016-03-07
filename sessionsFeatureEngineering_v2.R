#####################################################################
## Goal: engineer more features from the sessions file
## AirBNB kaggle project
#####################################################################
library(plyr)
library(dplyr)
library(ggplot2)
library(caret)
library(reshape2)
source('L:/Ideas/kaggle_AirBNB/scriptsR/DCG.R')
source('E:/Dropbox/R/general/timestamp.R')

## load data
if (!exists("dfSessions")){
  dfSessions <- read.csv("../data/sessions.csv")
}

## create features based on seconds elapsed
if (!exists("dfSessions_byUser")){
  dfSessions_byUser <- ddply(dfSessions, c("user_id"), summarise, 
                             sum = sum(secs_elapsed, na.rm = TRUE), 
                             mean = mean(secs_elapsed, na.rm = TRUE), 
                             sd = sd(secs_elapsed, na.rm = TRUE), 
                             N = length(secs_elapsed),
                             max = max(secs_elapsed, na.rm = TRUE),
                             min = min(secs_elapsed, na.rm = TRUE)
  )
}
write.csv(dfSessions_byUser, 'dfSessions_byUser.csv', row.names = FALSE)

############################################################################

## one-hot-encoding features
ohe_feats <- c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
dummies <- dummyVars(~ gender + signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)

############################################################################
### create features by device type
## find top devices
topN <- 6
deviceTypeFreq <- table(dfSessions$device_type)
deviceTypeFreq <- sort(deviceTypeFreq, decreasing = TRUE)
topDevices <- names(deviceTypeFreq)[1:topN]

#dfSessions1 <- dfSessions[1:10000,]

ptm <- proc.time()
# if (!exists("dfSessions_byUser")){
dfSessions_device <- ddply(dfSessions, c("user_id"), summarise, 
                             dev1 = sum(device_type %in% topDevices[1]),
                             dev2 = sum(device_type %in% topDevices[2]),
                             dev3 = sum(device_type %in% topDevices[3]),
                             dev4 = sum(device_type %in% topDevices[4]),
                             dev5 = sum(device_type %in% topDevices[5]),
                             dev6 = sum(device_type %in% topDevices[6])
                             #dev1 = length(device_type %in% topDevices[1])
)
# }
proc.time()-ptm
write.csv(dfSessions_device, 'dfSessions_deviceStats.csv', row.names = FALSE)


############################################################################
### create features by "action detail"
## find top devices
topN <- 7
actionDetailFreq <- table(dfSessions$action_detail)
actionDetailFreq <- sort(actionDetailFreq, decreasing = TRUE)
topActionDetais <- names(actionDetailFreq)[1:topN]

#dfSessions1 <- dfSessions[1:10000,]

ptm <- proc.time()
# if (!exists("dfSessions_byUser")){
dfSessions_actionDetails <- ddply(dfSessions, c("user_id"), summarise, 
                           act1 = sum(action_detail %in% topActionDetais[1]),
                           act2 = sum(action_detail %in% topActionDetais[2]),
                           act3 = sum(action_detail %in% topActionDetais[3]),
                           act4 = sum(action_detail %in% topActionDetais[4]),
                           act5 = sum(action_detail %in% topActionDetais[5]),
                           act6 = sum(action_detail %in% topActionDetais[6]),
                           act7 = sum(action_detail %in% topActionDetais[7])
                           #dev1 = length(device_type %in% topDevices[1])
)
# }
proc.time()-ptm
write.csv(dfSessions_actionDetails, 'dfSessions_actionDetails.csv', row.names = FALSE)


############################################################################
### create features by action
## find top actions
topN <- 6
actionFreq <- table(dfSessions$action)
actionFreq <- sort(actionFreq, decreasing = TRUE)
topActions <- names(actionFreq)[1:topN]

#dfSessions1 <- dfSessions[1:10000,]

ptm <- proc.time()
# if (!exists("dfSessions_byUser")){
dfSessions_action <- ddply(dfSessions, c("user_id"), summarise, 
                           action1 = sum(action %in% topActions[1]),
                           action2 = sum(action %in% topActions[2]),
                           action3 = sum(action %in% topActions[3]),
                           action4 = sum(action %in% topActions[4]),
                           action5 = sum(action %in% topActions[5]),
                           action6 = sum(action %in% topActions[6])
                           #dev1 = length(device_type %in% topDevices[1])
)
# }
#names(dfSessions_action) <- c('user_id','action1','action2','action3','action4','action5','action6')
proc.time()-ptm
ts <- timestamp()
saveName <- paste('dfSessions_actions_', ts, '.csv', sep="")
write.csv(dfSessions_action, saveName, row.names = FALSE)

############################################################################
### create features by action type
## find top actions
topN <- 6
actionTypeFreq <- table(dfSessions$action_type)
actionTypeFreq <- sort(actionTypeFreq, decreasing = TRUE)
topActionTypes <- names(actionTypeFreq)[1:topN]

#dfSessions1 <- dfSessions[1:10000,]

ptm <- proc.time()
# if (!exists("dfSessions_byUser")){
dfSessions_actionType <- ddply(dfSessions, c("user_id"), summarise, 
                           actType1 = sum(action_type %in% topActionTypes[1]),
                           actType2 = sum(action_type %in% topActionTypes[2]),
                           actType3 = sum(action_type %in% topActionTypes[3]),
                           actType4 = sum(action_type %in% topActionTypes[4]),
                           actType5 = sum(action_type %in% topActionTypes[5]),
                           actType6 = sum(action_type %in% topActionTypes[6])
                           #dev1 = length(device_type %in% topDevices[1])
)
# }
#names(dfSessions_actionType) <- c('user_id','actType1','actType2','actType3','actType4','actType5','actType6')
proc.time()-ptm
ts <- timestamp()
saveName <- paste('dfSessions_actionTypes_', ts, '.csv', sep="")
write.csv(dfSessions_actionType, saveName, row.names = FALSE)




############################################################################
### create features by all "action detail"

actionDetailFreq <- table(dfSessions$action_detail)
actionDetailFreq <- sort(actionDetailFreq, decreasing = TRUE)
actionDetailNamesOI <- names(actionDetailFreq)[actionDetailFreq > 10]

## one-hot-encoding features
#ohe_feats <- c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
ohe_feats <- actionDetailNamesOI


## must loop over the dfSessions data set since it is so big


df_sessions_detail <- select(dfSessions, action_detail, user_id)
df_sessions_detail <- filter(df_sessions_detail, action_detail %in% actionDetailNamesOI)
# levels(df_sessions_detail$action_detail)[1] <- "blank"
# df_st_detail1 <- df_st_detail[1:100000,]

# Start the clock!
ptm <- proc.time()

# these steps count the number of each action_detail for each user
mdf <- melt(df_sessions_detail, id = "user_id")
df_actDet0 <- ddply(mdf, .(user_id, value), summarize, nVals = length(value))
df_actDet <- dcast(df_actDet0, user_id~value, value.var = "nVals")
df_actDet[is.na(df_actDet)] <- 0

# Stop the clock
proc.time() - ptm

## write to disk
ts <- timestamp()
saveName <- paste('dfSessions_allActionDetails_', ts, '.csv', sep="")
write.csv(df_actDet, saveName, row.names=FALSE)


## remove low frequency columns
df_actDet <- read.csv("dfSessions_allActionDetails_2016-02-05_00-15-35.csv")
df_actDet1 <- df_actDet %>% select(-user_id)
actDet_sum <- sapply(df_actDet1, sum)
# actDet_sum[order(actDet_sum, decreasing=TRUE)]
notFullCol <- names(actDet_sum)[actDet_sum < 10]
df_actDet <- df_actDet[, !names(df_actDet) %in% notFullCol]

ts <- timestamp()
saveName <- paste('dfSessions_allActionDetails_', ts, '.csv', sep="")
write.csv(df_actDet, saveName, row.names=FALSE)


##################################
## OHE approach failed due to memory and incompatible strategy
# dummies <- dummyVars(user_id ~ ., data = df_sessions_detail)
# df_all_ohe <- as.data.frame(predict(dummies, newdata = df_sessions_detail))
# df_all_combined <- cbind(df_sessions_detail[,-c(which(colnames(df_sessions_detail) %in% ohe_feats))],df_all_ohe)

############################################################################
### create features by all "action"

actionFreq <- table(dfSessions$action)
actionFreq <- sort(actionFreq, decreasing = TRUE)
actionNamesOI <- names(actionFreq)[actionFreq > 10]

df_sessions_action <- select(dfSessions, action, user_id)
df_sessions_action <- filter(df_sessions_action, action %in% actionNamesOI)

# Start the clock!
ptm <- proc.time()

# these steps count the number of each action for each user
mdf <- melt(df_sessions_action, id = "user_id")
df_act0 <- ddply(mdf, .(user_id, value), summarize, nVals = length(value))
df_act <- dcast(df_act0, user_id~value, value.var = "nVals")
df_act[is.na(df_act)] <- 0

# Stop the clock
proc.time() - ptm

## write to disk
ts <- timestamp()
saveName <- paste('dfSessions_allActions_', ts, '.csv', sep="")
write.csv(df_act, saveName, row.names=FALSE)

#########################################
## refine the feature set
df_act <- read.csv("dfSessions_allActions_2016-02-05_09-04-24.csv")
df_act_lowImp <- read.csv("actionFeaturesWithLowImportance.csv")
df_act <- df_act[,!(names(df_act) %in% df_act_lowImp$feat)]

## save to disk
ts <- timestamp()
saveName <- paste('dfSessions_allActions_', ts, '.csv', sep="")
write.csv(df_act, saveName, row.names = F)
########################################

# df_act2 <- (df_act %>% select(-user_id))

# noImpSum <- sapply(df_act2, sum)
# noImpSum[order(noImpSum,decreasing = TRUE)]

############################################################################
### create features by all "action_type"

actionTypeFreq <- table(dfSessions$action_type)
actionTypeFreq <- sort(actionTypeFreq, decreasing = TRUE)
actionTypeNamesOI <- names(actionTypeFreq)[actionTypeFreq > 10]

df_sessions_actionType <- select(dfSessions, action_type, user_id)
df_sessions_actionType <- filter(df_sessions_actionType, action_type %in% actionTypeNamesOI)

# Start the clock!
ptm <- proc.time()

# these steps count the number of each action for each user
mdf <- melt(df_sessions_actionType, id = "user_id")
df_actType0 <- ddply(mdf, .(user_id, value), summarize, nVals = length(value))
df_actType <- dcast(df_actType0, user_id~value, value.var = "nVals")
df_actType[is.na(df_actType)] <- 0

# Stop the clock
proc.time() - ptm

## write to disk
ts <- timestamp()
saveName <- paste('dfSessions_allActionTypes_', ts, '.csv', sep="")
write.csv(df_actType, saveName, row.names=FALSE)

############################################################################
### create all features by device type

# topN <- 6
# deviceTypeFreq <- table(dfSessions$device_type)
# deviceTypeFreq <- sort(deviceTypeFreq, decreasing = TRUE)
# topDevices <- names(deviceTypeFreq)[1:topN]


deviceTypeFreq <- table(dfSessions$device_type)
deviceTypeFreq <- sort(deviceTypeFreq, decreasing = TRUE)
deviceTypeNamesOI <- names(deviceTypeFreq)[deviceTypeFreq > 10]

df_sessions_deviceType <- select(dfSessions, device_type, user_id)
df_sessions_deviceType <- filter(df_sessions_deviceType, device_type %in% deviceTypeNamesOI)

# Start the clock!
ptm <- proc.time()

# these steps count the number of each action for each user
mdf <- melt(df_sessions_deviceType, id = "user_id")
df_devType0 <- ddply(mdf, .(user_id, value), summarize, nVals = length(value))
df_devType <- dcast(df_devType0, user_id~value, value.var = "nVals")
df_devType[is.na(df_devType)] <- 0

# Stop the clock
proc.time() - ptm

## write to disk
ts <- timestamp()
saveName <- paste('dfSessions_allDeviceTypes_', ts, '.csv', sep="")
write.csv(df_devType, saveName, row.names=FALSE)