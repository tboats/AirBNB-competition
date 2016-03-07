## Goal: explore df_sessions

library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)

## load data
if (!exists("dfSessions")){
  dfSessions <- read.csv("../data/sessions.csv")
}

df_train <- read.csv("../data/train_users_2.csv")

######################################################################################################
## How does the "contact_host" action relate to bookings?

# filter users who did book
df_book <- filter(df_train, !(country_destination %in% c("NDF")))
users_book <- df_book$id

# filter users who did not book
df_NDF <- filter(df_train, (country_destination %in% c("NDF")))
users_NDF <- df_NDF$id

users_all <- c(as.character(users_book), as.character(users_NDF))


######################################################################################################
## filter out "contact_host" in action_detail
df_sessions_train <- filter(dfSessions, user_id %in% users_all)
df_sessions_train$travel <- 1
df_sessions_train[df_sessions_train$user_id %in% users_NDF, "travel"] <- 0

######################################################################################################
## look at a single user who went to DE
df_book_de <- filter(df_book, country_destination %in% "DE")
df_de <- filter(df_sessions_train, user_id %in% df_book_de$id)
df_de[df_de$user_id %in% df_de$user_id[1],]

######################################################################################################
## aggregate data to see what is more common
# df_sessions_contact <- filter(df_sessions_train, action_detail %in% c("contact_host"))

agg1 <- ddply(df_sessions_train, .(travel), summarize,
              countContactHost = table(action_detail)['contact_host']/length(action_detail),
              countBookIt = table(action_detail)['book_it']/length(action_detail),
              countBooking = table(action_detail)['booking']/length(action_detail),
              countInstantBook = table(action_detail)['instant_book']/length(action_detail),
              countRequestBooking = table(action_detail)['request_to_book']/length(action_detail),
              countChangeDates = table(action_detail)['change_contact_host_dates']/length(action_detail),
              countCoupon = table(action_detail)['apply_coupon']/length(action_detail),
              
              countChngChar = table(action_detail)['change_trip_characteristics']/length(action_detail),
              countYourTrips = table(action_detail)['your_trips']/length(action_detail),
              countSocialConn = table(action_detail)['user_social_connections']/length(action_detail),
              countCompleteBooking = table(action_detail)['complete_booking']/length(action_detail)
              )
agg1

# agg1m <- melt(agg1, id = c("travel"))
# agg1m$travel <- factor(agg1m$travel)
# q <- ggplot(data = agg1m, aes(x = variable, y = value, fill = travel))
# q + geom_bar(stat = "identity") + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   scale_y_sqrt()


agg2 <- ddply(df_sessions_train, .(travel), summarize,
              countContactHost = table(action_detail)['contact_host'],#/length(action_detail),
              countBookIt = table(action_detail)['book_it'],#/length(action_detail),
              countBooking = table(action_detail)['booking'],#/length(action_detail),
              countInstantBook = table(action_detail)['instant_book'],#/length(action_detail),
              countRequestBooking = table(action_detail)['request_to_book'],#/length(action_detail),
              countChangeDates = table(action_detail)['change_contact_host_dates'],#/length(action_detail),
              countCoupon = table(action_detail)['apply_coupon'],#/length(action_detail),
              
              countChngChar = table(action_detail)['change_trip_characteristics'],#/length(action_detail),
              countYourTrips = table(action_detail)['your_trips'],#/length(action_detail),
              countSocialConn = table(action_detail)['user_social_connections'],#/length(action_detail),
              countCompleteBooking = table(action_detail)['complete_booking']#/length(action_detail)
)
agg2



######################################################################################################
## make a bar graph of data summary

agg1m <- melt(agg1, id = "travel" )
agg1m$travel <- as.factor(agg1m$travel)
q <- ggplot(data= agg1m, aes(variable, fill = travel))
q + geom_bar(position = "dodge")


## plot frequency of each action by action detail
df_sessions_train$travel <- factor(df_sessions_train$travel, labels = c("no travel", "travel"))
q <- ggplot(data = df_sessions_train, aes(action_detail, fill = travel))
q + geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## plot frequency by action detail (only high frequency ones)
act_detail_freq <- table(df_sessions_train$action_detail)
ix_detail_freq <- order(act_detail_freq, decreasing = TRUE)
act_detail_freq <- act_detail_freq[ix_detail_freq]
act_detail_sel <- act_detail_freq[1:30]

# remove columns that are unclear
act_detail_sel <- act_detail_sel[!(names(act_detail_sel) %in% c("", "p3", "-unknown-"))]

df_act_det_sel <- filter(df_sessions_train, action_detail %in% names(act_detail_sel))
df_act_det_sel$action_detail <- factor(df_act_det_sel$action_detail, levels = names(act_detail_sel))
# df_act_det_sel$travel <- factor(df_act_det_sel$travel, levels = c("no travel", "travel"))
q <- ggplot(data = df_act_det_sel, aes(action_detail, fill = travel))
q + geom_bar(position = "dodge", width = 0.5) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(legend.title=element_blank()) +
  scale_y_sqrt(labels = comma) + 
  scale_fill_brewer(palette = "Set2")




uActionDetails <- unique(df_sessions_train$action_detail)
grep('book', uActionDetails, ignore.case = TRUE, value = TRUE)



######################################################################################################