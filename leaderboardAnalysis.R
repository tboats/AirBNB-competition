## compute leaderboard histogram
file_path <- readClipboard()
file_path <- file.path("L:\\Ideas\\kaggle_AirBNB\\scriptsR\\leaderboard", "airbnb-recruiting-new-user-bookings_public_leaderboard.csv")
df_leaderboard <- read.csv(file_path)

names(df_leaderboard) <- c("TeamId", "TeamName", "SubmissionDate", "Score")

library(plyr)
library(dplyr)
library(ggplot2)

# get the maximum score by each user
df_agg_leader <- ddply(df_leaderboard, .(TeamId, TeamName), summarize, maxScore = max(Score))

# filter out the very low scores
df_agg_leader_sel <- filter(df_agg_leader, maxScore > 0.7)

# plot a histogram of the scores
myScore <- filter(df_agg_leader_sel, TeamName == "Tom Boatwright")$maxScore
q <- ggplot(data = df_agg_leader_sel, aes(x = maxScore))
q + geom_histogram(binwidth = 0.001) + scale_y_sqrt() + geom_vline(xintercept = myScore)
