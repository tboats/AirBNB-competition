df_a <- read.csv("parametersXGBoost_2016-02-10_18-58-36.csv")
df_b <- read.csv("parametersXGBoost_2016-02-10_03-23-13.csv")
df_c <- read.csv("parametersXGBoost_2016-02-10_20-08-19.csv")
df_d <- read.csv("parametersXGBoost_2016-02-10_21-33-54.csv")
df_e <- read.csv("parametersXGBoost_2016-02-10_22-21-40.csv")
df_f <- read.csv("parametersXGBoost_2016-02-10_22-48-44.csv")
df_g <- read.csv("parametersXGBoost_2016-02-10_22-48-44.csv")
df_h <- read.csv("parametersXGBoost_2016-02-11_07-47-01.csv")
df <- rbind(df_a, df_b, df_c, df_d, df_e, df_f, df_g, df_h)
df$colsample_bytree <- as.factor(df$colsample_bytree)

library(ggplot2)
q <- ggplot(data=df, aes(x=jitter(alpha), y=gamma, size=nDCG_mean+1, col=colsample_bytree))
q + geom_point(alpha=0.5)

df1 <- filter(df, colsample_bytree == 0.5)
q <- ggplot(data=df1, aes(x=alpha, y=nDCG_mean, col=as.factor(gamma)))
q + geom_point(size=5, alpha=0.5)

df1 <- filter(df1, nDCG_mean == max(nDCG_mean))
df1

df2 <- filter(df, gamma==2, subsample==0.7, lambda==0)
q <- ggplot(data=df2, aes(x=alpha, y=nDCG_mean))
q + geom_point(size=5, alpha=0.5)
