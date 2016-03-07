
###########################################################################
## run cross validation on data sets
# set up the cross-validated hyper-parameter search
xgb_grid_1  <- expand.grid(
  eta = c(0.03, 0.1, 0.3),
  max_depth = c(5,6,7),#c(6, 7, 8),
  nrounds = c(50,80,100,120,150)
  #   subsample = 0.5 #c(0.5, 0.7, 1),
  #   colsample_bytree = 0.5,#c(0.5, 0.7, 1),
  #   lambda = 0, #c(0, 1),
  #   alpha = 1, #c(0, 1),
  #   gamma = 0 #c(0, 1)
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
trainFraction <- 0.5
trainIndex <- createDataPartition(y, p = trainFraction, list = FALSE)
y1 <- y[trainIndex]
X1 <- X[trainIndex,]


# train the model for each parameter combination in the grid, 
#   using CV to evaluate
cv <- train(
  x = as.matrix(X1 %>% select(-id)),
  y = as.matrix(y1),
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

# str(cv$results)