## k-fold CV for selecting LASSO and PLS hyperparameter
source("pls.R")

kfoldCV.selection <- function(Xtrain, ytrain, nfolds, max.d) {
  
  Xtrain <- as.matrix(Xtrain)
  
  lasso.fit <- glmnet::cv.glmnet(Xtrain, ytrain, type.measure = "mse", nfolds = nfolds)
  
  pls.fit <- cv.pls(Xtrain, ytrain, nfolds, max.d)
  
  train_control = caret::trainControl(method = "cv", number = nfolds)
  gbmGrid <-  expand.grid(max_depth = c(3, 5, 7), 
                          nrounds = (1:10)*20,    # number of trees
                          # default values below
                          eta = 0.3,
                          gamma = c(0,0.1,0.01,0.001),
                          subsample = 1,
                          min_child_weight = 1,
                          colsample_bytree = 0.6)
  xgb.fit = train(ytrain~., data = cbind(ytrain,Xtrain), method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid)
  
  
  return(list(lambda.min = lasso.fit$lambda.min, d.min = pls.fit$d.min, xgb.model = xgb.fit))
}


