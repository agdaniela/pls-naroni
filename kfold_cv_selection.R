## k-fold CV for selecting LASSO and PLS hyperparameter
source("pls.R")

kfoldCV.selection <- function(Xtrain, ytrain, nfolds, max.d) {
  
  Xtrain <- as.matrix(Xtrain)
  
  lasso.fit <- glmnet::cv.glmnet(Xtrain, ytrain, type.measure = "mse", nfolds = nfolds)
  
  pls.fit <- cv.pls(Xtrain, ytrain, nfolds, max.d)
  
  return(list(lambda.min = lasso.fit$lambda.min, d.min = pls.fit$d.min))
}

