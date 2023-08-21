modelPredictions <- function(test, models){
  
  test <- test[,-1]
  
  ypred.lm <- predict(models$LinearModel, newdata = test)
  
  ypred.lasso <- predict(models$LassoModel, newdata = test)
  
  ypred.pls <- predict(models$PLSModel, newdata = test)
  
  ypred.xgb <- predict(models$XGBModel, newdata = test)
  
  outlist <- list(
    Linear = ypred.lm,
    PLS = ypred.pls,
    Lasso = ypred.lasso,
    XGB = ypred.xgb
  )
  outlist
}
