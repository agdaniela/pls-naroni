KfoldCV.RandomSearch <- function(train, nfolds){
  fitControl <- caret::trainControl(method = "cv", number = nfolds)
  
  ### Linear model
  lm.fit <- caret::train(ytrain ~ ., data = train, 
                             method = "lm", 
                             trControl = fitControl)
  
  ### LASSO
  
  lasso.fit <- caret::train(ytrain ~ ., data = train, 
                     method = "glmnet", 
                     trControl = fitControl)
  
  ### Partial Least Squares Regression
  
  n_components <- expand.grid(ncomp = c(1:15))
  
  pls.fit <- caret::train(ytrain ~ ., data = train, 
                   method = "pls", 
                   trControl = fitControl, tuneGrid = n_components)
  
  ### XGBoost
  
  xgb.fit <- caret::train(ytrain~., data = train,
                          method = "xgbTree", 
                          trControl = fitControl)
  
  outlist <- list(
    LinearModel = lm.fit,
    LassoModel = lasso.fit,
    PLSModel = pls.fit,
    XGBModel = xgb.fit
  )
  return(outlist)
}
