cv.betalasso <- function(X, y, nfolds, nlambda = 100, ...) {
  
  # sequence of penalization values
  S <-seq(0.0001,0.1, length.out = nlambda)
  
  K <- nfolds
  n <- dim(X)[1]; p <- dim(X)[2] #number of observations and number of predictors
  id <- sample(1:K, n, replace=TRUE, prob=rep(1/K,K))
  
  results <- array(NA, dim=c(nlambda,K))
  for (s in S){
    for(i in 1:K){
      ytrain <- y[id!=i]
      ytest <- y[id==i]
      Xtrain <- X[id!=i,]
      Xtest <- X[id==i,]
      
      # fit
      fit <- penalizedbeta::betareg_lasso(Xtrain,ytrain,lambda = s)
      
      # predict
      #y_pred_train <- predict(fit, newdata = Xtrain)
      y_pred_test <- predict(fit, newdata = Xtest)

      
      MSE_test <- mean(c(ytest-y_pred_test)^2, na.rm=TRUE)
      
      
      # beta.fit <- lm("ytrain~.",data=data.frame(ytrain,Pr_train), na.action="na.exclude")
      # Predictions in test
      #  y_pred_test <- predict.lm(lm.fit, newdata = data)
      #  MSE_test <- mean(c(ytest-y_pred_test)^2, na.rm=TRUE)
      
      results[s, i] <- MSE_test
    }
  }
  min.MSE <- which.min(apply(results, MARGIN = 1, FUN = mean))
  return(list(s.min = min.MSE))
}