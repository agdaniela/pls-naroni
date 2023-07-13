cv.pls <- function(X, y, nfolds, max.directions) {
  K <- nfolds
  D <- max.directions
  n <- dim(X)[1]; p <- dim(X)[2] #number of observations and number of predictors
  id <- sample(1:K, n, replace=TRUE, prob=rep(1/K,K))
  
  results <- array(NA, dim=c(D,K))
  for (d in 1:D){
    for(i in 1:K){
      ytrain <- y[id!=i]
      ytest <- y[id==i]
      Xtrain <- X[id!=i,]
      Xtest <- X[id==i,]

      W <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d, scale = FALSE)$W
    
      # Projection in train set
      Pr_train <- (Xtrain )%*%W
      # Projection in test set
      Pr_test <- (Xtest )%*%W
    
      # Fit linear model
      lm.fit <- lm("ytrain~.",data=data.frame(ytrain,Pr_train), na.action="na.exclude")
      # Predictions in test
      data <- data.frame(ytest,Pr_test); names(data) <- names(data.frame(ytrain,Pr_train))
      y_pred_test <- predict.lm(lm.fit, newdata = data)
      MSE_test <- mean(c(ytest-y_pred_test)^2, na.rm=TRUE)
    
      results[d, i] <- MSE_test
    }
  }
  min.MSE <- which.min(apply(results, MARGIN = 1, FUN = mean))
  return(list(d.min = min.MSE))
}


