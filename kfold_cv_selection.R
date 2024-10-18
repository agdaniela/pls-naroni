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


kfoldCV.selection2 <- function(Xtrain, ytrain, nfolds, max.d) {
  
  Xtrain <- as.matrix(Xtrain)
  
  #lasso.fit <- glmnet::cv.glmnet(Xtrain, ytrain, type.measure = "mse", nfolds = nfolds)
  #search the alpha
  # alpha <- seq(0.01, 0.99, 0.04)
  # best <- list(a=NULL, mse=NULL)
  # 
  # for (i in 1:length(alpha)) 
  # {
  #   cvg <- glmnet::cv.glmnet(x = Xtrain, y=ytrain, family = "gaussian", alpha = alpha[i])
  #   best$a <- c(best$a, alpha[i])
  #   best$mse <- c(best$mse, min(cvg$cvm))
  # }
  # 
  # index <- which(best$mse==min(best$mse))
  # best.alpha <- best$a[index]
  #best_mse <- best$mse[index]
  #cat("alpha:", best_alpha, " mse:", best_mse)
  
  #cross-validation again with the best alpha to get the lambda (shrinkage level).
  
  elastic_cv <- glmnet::cv.glmnet(x = Xtrain, y = ytrain, family = "gaussian", alpha = best.alpha)
  #best_lambda <- elastic_cv$lambda.min 
  
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
  
  
  return(list(best.alpha =  best.alpha,  best.lambda = elastic_cv$lambda.min , d.min = pls.fit$d.min, xgb.model = xgb.fit))
}

kfoldCV.elastic <- function(Xtrain, ytrain, nfolds) {
  
  Xtrain <- as.matrix(Xtrain)
  
  #search the alpha
  alpha <- seq(0.01, 0.99, 0.04)
  best <- list(a=NULL, mse=NULL)
  
  for (i in 1:length(alpha)) 
  {
    cvg <- glmnet::cv.glmnet(x = Xtrain, y=ytrain, nfolds = nfolds, family = "gaussian", alpha = alpha[i])
    best$a <- c(best$a, alpha[i])
    best$mse <- c(best$mse, min(cvg$cvm))
  }
  
  index <- which(best$mse==min(best$mse))
  best.alpha <- best$a[index]
     
  elastic_cv <- glmnet::cv.glmnet(x = Xtrain, y = ytrain, family = "gaussian", alpha = best.alpha)
   
  
  return(list(best.alpha =  best.alpha,  best.lambda = elastic_cv$lambda.min ))
}

kfoldCV.pls <- function(Xtrain, ytrain, nfolds, max.d) {
  
  Xtrain <- as.matrix(Xtrain)
  
  pls.fit <- cv.pls(Xtrain, ytrain, nfolds, max.d)
  
  return(list( d.min = pls.fit$d.min))
}

kfoldCV.plsbeta <- function(Xtrain, ytrain, nfolds, max.d) {
  
  Xtrain <- as.matrix(Xtrain)
  
  pls.fit <- cv.pls2(Xtrain, ytrain, nfolds, max.d)
  
  return(list( d.min = pls.fit$d.min))
}

kfoldCV.betalasso <- function(Xtrain, ytrain, nfolds) {
  
  Xtrain <- as.matrix(Xtrain)
  
  betalasso.fit <- cv.betalasso(Xtrain, ytrain, nfolds)
  
  return(list( s.min = betalasso.fit$s.min))
}

kfoldCV.xgboost <- function(Xtrain, ytrain, nfolds) {
  
  Xtrain <- as.matrix(Xtrain)
  
  train_control = caret::trainControl(method = "cv", number = nfolds)
  gbmGrid <-  expand.grid(max_depth = c(3, 5, 7), 
                          nrounds = (1:10)*20,    # number of trees
                          # default values below
                          eta = 0.3,
                          gamma = c(0,0.1,0.01,0.001),
                          subsample = 1,
                          min_child_weight = 1,
                          colsample_bytree = 0.6)
  xgb.fit = caret::train(ytrain~., data = cbind(ytrain,Xtrain), method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid, verbosity = 0)
  
  
  return(list( xgb.model = xgb.fit))
}




kfoldCV.stack <- function(Xtrain, ytrain, nfolds) {
  K <- nfolds
  n <- dim(Xtrain)[1]; p <- dim(Xtrain)[2] #number of observations and number of predictors
  id <- sample(1:K, n, replace=TRUE, prob=rep(1/K,K))
  
  results <- list()
  for(i in 1:K){
    y <- ytrain[id!=i]
    yval <- ytrain[id==i]
    X <- Xtrain[id!=i,]
    Xval <- Xtrain[id==i,]
    
    # Fit linear model
    lm.fit <- lm("y ~ . - 1", data = data.frame(y,X))
    # Predictions in test
    lm.preds <- predict(lm.fit, data.frame(Xval))
    mse_val <- mean(c(yval-lm.preds)^2, na.rm=TRUE)
      
    results[[i]] <- list(model = lm.fit, error = mse_val)
    }
  min.MSE <- which.min(apply(results, MARGIN = 1, FUN = mean))
  return(list(d.min = min.MSE))
}


kfoldCV.betaboost <- function(data, nfolds) {
  print("kfold working")
  K <- nfolds
  n <- nrow(data)
  id <- sample(1:K, n, replace=TRUE, prob=rep(1/K,K))
  print(id)
  # Hyperparameters
  hyper_max_depth <- c(3,5,7)
  
    # empty array to save results
  results <- array(NA, dim=c(length(hyper_max_depth),K))
  print("Here it will start estimating")
  for(lambda in 1:length(hyper_max_depth)){
    hyperparam <- hyper_max_depth[lambda]
    print(paste("Hyperparam: ", hyperparam))
    for(i in 1:K){
      data_train.i <- data[id!=i,]
      data_test.i <- data[id==i,]
      
      print(dim(data_train.i))
    
    
      betaboost.fit <- mboost::blackboost(y ~ ., data = data_train.i, family = betaboost::BetaReg(),
                                        control = mboost::boost_control(mstop = 200),
                                        tree_controls = partykit::ctree_control(maxdepth = hyperparam))
      
      ypred <- predict(betaboost.fit, newdata = data_test.i[,-1], type = "response")
      
      mse.i <- mean((ypred - data_test.i[,1])^2)
      
      results[lambda, i] <- mse.i
    }
  }
  min.mse <- hyper_max_depth[which.min(apply(results, MARGIN = 1, FUN = mean))]
  
  return(list(max_depth.min = min.mse))

}


