main_function <- function(data = NULL, target = c("MPI","H","A"), nfolds = 5,
                          methods = c("elasticnet","betaboost"), ...){

# target to lower case ----------------------------------------------------
  # target <- switch(target,
  #                  MPI = {"mpi"},
  #                  H = {"h"},
  #                  A = {"a"})  

# Preprocess data ---------------------------------------------------------

  # Include time effects as a trend
  data$year_trend <- as.numeric(as.character(data$year_Other))
  data$year_trend <- data$year_trend - min(data$year_trend)
  
  # Split data in train and test
  split <- random.split(data, 0.8)
  ytrain <- split$data_train[, target] 
  Xtrain <- split$data_train[,-c(1:33)]
  
  ytest <- split$data_test[, target]
  Xtest <- split$data_test[,-c(1:33)]
  
    # keep regions dummy variables apart
  Rtrain <- split$data_train[, c(8:12)]
  Rtest <- split$data_test[, c(8:12)]
  
  
  # Split in train and test predictors
  data_train <- data.frame(Xtrain, Rtrain)
  data_test <- data.frame(Xtest, Rtest)
    #for PLS....
    #regions.cols <- which(grepl(pattern = "^df.region_", colnames(data_train)))
  
  predictions <- lapply(methods, FUN = function(x) {
    switch(x,
           "elasticnet" = {method.elasticnet(data_train, ytrain, data_test, nfolds)},
           "betaboost" = {method.betaboost(data_train, ytrain, data_test, nfolds)})
  })
  
  return(apply((do.call("cbind",predictions) - ytest)^2, 2, mean) )
}

sim <- lapply(1:10, FUN = main_function, data = data, 
                    target = "mpi_Other",
                    methods = c("elasticnet","betaboost"), nfolds = 5)

# Auxiliar functions ------------------------------------------------------

method.elasticnet <- function(data_train, y_train, data_test, nfolds){
  # Select hyperparameters
  hyperparam <- kfoldCV.elastic(data_train, y_train, nfolds)
  # Fit model
  elastic.fit <- glmnet::glmnet(x = data_train, y = y_train, family = "gaussian",
                                   alpha = hyperparam$best.alpha,
                                   lambda = hyperparam$best.lambda)
  # Predict over test data
  ypred <- glmnet::predict.glmnet(elastic.fit, as.matrix(data_test))
  colnames(ypred) <- "elasticnet"
  return(ypred)
} 

method.betaboost <- function(data_train, y_train, data_test, nfolds){
  # merge ytrain and predictors
  y_train <- y_train + 1e-06 # ensure that is not equal to zero
  data_betaboost <- data.frame(y_train, data_train)
  colnames(data_betaboost)[1] <- "y" 
  print("Create data_betaboost")
  # Select hyperparameters
  hyperparam <- kfoldCV.betaboost(data_betaboost, nfolds)
  print("kfold CV worked!")
  # Fit model
  betaboost.fit <- mboost::blackboost(y ~ ., data = data_betaboost, family = betaboost::BetaReg(),
                                      control = mboost::boost_control(mstop = 200),
                                      tree_controls = partykit::ctree_control(maxdepth = hyperparam$max_depth.min))
  
  # Predict over test data
  ypred <- predict(betaboost.fit, newdata = data_test, type = "response")
  colnames(ypred) <- "betaboost"
  return(ypred)
  }
