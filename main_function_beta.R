source("dataframes.R")
source("splits.R")
source("kfold_cv_selection.R")
source("pls.R")
library(xgboost)
library(caret)

#########################################################
#dataframes (correr la primera vez sin comentar)
#datas = list()
#for (i in c(1,2,5,8,9,10,13,15)){
# nombre <- paste("plsdata", i, sep="_")
#datas[[nombre]] = selectdfs(plsdata,i)

#}


#########################################################
#df = datas[[8]]
target = "mpi_Other"

main_function_beta = function(df, target){
  results = data.frame()
  nfolds <- 5
  
  
  # Train and test datasets
  data <- random.split(df, 0.8)
  ytrain <- data$data_train[, target]
  Xtrain <- data$data_train[,-c(1:33)]
  Ttrain = data$data_train[, c(13:33)]
  Rtrain = data$data_train[, c(8:12)]
  
  ytest <- data$data_test[, target]
  Xtest <- data$data_test[,-c(1:33)]
  Ttest = data$data_test[, c(13:33)]
  Rtest = data$data_test[, c(8:12)]
  
  
   # Create data frame for prediction
  data <- as.data.frame(cbind(ytrain, Xtrain))
  
  # Methods for estimation
  # PLS model, d = 1 
  d_1 = 1
  pls.projections <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d_1, scale = FALSE)
  data <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections$W))
  pls.fit <- lm(ytrain ~ . , data)
  newdata <- data.frame(as.matrix(Xtest) %*% pls.projections$W)
  colnames(newdata) <- colnames(data)[-1]
  # Predict over Xtest
  ytest_pred.pls_d1 <- predict(pls.fit, newdata)
  
  #Non parametric PLS model, d = 1
  bw_d1 <- np::npregbw(ytrain ~ V2, data=data)
  np_PLS_d1 <- np::npreg(bw_d1)
  # Predict over Xtest
  ytest_pred.np_d1 = predict(np_PLS_d1, newdata=newdata, type="response")
  
  #Beta-regression d=1
  data_beta = data.frame(data, Ttrain, Rtrain)
  formu = as.formula(paste("ytrain", paste(names(data_beta)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit = betareg::betareg(formu , data = data_beta)
  # Predict over Xtest
  newdata_beta = data.frame(newdata,Ttest,Rtest)
  ytest_pred.beta_d1 = predict(beta.fit, newdata_beta)
  
  
  #PLS model, optimal d
  # Hyperparameter selection 
  pls.directions = 4
  hyperparam <- kfoldCV.selection2(Xtrain, ytrain, nfolds, pls.directions)
  d_opt = hyperparam$d.min
  pls.projections_dopt <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d_opt, scale = FALSE)
  data_dopt <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections_dopt$W))
  pls.fit_dopt <- lm(ytrain ~ . , data_dopt)
  newdata_dopt <- data.frame(as.matrix(Xtest) %*% pls.projections_dopt$W)
  colnames(newdata_dopt) <- colnames(data_dopt)[-1]
  # Predict over Xtest
  ytest_pred.pls_dopt <- predict(pls.fit_dopt, newdata_dopt)
  
  
  # Non-parametric PLS model, optimal d
  formu = as.formula(paste("ytrain", paste(names(data_dopt)[-1], collapse=" + "), sep=" ~ ")) 
  bw_dopt <- np::npregbw(formu, data=data_dopt) 
  np_PLS_dopt <- np::npreg(bw_dopt)
  # Predict over Xtest
  ytest_pred.np_opt = predict(np_PLS_dopt, newdata=newdata_dopt, type="response")
  
  #Beta-regression d optimo
  data_beta_opt = data.frame(data_dopt, Ttrain, Rtrain)
  formu = as.formula(paste("ytrain", paste(names(data_beta_opt)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit = betareg::betareg(formu , data = data_beta_opt)
  # Predict over Xtest
  newdata_beta = data.frame(newdata_dopt,Ttest,Rtest)
  ytest_pred.beta_opt = predict(beta.fit, newdata_beta)
  
  #mean((ytest-ytest_pred.beta_opt)^2)
  
  
  # LASSO model
  #newdata <- data.frame(Xtest)
  #lasso.fit <- glmnet::glmnet(x = Xtrain, y = ytrain, lambda = hyperparam$lambda.min)
  # Predict over Xtest
  #ytest_pred.lasso <- glmnet::predict.glmnet(lasso.fit, as.matrix(newdata))
  
  #Elastic net
  data_ela = data.frame(Xtrain,Ttrain,Rtrain)
  newdata <- data.frame(Xtest,Ttest,Rtest)
  elastic.fit <- glmnet::glmnet(x = data_ela, y = ytrain, family = "gaussian", alpha = hyperparam$best.alpha, lambda = hyperparam$best.lambda)
  # Predict over Xtest
  ytest_pred.elastic <- glmnet::predict.glmnet(elastic.fit, as.matrix(newdata))
  
  # XGBoost model
  # Predict over Xtest
  ytest_pred.xgb <- predict(hyperparam$xgb.model, newdata)
  
  
  
  
  
  # Saving results
  results = data.frame( 
    "MSE elastic" = mean((ytest-ytest_pred.elastic)^2),
    "MSE pls_d1" = mean((ytest-ytest_pred.pls_d1)^2),
    "MSE pls_opt" = mean((ytest-ytest_pred.pls_dopt)^2),
    "MSE pls_np_d1" = mean((ytest-ytest_pred.np_d1)^2),
    "MSE pls_np_opt" = mean((ytest-ytest_pred.np_opt)^2),
    "MSE xgBoost" = mean((ytest-ytest_pred.xgb)^2),
    "MSE beta_d1" = mean((ytest-ytest_pred.beta_d1)^2),
    "MSE beta_dopt" = mean((ytest-ytest_pred.beta_opt)^2),
    "MAE elastic" = mean(abs(ytest-ytest_pred.elastic)),
    "MAE pls_d1" = mean(abs(ytest-ytest_pred.pls_d1)),
    "MAE pls_opt" = mean(abs(ytest-ytest_pred.pls_dopt)),
    "MAE pls_np_d1" = mean(abs(ytest-ytest_pred.np_d1)),
    "MAE pls_np_opt" = mean(abs(ytest-ytest_pred.np_opt)),
    "MAE xgBoost" = mean(abs(ytest-ytest_pred.xgb)),
    "MAE beta_d1" = mean(abs(ytest-ytest_pred.beta_d1)),
    "MAE beta_dopt" = mean(abs(ytest-ytest_pred.beta_opt)),
    "n" = nrow(df),  
    "p"= length(colnames(df)[!((colnames(df) %in% c(colnames(df)[1:13])))]),  
    "Total de paises" = length(unique(df$iso)),
    "d optimo" = d_opt,
    "variables elastic" = length(as.vector(unname(coefficients(elastic.fit))))-sum(as.vector(unname(coefficients(elastic.fit))) == 0)
    
    
    
  )
  
  return(results)
  
}
