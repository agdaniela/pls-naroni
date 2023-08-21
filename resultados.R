source("dataframes.R")
source("splits.R")
source("kfold_cv_selection.R")
source("pls.R")
library(xgboost)
library(caret)

#########################################################
#dataframes
datas = list()
for (i in c(1,2,5,8,9,10,13,15)){
  nombre <- paste("plsdata", i, sep="_")
  datas[[nombre]] = selectdfs(plsdata,i)
  
}

#########################################################


main_function = function(df, target){
  results = data.frame()
  nfolds <- 5
  
  
  # Train and test datasets
  data <- random.split(df, 0.8)
  ytrain <- data$data_train[, target]; Xtrain <- scale(data$data_train[,-c(1:13)])
  ytrain <- (ytrain - mean(ytrain))/sd(ytrain)
  ytest <- data$data_test[, target]; Xtest <- scale(data$data_test[,-c(1:13)])
  ytest <- (ytest - mean(ytest))/sd(ytest)
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
  
  
  #PLS model, optimal d
  # Hyperparameter selection 
  pls.directions = 30
  hyperparam <- kfoldCV.selection(Xtrain, ytrain, nfolds, pls.directions)
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
  
  
  # LASSO model
  newdata <- data.frame(Xtest)
  lasso.fit <- glmnet::glmnet(x = Xtrain, y = ytrain, lambda = hyperparam$lambda.min)
  # Predict over Xtest
  ytest_pred.lasso <- glmnet::predict.glmnet(lasso.fit, as.matrix(newdata))
  
  # XGBoost model
  # Predict over Xtest
  ytest_pred.xgb <- predict(hyperparam$xgb.model, Xtest)
  
  # Saving results
  results = data.frame( 
                     #"n" = nrow(df), #Total de observaciones
                     #"p"= length(colnames(df)[!((colnames(df) %in% c("iso","country","region","year","MPI","H","A",colnames(df)[8:13])))]), #Total de predictores (WBI)
                     #"Total de paises" = length(unique(df$iso)),
                     "MSE lasso" = mean((ytest-ytest_pred.lasso)^2),
                     "MSE pls_d1" = mean((ytest-ytest_pred.pls_d1)^2),
                     "MSE pls_opt" = mean((ytest-ytest_pred.pls_dopt)^2),
                     "MSE pls_np_d1" = mean((ytest-ytest_pred.np_d1)^2),
                     "MSE pls_np_opt" = mean((ytest-ytest_pred.np_opt)^2),
                     "MSE xgBoost" = mean((ytest-ytest_pred.xgb)^2)
                     #"d optimo" = d_opt,
                     #"variables lasso" = length(as.vector(unname(coefficients(lasso.fit))))-sum(as.vector(unname(coefficients(lasso.fit))) == 0)
                     
  )
   
  return(results)
  
}





#########################################################
#tabla de resultados
resultados = function(){
  tabla = data.frame()
  
  for (i in c(1:8)) {
    df <- datas[[i]]
    
    #train and test
    target <- "mpi_Other"
    data <- random.split(df, 0.8)
    ytrain <- data$data_train[, target]; Xtrain <- scale(data$data_train[,-c(1:13)])
    ytrain <- (ytrain - mean(ytrain))/sd(ytrain)
    ytest <- data$data_test[, target]; Xtest <- scale(data$data_test[,-c(1:13)])
    ytest <- (ytest - mean(ytest))/sd(ytest)
    # Create data frame for prediction
    data <- as.data.frame(cbind(ytrain, Xtrain))
    
    
    
    ##################################################
    #Estimation
    
    nfolds <- 5
    
    
    # PLS d = 1 
    d_1 = 1
    pls.projections <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d_1, scale = FALSE)
    data <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections$W))
    pls.fit <- lm(ytrain ~ . , data)
    newdata <- data.frame(as.matrix(Xtest) %*% pls.projections$W)
    colnames(newdata) <- colnames(data)[-1]
    
    # Predict over Xtest
    ytest_pred.pls_d1 <- predict(pls.fit, newdata)
    
    #Non parametric PLS d=1
    bw_d1 <- np::npregbw(ytrain ~ V2, data=data)
    np_PLS_d1 <- np::npreg(bw_d1)
    # Predict over Xtest
    ytest_pred.np_d1 = predict(np_PLS_d1, newdata=newdata, type="response")
    
    
    #PLS d optimo
    # Hyperparameter selection ####
    pls.directions = 30
    hyperparam <- kfoldCV.selection(Xtrain, ytrain, nfolds, pls.directions)
    d_opt = hyperparam$d.min
    pls.projections_dopt <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d_opt, scale = FALSE)
    data_dopt <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections_dopt$W))
    pls.fit_dopt <- lm(ytrain ~ . , data_dopt)
    newdata_dopt <- data.frame(as.matrix(Xtest) %*% pls.projections_dopt$W)
    colnames(newdata_dopt) <- colnames(data_dopt)[-1]
    # Predict over Xtest
    ytest_pred.pls_dopt <- predict(pls.fit_dopt, newdata_dopt)
    
    
    # Non-parametric PLS d optimo
    formu = as.formula(paste("ytrain", paste(names(data_dopt)[-1], collapse=" + "), sep=" ~ ")) 
    bw_dopt <- np::npregbw(formu, data=data_dopt) #ojo le puse un punto en vez de v2
    np_PLS_dopt <- np::npreg(bw_dopt)
    # Predict over Xtest
    ytest_pred.np_opt = predict(np_PLS_dopt, newdata=newdata_dopt, type="response")
    
    
    # LASSO model
    newdata <- data.frame(Xtest)
    lasso.fit <- glmnet::glmnet(x = Xtrain, y = ytrain, lambda = hyperparam$lambda.min)
    # Predict over Xtest
    ytest_pred.lasso <- glmnet::predict.glmnet(lasso.fit, as.matrix(newdata))
    
    
   
    
    # XGBoost model
    # Predict over Xtest
    ytest_pred.xgb <- predict(hyperparam$xgb.model, Xtest)
    
    
    
    fila1 = data.frame("nombre del df" = paste("plsdata",i,sep = "_"), 
                       "n" = nrow(df), #Total de observaciones
                       "p"= length(colnames(df)[!((colnames(df) %in% c("iso","country","region","year","MPI","H","A",colnames(df)[8:13])))]), #Total de predictores (WBI)
                       "Total de paises" = length(unique(df$iso)),
                       "MSE lasso" = mean((ytest-ytest_pred.lasso)^2),
                       "MSE pls_d1" = mean((ytest-ytest_pred.pls_d1)^2),
                       "MSE pls_opt" = mean((ytest-ytest_pred.pls_dopt)^2),
                       "MSE pls_np_d1" = mean((ytest-ytest_pred.np_d1)^2),
                       "MSE pls_np_opt" = mean((ytest-ytest_pred.np_opt)^2),
                       "MSE xgBoost" = mean((ytest-ytest_pred.xgb)^2),
                       "d optimo" = d_opt,
                       "variables lasso" = length(as.vector(unname(coefficients(lasso.fit))))-sum(as.vector(unname(coefficients(lasso.fit))) == 0)
                       
    )
    tabla = rbind(tabla,fila1)
  }
  numdedf = seq(1:nrow(tabla))
  tabla = cbind(numdedf,tabla)
  tabla$Ganador <- apply(tabla[,5:12], 1, function(x){names(tabla[,5:12])[which.min(x)]})
  
  return(tabla)
}


#res3 = resultados()

#########################################################

resultadosviejos = function(nom,num){
  tabla = data.frame()
  
  for (i in 1:num) {
    df <- selectdfs(nom,i)
    
    target <- "mpi_Other"
    nfolds <- 5
    pls.directions <- 20
    
    # Train and Test split ####
    data <- random.split(df, 0.8)
    
    ytrain <- data$data_train[, target]; Xtrain <- scale(data$data_train[,-c(1:13)])
    ytrain <- (ytrain - mean(ytrain))/sd(ytrain)
    ytest <- data$data_test[, target]; Xtest <- scale(data$data_test[,-c(1:13)])
    ytest <- (ytest - mean(ytest))/sd(ytest)
    # Hyperparameter selection ####
    hyperparam <- kfoldCV.selection(Xtrain, ytrain, nfolds, pls.directions)
    
    # Estimate model ####
    
    # Linear model
    # Create data frame for prediction
    data <- as.data.frame(cbind(ytrain, Xtrain))
    # Fit model
    lm.fit <- lm(ytrain ~ . , data)
    # New data for prediction
    newdata <- data.frame(Xtest)
    # Predict over Xtest
    ytest_pred.lm <- predict(lm.fit, newdata)
    
    # LASSO model
    # Fit model
    lasso.fit <- glmnet::glmnet(x = Xtrain, y = ytrain, lambda = hyperparam$lambda.min)
    # Predict over Xtest
    ytest_pred.lasso <- glmnet::predict.glmnet(lasso.fit, as.matrix(newdata))
    
    # PLS model
    # Fit linear model
    pls.projections <- chemometrics::pls1_nipals(Xtrain, ytrain, a = hyperparam$d.min, scale = FALSE)
    data <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections$W))
    pls.fit <- lm(ytrain ~ . , data)
    
    newdata <- data.frame(as.matrix(Xtest) %*% pls.projections$W)
    colnames(newdata) <- colnames(data)[-1]
    # Predict over Xtest
    ytest_pred.pls <- predict(pls.fit, newdata)
    
    # Fit non-parametric model
    
    bw <- np::npregbw(ytrain ~ V2, data=data)
    np_PLS <- np::npreg(bw)
    # Predict over Xtest
    ytest_pred.np = predict(np_PLS, newdata=newdata, type="response")
     
    
    
    fila1 = data.frame("nombre del df" = paste("plsdata",i), 
                      "n" = nrow(df), #Total de observaciones
                       "p"= length(colnames(df)[!((colnames(df) %in% c("iso","country","region","year","MPI","H","A",colnames(df)[8:13])))]), #Total de predictores (WBI)
    "Total de paises" = length(unique(df$iso)),
    "MSE lm" = mean((ytest-ytest_pred.lm)^2),
    "MSE lasso" = mean((ytest-ytest_pred.lasso)^2),
    "MSE pls" = mean((ytest-ytest_pred.pls)^2),
    "MSE np" = mean((ytest-ytest_pred.np)^2)
    
    )
    tabla = rbind(tabla,fila1)
  }
  
  numdedf = seq(1:nrow(tabla))
  tabla = cbind(numdedf,tabla)
  tabla$Ganador <- apply(tabla[,6:9], 1, function(x){names(tabla[,6:9])[which.min(x)]})
  
  return(tabla)
}

#res1 = resultados(plsdata,12)
#res2 = resultados(plsdata,16)
#res3 = resultados(plsdata,24)
#res4 = resultados(plsdata,10)
#res5 = resultados(plsdata,15)


 
