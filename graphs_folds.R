# Crear 5 folds disjuntos - predecir en uno




#Xtrain =trainData
#Xtest = testData
main_function_pred = function(Xtrain, Xtest , target,corte, link_phi, link_mu, distancia){
  
  predicted = data.frame()
  results = data.frame()
  nfolds <- 5
  
  Xtrain$year_trend <- as.numeric(as.character(Xtrain$year_Other))
  Xtrain$year_trend <- Xtrain$year_trend - min(Xtrain$year_trend)
  
  Xtest$year_trend <- as.numeric(as.character(Xtest$year_Other))
  Xtest$year_trend <- Xtest$year_trend - min(Xtest$year_trend)
  
  # Train and test datasets
  ytrain <- Xtrain[, target] 
  
  Ttrain = Xtrain[, c(13:33)]
  Rtrain = Xtrain[, c(8:12)]
  Xtrain <- Xtrain[,-c(1:33)]
  
  ytest <- Xtest[, target]
  Ttest = Xtest[, c(13:33)]
  Rtest = Xtest[, c(8:12)]
  Xtest <- Xtest[,-c(1:33)]
  
  pls.directions = 30 
   
  
  ########################################## ----
  
  # 
  ##################################################################################################### ----
  # Continuous time
  
  # 1.  Using dimension reduction
  
  hyperparam_pls <- kfoldCV.pls(Xtrain, ytrain, nfolds, pls.directions)
  hyperparam_beta <- kfoldCV.plsbeta(Xtrain, ytrain, nfolds, pls.directions)
  d_pls = hyperparam_pls$d.min
  d_plsbeta = hyperparam_beta$d.min
  #  
  
  # 1.1 PLS with optimal $d$ using linear predictor. 
  #  Data with dimension reduction
  pls.projections_tc_cr <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d_pls, scale = FALSE)
  data_tc_cr <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections_tc_cr$W))
  newdata_tc_cr <- data.frame(as.matrix(Xtest) %*% pls.projections_tc_cr$W)
  colnames(newdata_tc_cr) <- colnames(data_tc_cr)[-1]
  
  # Train and test data with dimension reduction and dummies of time and region - formula of fit
  data_tc_cr_dum = data.frame(data_tc_cr, Rtrain)
  formu_tc_cr = as.formula(paste("ytrain", paste(names(data_tc_cr_dum)[-1], collapse=" + "), sep=" ~ ")) 
  newdata_tc_cr_dum = data.frame(newdata_tc_cr,Rtest)
  
  # Fit
  pls.fit <- tryCatch(lm(ytrain ~ . , data_tc_cr_dum), error= function(e) {return(NA)}  )
  # Predict over Xtest
  ytest_pred.pls_tc  <- tryCatch(predict(pls.fit, newdata_tc_cr_dum), error= function(e) {return(NA)}  )
  # Distance
  dist_pls_tc = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.pls_tc)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
   # 
  # 1.3 PLS with optimal $d$ using beta regression (For hyperparameter, training with the prediction model)
  pls.projections_tc_dbeta <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d_plsbeta, scale = FALSE)
  data_tc_dbeta <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections_tc_dbeta$W))
  newdata_tc_dbeta <- data.frame(as.matrix(Xtest) %*% pls.projections_tc_dbeta$W)
  colnames(newdata_tc_dbeta) <- colnames(data_tc_dbeta)[-1]
  
  # Fit
  formu_tc_dbeta = as.formula(paste("ytrain", paste(names(data_tc_dbeta)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_tc_cr <- tryCatch(betareg::betareg(formu_tc_dbeta, data = data_tc_dbeta, link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
  # Predict over Xtest
  ytest_pred.beta_tc_cr = tryCatch(betareg::predict(beta.fit_tc_cr, newdata_tc_dbeta, type = "response"), error= function(e) {return(NA)}  )
  # distance
  dist_beta_tc_cr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_cr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  
  # 1.4 PLS with optimal $d$ using beta tree model for prediction.
  
  # Fit
  dummy_corte_train = ifelse(ytrain<=corte, 1, 0)
  dummy_corte_test = as.vector(ifelse(ytest<=corte, 1, 0))
  #
  data_beta_tc_tree_cr = data.frame(data_tc_dbeta,dummy_corte_train)
  formu_tc_dbeta_tree = as.formula(paste("ytrain", paste(names(data_beta_tc_tree_cr)[-1], collapse=" + "), sep=" ~ ")) 
  
  beta.fit_tc_tree_cr <- tryCatch(betareg::betatree(formu_tc_dbeta_tree, ~ dummy_corte_train, data = data_beta_tc_tree_cr ,  link.phi = link_phi, link = link_mu ), error= function(e) {return(NA)}  )
  #predict
  newdata_beta_tc_tree_cr = data.frame(newdata_tc_dbeta, dummy_corte_test)
  names(newdata_beta_tc_tree_cr)[length(names(newdata_beta_tc_tree_cr))]  = "dummy_corte_train"
  ytest_pred.beta_tc_tree_cr =  tryCatch(predict(beta.fit_tc_tree_cr, newdata_beta_tc_tree_cr, type = "response"), error= function(e) {return(NA)}  )
  # distance
  dist_beta_tc_tree_cr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_tree_cr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ) , error= function(e) {return(NA)}  )
  
  
  
  # 2. Based on Regularized Regresion/Variable Selection
  # Data without dimension reduction
  data_tc_sr = data.frame(Xtrain, Rtrain)
  newdata_tc_sr <- data.frame(Xtest, Rtest)
  
  # 2.1 Elastic Net.
  # Fit 
  hyperparam <- kfoldCV.elastic(data_tc_sr, ytrain, nfolds)
  elastic.fit_tc <- glmnet::glmnet(x = data_tc_sr, y = ytrain, family = "gaussian", alpha = hyperparam$best.alpha, lambda = hyperparam$best.lambda)
  # Predict over Xtest
  ytest_pred.elastic_tc <- glmnet::predict.glmnet(elastic.fit_tc, as.matrix(newdata_tc_sr))
  # distance
  dist_elastic_tc = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.elastic_tc)$y), est.prob = "empirical", method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  # 2.2 Beta regression using selected covariates with elastic net.
  tmp_coeffs_tc <- coef(elastic.fit_tc, s = "lambda.min")
  var_selected_tc = tmp_coeffs_tc@Dimnames[[1]][tmp_coeffs_tc@i +1]
  Xtrain_tc_ela = Xtrain[,colnames(Xtrain) %in% var_selected_tc]
  Xtest_tc_ela <- Xtest[,colnames(Xtest) %in% var_selected_tc]
  # Fit
  data_beta_tc_ela = data.frame(Xtrain_tc_ela, Rtrain)
  formu_tc_ela = as.formula(paste("ytrain", paste(names(data_beta_tc_ela)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_tc_ela <- tryCatch(betareg::betareg(formu_tc_ela , data = data_beta_tc_ela, link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
  # Predict over Xtest
  newdata_beta_tc_ela = data.frame(Xtest_tc_ela, Rtest)
  ytest_pred.beta_tc_ela = tryCatch(betareg::predict(beta.fit_tc_ela, newdata_beta_tc_ela, type = "response"), error= function(e) {return(NA)}  )
  # distance
  dist_beta_tc_ela = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_ela)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  # 2.3 Beta tree using selected covariates with elastic net.
  # Fit
  data_beta_tc_tree_ela = data.frame(data_beta_tc_ela, dummy_corte_train)
  formu_tc_tree_ela = as.formula(paste("ytrain", paste(names(data_beta_tc_tree_ela)[-1], collapse=" + "), sep=" ~ "))
  beta.fit_tc_tree_ela <- tryCatch(betareg::betatree(formu_tc_tree_ela, ~ dummy_corte_train, data = data_beta_tc_tree_ela,  link.phi = link_phi, link = link_mu )  ,error = function(e) {return((NA))})
  # predict
  newdata_beta_tc_tree_ela = data.frame(newdata_beta_tc_ela, dummy_corte_test )
  names(newdata_beta_tc_tree_ela)[length(names(newdata_beta_tc_tree_ela))]  = "dummy_corte_train"
  ytest_pred.beta_tc_tree_ela =  tryCatch(betareg::predict(beta.fit_tc_tree_ela, newdata_beta_tc_tree_ela), error= function(e) {return(NA)})
  # distance
  dist_beta_tc_tree_ela = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_tree_ela)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )

 
  # 3 Based on Model Selection
  
  # 3.1 XGBoost lineal.
  # Fit
  hyperparam_xgb_tc = kfoldCV.xgboost(data_tc_sr, ytrain, nfolds)
  # Predict over Xtest
  ytest_pred.xgb_tc <- tryCatch(predict(hyperparam_xgb_tc$xgb.model, newdata_tc_sr), error= function(e) {return(NA)}  )
  # distance
  dist_xgb_tc = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.xgb_tc)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  # 3.2 Beta Boost.
  # Fit
  formu_boost_tc = as.formula(paste("ytrain", paste(names(data_tc_sr)[-1], collapse=" + "), sep=" ~ ")) 
  betaboost.fit_tc <- tryCatch(mboost::glmboost(formu_boost_tc, data = data_tc_sr, family = betaboost::BetaReg()), error= function(e) {return(NA)}  )
  # Predict over Xtest
  ytest_pred.betaboost_tc = tryCatch(predict(betaboost.fit_tc, newdata = newdata_tc_sr, type = "response"), error= function(e) {return(NA)}  )
  dist_betaboost_tc  = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.betaboost_tc)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  
  
  
  #########################################################################
  
  # Saving results
  results = data.frame( 
    
    
    
    "MSE pls_tc" = mean((ytest-ytest_pred.pls_tc)^2),
    "MSE beta_tc_cr" = mean((ytest-ytest_pred.beta_tc_cr)^2),
    "MSE beta_tc_tree_cr" = mean((ytest-ytest_pred.beta_tc_tree_cr)^2),
    
    "MSE elastic_tc" = mean((ytest-ytest_pred.elastic_tc)^2),
    "MSE beta_tc_ela" = mean((ytest-ytest_pred.beta_tc_ela)^2),
    "MSE beta_tc_tree_ela" = mean((ytest-ytest_pred.beta_tc_tree_ela)^2),

    "MSE xgb_tc" = mean((ytest-ytest_pred.xgb_tc)^2),
    "MSE betaboost_tc" = mean((ytest-ytest_pred.betaboost_tc)^2),
    
    # 
    "dist pls_tc" = dist_pls_tc,
    "dist beta_tc_cr" = dist_beta_tc_cr,
    "dist beta_tc_tree_cr" = dist_beta_tc_tree_cr,
    
    "dist elastic_tc" = dist_elastic_tc,
    "dist beta_tc_ela" = dist_beta_tc_ela,
    "dist beta_tc_tree_ela" = dist_beta_tc_tree_ela,

    "dist xgb_tc" = dist_xgb_tc,
    "dist betaboost_tc" = dist_betaboost_tc,
    
   
    
    
    "n" = nrow(df),  
    "p"= length(colnames(df)[!((colnames(df) %in% c(colnames(df)[1:33])))]),
    "Total de paises" = length(unique(df$iso))
    
    
  )
  
  predicted = data.frame(
    
    "y_test" = ytest,
    
   
    # 
    "yhat pls_tc" = ytest_pred.pls_tc,
    "yhat beta_tc_cr" = ytest_pred.beta_tc_cr,
    "yhat beta_tc_tree_cr" = ytest_pred.beta_tc_tree_cr,
    
    "yhat elastic_tc" = ytest_pred.elastic_tc,
    "yhat beta_tc_ela" = ytest_pred.beta_tc_ela,
    "yhat beta_tc_tree_ela" = ytest_pred.beta_tc_tree_ela,

    "yhat xgb_tc" = ytest_pred.xgb_tc,
    "yhat betaboost_tc" = ytest_pred.betaboost_tc
    
  )
     
  
  return(list("results"=results,"predicted" = predicted))
  
}






#######################################################################
# Ten fold experiment df2
#######################################################################

df_fold  = datas[[2]] 

df_fold  <- df_fold[sample(nrow(df_fold)),]

folds_index <- cut(seq(1,nrow(df_fold)),breaks=10,labels=FALSE) #indices

predichos10 = list()
for(i in 1:10){
  testIndexes <- which(folds_index == i,arr.ind=TRUE)
  testData <- df_fold[testIndexes, ]
  trainData <- df_fold[-testIndexes, ]
  nombre <- paste("yhats", i, sep="_")
  predichos10[[nombre]] = main_function_pred(trainData,testData,"mpi_Other", link_phi = "log", link_mu = "logit", distancia = "hellinger")
}
View(predichos[["yhats_1"]]$predicted)

View(predichos10[["yhats_1"]]$predicted)
View(predichos10[["yhats_2"]]$predicted)
View(predichos10[["yhats_3"]]$predicted)
View(predichos10[["yhats_4"]]$predicted)
View(predichos10[["yhats_5"]]$predicted)
View(predichos10[["yhats_6"]]$predicted)
View(predichos10[["yhats_7"]]$predicted) 
View(predichos10[["yhats_8"]]$predicted) 
View(predichos10[["yhats_9"]]$predicted) 
View(predichos10[["yhats_10"]]$predicted) 

saveRDS(df_fold, "df_fold.Rdata")
saveRDS(predichos10, "predichos_mpi.Rdata")

# Folds for A and H
# cortes: mpi=0.2, h=0.2,a=0.5
df_fold$a_Other = df_fold$a_Other / 100
df_fold$h_Other = df_fold$h_Other / 100

predichos_a = list()
for(i in 1:10){
  testIndexes <- which(folds_index == i,arr.ind=TRUE)
  testData <- df_fold[testIndexes, ]
  trainData <- df_fold[-testIndexes, ]
  nombre <- paste("ahats", i, sep="_")
  predichos_a[[nombre]] = main_function_pred(trainData,testData,"a_Other", corte=0.5, link_phi = "log", link_mu = "logit", distancia = "hellinger")
}

View(predichos_a[["ahats_1"]]$predicted)
View(predichos_a[["ahats_2"]]$predicted)
View(predichos_a[["ahats_3"]]$predicted)
View(predichos_a[["ahats_4"]]$predicted)
View(predichos_a[["ahats_5"]]$predicted)
View(predichos_a[["ahats_6"]]$predicted)
View(predichos_a[["ahats_7"]]$predicted) 
View(predichos_a[["ahats_8"]]$predicted) 
View(predichos_a[["ahats_9"]]$predicted) 
View(predichos_a[["ahats_10"]]$predicted) 


saveRDS(predichos_a,"predichos_a.Rdata")


predichos_h = list()
for(i in 1:10){
  testIndexes <- which(folds_index == i,arr.ind=TRUE)
  testData <- df_fold[testIndexes, ]
  trainData <- df_fold[-testIndexes, ]
  nombre <- paste("hhats", i, sep="_")
  predichos_h[[nombre]] = main_function_pred(trainData,testData,"h_Other", corte=0.2, link_phi = "log", link_mu = "logit", distancia = "hellinger")
}

View(predichos_h[["hhats_1"]]$predicted)
View(predichos_h[["hhats_2"]]$predicted)
View(predichos_h[["hhats_3"]]$predicted)
View(predichos_h[["hhats_4"]]$predicted)
View(predichos_h[["hhats_5"]]$predicted)
View(predichos_h[["hhats_6"]]$predicted)
View(predichos_h[["hhats_7"]]$predicted) 
View(predichos_h[["hhats_8"]]$predicted) 
View(predichos_h[["hhats_9"]]$predicted) 
View(predichos_h[["hhats_10"]]$predicted) 


saveRDS(predichos_h,"predichos_h.Rdata")

#######################################################################
# Ten fold experiment df1 
#######################################################################

df_fold_1  = datas[[1]] 
#df_1 = datas[[1]] #249 observations 110 WBI
df_fold_1$h_Other = df_fold_1$h_Other /100
df_fold_1$a_Other = df_fold_1$a_Other /100

df_fold_1$mpi_Other = df_fold_1$mpi_Other + 0.0000000001
df_fold_1$h_Other = df_fold_1$h_Other + 0.0000000001
df_fold_1$a_Other = df_fold_1$a_Other + 0.0000000001

df_fold_1  <- df_fold_1[sample(nrow(df_fold_1)),]

folds_index_1 <- cut(seq(1,nrow(df_fold_1)),breaks=10,labels=FALSE) #indices

predichos_df1 = list()
for(i in 1:10){
  testIndexes_1 <- which(folds_index_1 == i,arr.ind=TRUE)
  testData_1 <- df_fold_1[testIndexes_1, ]
  trainData_1 <- df_fold_1[-testIndexes_1, ]
  nombre <- paste("yhats", i, sep="_")
  predichos_df1[[nombre]] = main_function_pred(trainData_1,testData_1,"mpi_Other",corte=0.2, link_phi = "log", link_mu = "logit", distancia = "hellinger")
}


saveRDS(df_fold_1, "df_fold_1.Rdata")
saveRDS(predichos_df1, "predichos_mpi_df1.Rdata")
predichos_mpi_df1 = readRDS("predichos_mpi_df1.Rdata")
df_fold_1 = readRDS("df_fold_1.Rdata")

View(predichos_mpi_df1[["yhats_10"]]$results)



predichos_a_df1 = list()
for(i in 1:10){
  testIndexes_1 <- which(folds_index_1 == i,arr.ind=TRUE)
  testData_1 <- df_fold_1[testIndexes_1, ]
  trainData_1 <- df_fold_1[-testIndexes_1, ]
  nombre <- paste("ahats", i, sep="_")
  predichos_a_df1[[nombre]] = main_function_pred(trainData_1,testData_1,"a_Other", corte=0.5, link_phi = "log", link_mu = "logit", distancia = "hellinger")
}

View(predichos_a_df1[["ahats_1"]]$predicted)
View(predichos_a[["ahats_2"]]$predicted)
View(predichos_a[["ahats_3"]]$predicted)
View(predichos_a[["ahats_4"]]$predicted)
View(predichos_a[["ahats_5"]]$predicted)
View(predichos_a[["ahats_6"]]$predicted)
View(predichos_a[["ahats_7"]]$predicted) 
View(predichos_a[["ahats_8"]]$predicted) 
View(predichos_a[["ahats_9"]]$predicted) 
View(predichos_a[["ahats_10"]]$predicted) 


saveRDS(predichos_a_df1,"predichos_a_df1.Rdata")


predichos_h_df1 = list()
for(i in 1:10){
  testIndexes_1 <- which(folds_index_1 == i,arr.ind=TRUE)
  testData_1 <- df_fold_1[testIndexes_1, ]
  trainData_1 <- df_fold_1[-testIndexes_1, ]
  nombre <- paste("hhats", i, sep="_")
  predichos_h_df1[[nombre]] = main_function_pred(trainData_1,testData_1,"h_Other", corte=0.2, link_phi = "log", link_mu = "logit", distancia = "hellinger")
}

View(predichos_h[["hhats_1"]]$predicted)
View(predichos_h[["hhats_2"]]$predicted)
View(predichos_h[["hhats_3"]]$predicted)
View(predichos_h[["hhats_4"]]$predicted)
View(predichos_h[["hhats_5"]]$predicted)
View(predichos_h[["hhats_6"]]$predicted)
View(predichos_h[["hhats_7"]]$predicted) 
View(predichos_h[["hhats_8"]]$predicted) 
View(predichos_h[["hhats_9"]]$predicted) 
View(predichos_h_df1[["hhats_10"]]$predicted) 


saveRDS(predichos_h_df1,"predichos_h_df1.Rdata")
readRDS("predichos_h_df1.Rdata")

#######################################################################
# Ten fold experiment df13 
#######################################################################

df_fold_13  = datas[[7]] 
df_fold_13$h_Other = df_fold_13$h_Other /100
df_fold_13$a_Other = df_fold_13$a_Other /100

df_fold_13$mpi_Other = df_fold_13$mpi_Other + 0.0000000001
df_fold_13$h_Other = df_fold_13$h_Other + 0.0000000001
df_fold_13$a_Other = df_fold_13$a_Other + 0.0000000001
#
df_fold_13  <- df_fold_13[sample(nrow(df_fold_13)),]

folds_index_13 <- cut(seq(1,nrow(df_fold_13)),breaks=10,labels=FALSE) #indices

predichos_df13 = list()
for(i in 1:10){
  testIndexes_13 <- which(folds_index_13 == i,arr.ind=TRUE)
  testData_13 <- df_fold_13[testIndexes_13, ]
  trainData_13 <- df_fold_13[-testIndexes_13, ]
  nombre <- paste("yhats", i, sep="_")
  predichos_df13[[nombre]] = main_function_pred(trainData_13,testData_13,"mpi_Other",corte=0.2, link_phi = "log", link_mu = "logit", distancia = "hellinger")
}
View(predichos[["yhats_1"]]$predicted)


saveRDS(df_fold_13, "df_fold_13.Rdata")
saveRDS(predichos_df13, "predichos_mpi_df13.Rdata")

# Folds for A and H
# cortes: mpi=0.2, h=0.2,a=0.5

predichos_a_df13 = list()
for(i in 1:10){
  testIndexes_13 <- which(folds_index_13 == i,arr.ind=TRUE)
  testData_13 <- df_fold_13[testIndexes_13, ]
  trainData_13 <- df_fold_13[-testIndexes_13, ]
  nombre <- paste("ahats", i, sep="_")
  predichos_a_df13[[nombre]] = main_function_pred(trainData_13,testData_13,"a_Other", corte=0.5, link_phi = "log", link_mu = "logit", distancia = "hellinger")
}

View(predichos_a[["ahats_1"]]$predicted)
View(predichos_a[["ahats_2"]]$predicted)
View(predichos_a[["ahats_3"]]$predicted)
View(predichos_a[["ahats_4"]]$predicted)
View(predichos_a[["ahats_5"]]$predicted)
View(predichos_a[["ahats_6"]]$predicted)
View(predichos_a[["ahats_7"]]$predicted) 
View(predichos_a[["ahats_8"]]$predicted) 
View(predichos_a[["ahats_9"]]$predicted) 
View(predichos_a[["ahats_10"]]$predicted) 


saveRDS(predichos_a_df13,"predichos_a_df13.Rdata")
readRDS("predichos_a_df13.Rdata")

predichos_h_df13 = list()
for(i in 1:10){
  testIndexes_13 <- which(folds_index_13 == i,arr.ind=TRUE)
  testData_13 <- df_fold_13[testIndexes_13, ]
  trainData_13 <- df_fold_13[-testIndexes_13, ]
  nombre <- paste("hhats", i, sep="_")
  predichos_h_df13[[nombre]] = main_function_pred(trainData_13,testData_13,"h_Other", corte=0.2, link_phi = "log", link_mu = "logit", distancia = "hellinger")
}



saveRDS(predichos_h_df13,"predichos_h_df13.Rdata")
readRDS("predichos_h_df13.Rdata")



#######################################################################
#TABLAS

# df2
predichos10 = readRDS("predichos_mpi.Rdata")
predichos_a = readRDS("predichos_a.Rdata")
predichos_h = readRDS("predichos_h.Rdata")
# df1
predichos_mpi_df1 = readRDS("predichos_mpi_df1.Rdata")
predichos_a_df1 = readRDS("predichos_a_df1.Rdata")
predichos_h_df1 = readRDS("predichos_h_df1.Rdata")
# df13
predichos_mpi_df13 = readRDS("predichos_mpi_df13.Rdata")
predichos_a_df13 = readRDS("predichos_a_df13.Rdata")
predichos_h_df13 = readRDS("predichos_h_df13.Rdata")

 

tabla_exp1 = function(lista, df){
  results = data.frame()
  for (i in 1:10) {
    res = lista[[i]]$results
    results = rbind(results, res)
    average = sapply(results, function(x) mean(na.omit(x)))
    average$n = nrow(df)
    average$p = length(colnames(df)[!((colnames(df) %in% c(colnames(df)[1:33])))])
    average$Total.de.paises = length(unique(df$iso))

  }
  return(average)
}


#mpi
View(cbind(tabla_exp1(predichos_mpi_df1,df_1),tabla_exp1(predichos10,df),tabla_exp1(predichos_mpi_df13,df_13)))
results_mpi = data.frame(cbind(tabla_exp1(predichos_mpi_df1,df_1),tabla_exp1(predichos10,df),tabla_exp1(predichos_mpi_df13,df_13)))
colnames(results_mpi) = c("df1","df2","df13")

results_a = data.frame(cbind(tabla_exp1(predichos_a_df1,df_1),tabla_exp1(predichos_a,df),tabla_exp1(predichos_a_df13,df_13)))
colnames(results_a) = c("df1","df2","df13")

results_h = data.frame(cbind(tabla_exp1(predichos_h_df1,df_1),tabla_exp1(predichos_h,df),tabla_exp1(predichos_h_df13,df_13)))
colnames(results_h) = c("df1","df2","df13")

xtable::xtable(results_mpi, digits = 4)
xtable::xtable(results_a, digits = 4)
xtable::xtable(results_h, digits = 4)

#######################################################################
#PLOTS
#######################################################################
library(dplyr)
library(ggplot2)
 


best_methods = function(lista){
  results = data.frame()
  for (i in 1:10) {
    res = lista[[i]]$results
    results = rbind(results, res)
  }
  average = sapply(results, function(x) mean(x))
  average = average[-c(21:23)]
  best_mse = names(average[which.min(average[c(1:8)])])
  worst_mse = names(average[which.max(average[c(1:8)])])
  best_dist = names(average[c(9:17)])[which.min(average[c(9:17)])]
  worst_dist = names(average[c(9:17)])[which.max(average[c(9:17)])]
  print(paste("best mse = ",best_mse))
  print(paste("worst mse = ",worst_mse))
  print(paste("best dist = ",best_dist))
  print(paste("worst dist = ",worst_dist))
  return(average)
}

best_methods(predichos10)

graph_data = function(df, lista){
  predicted = data.frame()
  for (i in 1:10) {
    preds = lista[[i]]$predicted
    predicted = rbind(predicted, preds)
  }
  index = rownames(predicted)
  df$year_trend <- as.numeric(as.character(df$year_Other))
  df$year_trend <- df$year_trend - min(df$year_trend)
  data = df[,c(1:33,ncol(df))] #choose columns
  data = data[index,] #filter by rows
  data_graph = cbind(data,predicted) 
  names(data_graph)[names(data_graph) == 's0'] <- 'yhat.elastic_tc'
  #names(data_graph)[names(data_graph) == 's0.1'] <- 'yhat.elastic_tc'
  
  return(data_graph)
}



plot_data = function(df, division){
  
  data_plot = df
  
  # choose methods by comment
  
  data_plot$yhat.pls_tc = NULL
  data_plot$yhat.beta_tc_cr = NULL
   data_plot$yhat.beta_tc_tree_cr = NULL

  data_plot$yhat.elastic_tc = NULL
  data_plot$yhat.beta_tc_ela = NULL
  data_plot$yhat.beta_tc_tree_ela= NULL
 

  #data_plot$yhat.xgb_tc = NULL
  #data_plot$yhat.betaboost_tc = NULL

  ######################################
  
  if (division == "region"){
    data_plot = data_plot[,c(3,34:ncol(data_plot))]
    data_plot$year_trend = NULL  
    data_plot = reshape2::melt(data_plot,id.vars = "region_Other")
    

  } else {
  data_plot = data_plot[,c(1,35:ncol(data_plot))]
   data_plot = reshape2::melt(data_plot, id.vars = "iso_Other")

  }
  
  
  return(data_plot)
}


##################################################################
#PLOTS

# Preparamos los datos
df = datas[[2]]
folds = graph_data(df, predichos10)

# Densidades y vs yhat  
data_plot_all = plot_data(folds, "none")

theme_set(
  theme_light(base_size = 18) +
    theme(
      axis.line.x = element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      axis.line.y = element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = 0.5),
      panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = 0.5) ,
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 18),
      legend.position = "bottom",
      legend.text = element_text(size = 18), #
      
    )
)

densities_plot = ggplot(data_plot_all, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 
 
densities_plot + 
  labs(x = "MPI", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("Ytrue","Elastic Net","Beta (elastic)","Beta-Tree (elastic)"))+
  guides(linetype = "none")
  

# Histogramas
my_strip_labels <- as_labeller(c(
  "y_test" = "Ytrue",
  "yhat.pls_tc" = "Linear-PLS",
  "yhat.beta_tc_cr" = "Beta-PLS"
))

my_strip_labels <- as_labeller(c(
  "y_test" = "Ytrue",
  "yhat.elastic_tc" = "elastic",
  "yhat.beta_tc_ela" = "beta ela",
  "yhat.beta_tree_tc_cr" = "beta tree ela"
))


ggplot(data_plot_all, aes(value, fill = variable)) +
  geom_histogram(aes(y = after_stat(density * width)), position = "identity", alpha = 0.5, show.legend = F) +
  labs(x = "", y = "Relative Frequency", color = "")+
  xlim(-.001, 0.75) +
  facet_wrap(~ variable,
             labeller = my_strip_labels,  # add labels
             strip.position = "bottom") +
      theme(
          strip.placement = "outside",
          strip.text.x = element_text(
            size = 18, color = "black"),
          strip.background = element_blank(),
          panel.border = element_rect(fill = "transparent", # Necesario para agregar el borde
                                      color = "black", linewidth = 0.5)
          )
             



# #histograms
# #FA
# ggplot(data_plot_all, aes(x=value, fill = (variable)))+
#   geom_histogram( color='#e9ecef', alpha=0.6, position='identity')
# 
# #FR
# ggplot(data_plot_all, aes(value, fill = variable)) +
#   geom_histogram(aes(y = after_stat(density * width)),
#                  position = "identity", alpha = 0.5)
# 
# ggplot(data_plot_all, aes(value, fill = variable)) +
#   geom_histogram(aes(y = after_stat(density * width)), position = "identity", alpha = 0.5) +
#   facet_wrap(~ variable)
# #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.




# negative values on predictions
sapply(1:ncol(folds), function(x) nrow(folds[folds[,x]<0,]))
colnames(folds)[c(36,40,46,50)]

sapply(sign(folds[,36:ncol(folds)]), function(x) table(factor(x, levels=c(-1,0,1))))




#################################################
# Densidades por region  
 
data_plot_reg = plot_data(folds,"region")

ggplot(data_plot_reg, aes(x=value, color = variable)) + 
  geom_density(lwd = 1, linetype = 1) +
  facet_wrap(~region_Other, scales = "free")



################################################
#ELASTIC
################################################
#A
folds_a = graph_data(df,predichos_a)
data_plot_a = plot_data(folds_a,"none")

densities_plot_A = ggplot(data_plot_a, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_A + 
  labs(x = "A", y = "Density", color = "") +
  xlim(0.32, 1) +
  scale_color_discrete(labels = c("A-true","Elastic Net","Beta (elastic)","Beta-Tree (elastic)"))+
  guides(linetype = "none")

# cant menores a 0.33
sapply(1:ncol(folds_a), function(x) nrow(folds_a[folds_a[,x]<0.33,]))
 
#H
folds_h = graph_data(df,predichos_h)
data_plot_h = plot_data(folds_h,"none")

densities_plot_H = ggplot(data_plot_h, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_H + 
  labs(x = "H", y = "Density", color = "") +
 # xlim(0.32, 1) +
  scale_color_discrete(labels = c("H-true","Elastic Net","Beta (elastic)","Beta-Tree (elastic)"))+
  guides(linetype = "none")


#MPI  HXA


# Y-hat A*H
View(graph_data(df,predichos_a))
View(graph_data(df,predichos10))

A_hat = graph_data(df,predichos_a)
A_hat = A_hat[,35:43]
H_hat = graph_data(df,predichos_h)
H_hat = H_hat[,35:43]
MPI_hat = graph_data(df,predichos10)
MPI_hat_hxa = A_hat * H_hat
#MPI_hat_hxa$y_test = MPI_hat$y_test
# colnames(MPI_hat_hxa)[1]  = "y_test"               
# colnames(MPI_hat_hxa)[2] =  "yhat.pls_tc_hxa"          
# colnames(MPI_hat_hxa)[3] ="yhat.beta_tc_cr_hxa"      
# colnames(MPI_hat_hxa)[4] ="yhat.beta_tc_tree_cr_hxa" 
# colnames(MPI_hat_hxa)[5] ="yhat.elastic_tc_hxa"      
# colnames(MPI_hat_hxa)[6] ="yhat.beta_tc_ela_hxa"     
# colnames(MPI_hat_hxa)[7] ="yhat.beta_tc_tree_ela_hxa"
# colnames(MPI_hat_hxa)[8] ="yhat.xgb_tc_hxa"          
# colnames(MPI_hat_hxa)[9] = "yhat.betaboost_tc_hxa"
# MPI_hat_hxa$y_test = NULL
View(MPI_hat_hxa)
View(MPI_hat)

data_plot_hxa = reshape2::melt(MPI_hat_hxa[,c(1,5,6,7)])

densities_plot = ggplot(data_plot_hxa, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI (HxA)", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("HxA-true","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","a","b","c"))+
  guides(linetype = "none")


################################################
# Lineal
################################################
#A
folds_a = graph_data(df,predichos_a)
data_plot_a = plot_data(folds_a,"none")
View(data_plot_a)

densities_plot_A = ggplot(data_plot_a, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_A + 
  labs(x = "A", y = "Density", color = "") +
  xlim(0.32, 1) +
  scale_color_discrete(labels = c("A-true","Linear-PLS","Beta-PLS","Beta-Tree-PLS"))+
  guides(linetype = "none")

# cant menores a 0.33
sapply(1:ncol(folds_a), function(x) nrow(folds_a[folds_a[,x]<0.33,]))

#H
folds_h = graph_data(df,predichos_h)
data_plot_h = plot_data(folds_h,"none")
View(data_plot_h)

densities_plot_H = ggplot(data_plot_h, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_H + 
  labs(x = "H", y = "Density", color = "") +
  # xlim(0.32, 1) +
  scale_color_discrete(labels = c("H-true","Linear-PLS","Beta-PLS","Beta-Tree-PLS"))+
  guides(linetype = "none")


#MPI  HXA


# Y-hat A*H
View(graph_data(df,predichos_a))
View(graph_data(df,predichos10))

A_hat = graph_data(df,predichos_a)
A_hat = A_hat[,35:43]
H_hat = graph_data(df,predichos_h)
H_hat = H_hat[,35:43]
MPI_hat = graph_data(df,predichos10)
MPI_hat_hxa = A_hat * H_hat
#MPI_hat_hxa$y_test = MPI_hat$y_test
# colnames(MPI_hat_hxa)[1]  = "y_test"               
# colnames(MPI_hat_hxa)[2] =  "yhat.pls_tc_hxa"          
# colnames(MPI_hat_hxa)[3] ="yhat.beta_tc_cr_hxa"      
# colnames(MPI_hat_hxa)[4] ="yhat.beta_tc_tree_cr_hxa" 
# colnames(MPI_hat_hxa)[5] ="yhat.elastic_tc_hxa"      
# colnames(MPI_hat_hxa)[6] ="yhat.beta_tc_ela_hxa"     
# colnames(MPI_hat_hxa)[7] ="yhat.beta_tc_tree_ela_hxa"
# colnames(MPI_hat_hxa)[8] ="yhat.xgb_tc_hxa"          
# colnames(MPI_hat_hxa)[9] = "yhat.betaboost_tc_hxa"
# MPI_hat_hxa$y_test = NULL
View(MPI_hat_hxa)
View(MPI_hat)

data_plot_hxa = reshape2::melt(MPI_hat_hxa[,c(1,2,3,4)])
View(data_plot_hxa)

densities_plot = ggplot(data_plot_hxa, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI (HxA)", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("HxA-true","Linear-PLS","Beta-PLS","Beta-Tree-PLS"))+
  guides(linetype = "none")

# MPI

folds = graph_data(df, predichos10)
data_plot_all = plot_data(folds, "none")
View(data_plot_all)
unique(data_plot_all$variable)

densities_plot = ggplot(data_plot_all, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("Ytrue","Linear-PLS","Beta-PLS","Beta-Tree-PLS"))+
  guides(linetype = "none")



################################################
# Boost
################################################
#A
folds_a = graph_data(df,predichos_a)
data_plot_a = plot_data(folds_a,"none")
unique(data_plot_a$variable)

densities_plot_A = ggplot(data_plot_a, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_A + 
  labs(x = "A", y = "Density", color = "") +
  xlim(0.32, 1) +
  scale_color_discrete(labels = c("A-true","XGBoost","Betaboost"))+
  guides(linetype = "none")

# cant menores a 0.33
sapply(1:ncol(folds_a), function(x) nrow(folds_a[folds_a[,x]<0.33,]))

#H
folds_h = graph_data(df,predichos_h)
data_plot_h = plot_data(folds_h,"none")
unique(data_plot_h$variable)

densities_plot_H = ggplot(data_plot_h, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_H + 
  labs(x = "H", y = "Density", color = "") +
  # xlim(0.32, 1) +
  scale_color_discrete(labels = c("H-true","XGBoost","Betaboost"))+
  guides(linetype = "none")


#MPI  HXA


# Y-hat A*H
View(graph_data(df,predichos_a))
View(graph_data(df,predichos10))

A_hat = graph_data(df,predichos_a)
A_hat = A_hat[,35:43]
H_hat = graph_data(df,predichos_h)
H_hat = H_hat[,35:43]
MPI_hat = graph_data(df,predichos10)
MPI_hat_hxa = A_hat * H_hat
#MPI_hat_hxa$y_test = MPI_hat$y_test
# colnames(MPI_hat_hxa)[1]  = "y_test"               
# colnames(MPI_hat_hxa)[2] =  "yhat.pls_tc_hxa"          
# colnames(MPI_hat_hxa)[3] ="yhat.beta_tc_cr_hxa"      
# colnames(MPI_hat_hxa)[4] ="yhat.beta_tc_tree_cr_hxa" 
# colnames(MPI_hat_hxa)[5] ="yhat.elastic_tc_hxa"      
# colnames(MPI_hat_hxa)[6] ="yhat.beta_tc_ela_hxa"     
# colnames(MPI_hat_hxa)[7] ="yhat.beta_tc_tree_ela_hxa"
# colnames(MPI_hat_hxa)[8] ="yhat.xgb_tc_hxa"          
# colnames(MPI_hat_hxa)[9] = "yhat.betaboost_tc_hxa"
# MPI_hat_hxa$y_test = NULL
View(MPI_hat_hxa)
View(MPI_hat)

data_plot_hxa = reshape2::melt(MPI_hat_hxa[,c(1,8,9)])
unique(data_plot_hxa$variable)

densities_plot = ggplot(data_plot_hxa, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI (HxA)", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("HxA-true","XGBoost","Betaboost"))+
  guides(linetype = "none")

# MPI

folds = graph_data(df, predichos10)
data_plot_all = plot_data(folds, "none")
View(data_plot_all)
unique(data_plot_all$variable)

densities_plot = ggplot(data_plot_all, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("Ytrue","XGBoost","Betaboost"))+
  guides(linetype = "none")


##########################################################
#boxplots


#df1

rep_mpi_df1 = readRDS("rep_mpi_df1.Rdata")
rep_a_df1 = readRDS("rep_a_df1.Rdata")
rep_h_df1 = readRDS("rep_h_df1.Rdata")

#df2
rep_mpi_df2 = readRDS("rep_mpi_df2.Rdata")
rep_a_df2 = readRDS("rep_a_df2.Rdata")
rep_h_df2 = readRDS("rep_h_df2.Rdata")

#df13
rep_mpi_df13 = readRDS("rep_mpi_df13.Rdata")
rep_a_df13 = readRDS("rep_a_df13.Rdata")
rep_h_df13 = readRDS("rep_h_df13.Rdata")


graph_data_errors = function(lista){
  errors_df = data.frame(matrix(unlist(lista), nrow=50, byrow=F))
  colnames(errors_df) = names(lista)
  return(errors_df)
}

library(ggplot2)

theme_set(
  theme_light(base_size = 18) +
    theme(
      axis.line.x = element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      axis.line.y = element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = 0.5),
      panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = 0.5) ,
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.text = element_text(size = 18), #
      
    )
)


#MSE
#mpi -df1
errors = graph_data_errors(rep_mpi_df1)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
   geom_boxplot(aes(fill=Methods))+ 
   ylim(-0.0001, 0.03) +
   scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
   scale_fill_discrete(name = "",labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
   guides(linetype = "none")


#a - df1
errors = graph_data_errors(rep_a_df1)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  #ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#h - df1
errors = graph_data_errors(rep_h_df1)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )


#MSE
#mpi -df2
errors = graph_data_errors(rep_mpi_df2)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(name = "",labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  guides(linetype = "none")


#a - df2
errors = graph_data_errors(rep_a_df2)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.01) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#h - df1
errors = graph_data_errors(rep_h_df2)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#MSE
#mpi -df13
errors = graph_data_errors(rep_mpi_df13)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(name = "",labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  guides(linetype = "none")


#a - df13
errors = graph_data_errors(rep_a_df13)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.01) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#h - df13
errors = graph_data_errors(rep_h_df13)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.08) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )



# DISTANCE

#mpi -df1
errors = graph_data_errors(rep_mpi_df1)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 1) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(name = "",labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  guides(linetype = "none")


#a - df1
errors = graph_data_errors(rep_a_df1)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  #ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#h - df1
errors = graph_data_errors(rep_h_df1)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.75) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )


#Distance
#mpi -df2
errors = graph_data_errors(rep_mpi_df2)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 1) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(name = "",labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  guides(linetype = "none")


#a - df2
errors = graph_data_errors(rep_a_df2)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  #ylim(-0.0001, 0.01) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#h - df1
errors = graph_data_errors(rep_h_df2)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.6) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#dist
#mpi -df13
errors = graph_data_errors(rep_mpi_df13)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 1) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(name = "",labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  guides(linetype = "none")


#a - df13
errors = graph_data_errors(rep_a_df13)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
#  ylim(-0.0001, 0.01) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#h - df13
errors = graph_data_errors(rep_h_df13)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.6) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )













