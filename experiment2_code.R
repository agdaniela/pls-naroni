
source("dataframes.R")
source("splits.R")
source("kfold_cv_selection.R")
source("pls.R")
library(xgboost)
library(caret)
source("repetitions.R")
source("cv.betalasso.R")
#devtools::install_github("https://github.com/girelaignacio/penalizedbeta.git")
#########################################################
#dataframes (correr en caso de no tener la lista "datas")
datas = list()
for (i in c(1,2,5,8,9,10,13,15)){
  nombre <- paste("plsdata", i, sep="_")
  datas[[nombre]] = selectdfs(plsdata,i)
}


############################################################
# Experimento 2
############################################################



Xtrain =trainData_13
Xtest = testData_13
target = "mpi_Other" 
corte=0.2
link_phi = "log"
link_mu = "logit"


main_function_pred = function(Xtrain, Xtest , target,corte, link_phi, link_mu, distancia){
  
  predicted = data.frame()
  results = data.frame()
  nfolds <- 5
  # 
  # Xtrain$year_trend <- as.numeric(as.character(Xtrain$year_Other))
  # Xtrain$year_trend <- Xtrain$year_trend - min(Xtrain$year_trend)
  # 
  # Xtest$year_trend <- as.numeric(as.character(Xtest$year_Other))
  # Xtest$year_trend <- Xtest$year_trend - min(Xtest$year_trend)
  # 
  
  Xtrain$year_trend <- as.numeric(as.character(Xtrain$year))
  Xtrain$year_trend <- Xtrain$year_trend - min(Xtrain$year)
  
  Xtest$year_trend <- as.numeric(as.character(Xtest$year))
  Xtest$year_trend <- Xtest$year_trend - min(Xtest$year)
  
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
  ytest_pred.xgb_tc <- predict(hyperparam_xgb_tc$xgb.model, newdata_tc_sr)
  #ytest_pred.xgb_tc <- tryCatch(predict(hyperparam_xgb_tc$xgb.model, newdata_tc_sr), error= function(e) {return(NA)}  )
  
   # distance
  dist_xgb_tc = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.xgb_tc)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  # 3.2 Beta Boost.
  # Fit
  #hyper = kfoldCV.betaboost(data_tc_sr, nfolds)
  formu_boost_tc = as.formula(paste("ytrain", paste(names(data_tc_sr)[-1], collapse=" + "), sep=" ~ ")) 
  #betaboost.fit_tc <- mboost::blackboost(formu_boost_tc , data = data.frame(ytrain,data_tc_sr), family = betaboost::BetaReg(),
   #                                   control = mboost::boost_control(mstop = 200))
                                      #tree_controls = partykit::ctree_control(maxdepth =  3))
  
  
  
  
  #betaboost.fit_tc <- tryCatch(mboost::glmboost(formu_boost_tc, data = data.frame(ytrain,data_tc_sr), family = betaboost::BetaReg()), error= function(e) {return(NA)}  )
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
    "dist betaboost_tc" = dist_betaboost_tc
    
   
    
    
    #"n" = nrow(df),  
    #"p"= length(colnames(df)[!((colnames(df) %in% c(colnames(df)[1:33])))]),
    #"Total de paises" = length(unique(df$iso))
    
    
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

df_fold_2  = datas[[2]] 

df_fold_2  <- df_fold_2[sample(nrow(df_fold_2)),]

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
################:#######################################################
datas = list()
for (i in c(13)){
  nombre <- paste("plsdata", i, sep="_")
  datas[[nombre]] = selectdfs(plsdata,i)
}


df_fold_13  = datas[[1]] 
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

predichos_df13_nuevo = list()
for(i in 1:10){
  testIndexes_13_n <- which(folds_index_13 == i,arr.ind=TRUE)
  testData_13_n <- df_fold_13[testIndexes_13_n, ]
  trainData_13_n<- df_fold_13[-testIndexes_13_n, ]
  nombre_n <- paste("yhats", i, sep="_")
  predichos_df13_nuevo[[nombre_n]] = main_function_pred(trainData_13_n,testData_13_n,"mpi_Other",corte=0.2, link_phi = "log", link_mu = "logit", distancia = "hellinger")
}
View(predichos_df13[["yhats_"]]$predicted)


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

View(predichos_a_df13)

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


View(predichos_h_df13)

saveRDS(predichos_h_df13,"predichos_h_df13.Rdata")
readRDS("predichos_h_df13.Rdata")





  
#  summarise(count=n())




