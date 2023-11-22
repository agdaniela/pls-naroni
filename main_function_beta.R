source("dataframes.R")
source("splits.R")
source("kfold_cv_selection.R")
source("pls.R")
library(xgboost)
library(caret)
source("repetitions.R")
 
#########################################################
#dataframes (correr la primera vez sin comentar)
# datas = list()
# for (i in c(1,2,5,8,9,10,13,15)){
# nombre <- paste("plsdata", i, sep="_")
# datas[[nombre]] = selectdfs(plsdata,i)
# }
# 

#########################################################
df = datas[[2]]; target = "mpi_Other"


 
main_function_tcyd = function(df, target, d, link_phi, link_mu, distancia){
  
  #  
  # 
  results = data.frame()
  nfolds <- 5
  
  df$year_trend <- as.numeric(as.character(df$year_Other))
  df$year_trend <- df$year_trend - min(df$year_trend)
  
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
  
  data <- as.data.frame(cbind(ytrain, Xtrain))
  
  ##########################################
  # tiempo discreto
  Xtrain_td = subset(Xtrain, select=-c(year_trend))
  Xtest_td = subset(Xtest, select=-c(year_trend))
  
  # reduction with d chosen 
  pls.projections_td_de <- chemometrics::pls1_nipals(Xtrain_td, ytrain, a = d, scale = FALSE)
  data_td_de <- as.data.frame(cbind(ytrain, as.matrix(Xtrain_td) %*% pls.projections_td_de$W))
  newdata_td_de <- data.frame(as.matrix(Xtest_td) %*% pls.projections_td_de$W)
  colnames(newdata_td_de) <- colnames(data_td_de)[-1]
  
  #Beta-regression 
  data_beta_td_de = data.frame(data_td_de, Ttrain, Rtrain)
  formu = as.formula(paste("ytrain", paste(names(data_beta_td_de)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_td <- tryCatch(betareg::betareg(formu , data = data_beta_td_de, link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
  
  # Predict over Xtest
  newdata_beta_td_de = data.frame(newdata_td_de,Ttest,Rtest)
  ytest_pred.beta_td_de = tryCatch(predict(beta.fit_td, newdata_beta_td_de), error= function(e) {return(NA)}  )
  # distance
  dist_beta_td_de = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_td_de)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
    
  #elastic net 
  data_ela_td = data.frame(Xtrain_td,Ttrain, Rtrain)
  hyperparam <- kfoldCV.elastic(data_ela_td, ytrain, nfolds)
  newdata_ela_td <- data.frame(Xtest_td,Ttest,Rtest)
  elastic.fit_td <- glmnet::glmnet(x = data_ela_td, y = ytrain, family = "gaussian", alpha = hyperparam$best.alpha, lambda = hyperparam$best.lambda)
  
  # Predict over Xtest
  ytest_pred.elastic_td <- glmnet::predict.glmnet(elastic.fit_td, as.matrix(newdata_ela_td))
  # distance
  dist_elastic_td = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.elastic_td)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  # beta con las seleccionadas por elastic
  tmp_coeffs_td <- coef(elastic.fit_td, s = "lambda.min")
  var_selected_td = tmp_coeffs_td@Dimnames[[1]][tmp_coeffs_td@i +1]
  Xtrain_td_ela = Xtrain_td[,colnames(Xtrain_td) %in% var_selected_td]
  Xtest_td_ela <- Xtest_td[,colnames(Xtest_td) %in% var_selected_td]
  
  #Beta con d chosen
 
  pls.projections_td_ela_de <- chemometrics::pls1_nipals(Xtrain_td_ela, ytrain, a = d, scale = FALSE)
  data_td_ela_de <- as.data.frame(cbind(ytrain, as.matrix(Xtrain_td_ela) %*% pls.projections_td_ela_de$W))
  newdata_td_ela_de <- data.frame(as.matrix(Xtest_td_ela) %*% pls.projections_td_ela_de$W)
  colnames(newdata_td_ela_de) <- colnames(data_td_ela_de)[-1]
  
  #Beta-regression 
  data_beta_td_ela_de = data.frame(data_td_ela_de, Ttrain, Rtrain)
  formu_td_ela_de = as.formula(paste("ytrain", paste(names(data_beta_td_ela_de)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_td_ela_de <- tryCatch(betareg::betareg(formu_td_ela_de , data = data_beta_td_ela_de, link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
  
  # Predict over Xtest
  newdata_beta_td_ela_de = data.frame(newdata_td_ela_de,Ttest,Rtest)
  ytest_pred.beta_td_ela_de = tryCatch(predict(beta.fit_td_ela_de, newdata_beta_td_ela_de), error= function(e) {return(NA)}  )
  # distance
  dist_beta_td_ela_de = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_td_ela_de)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  # Beta con todas 
  
  #Beta-regression 
  data_beta_td_ela_sr = data.frame(Xtrain_td_ela, Ttrain, Rtrain)
  formu_td_ela_sr = as.formula(paste("ytrain", paste(names(data_beta_td_ela_sr)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_td_ela_sr <- tryCatch(betareg::betareg(formu_td_ela_sr, data = data_beta_td_ela_sr, link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
  
  # Predict over Xtest
  newdata_beta_td_ela_sr = data.frame(Xtest_td_ela,Ttest,Rtest)
  ytest_pred.beta_td_ela_sr = tryCatch(predict(beta.fit_td_ela_sr, newdata_beta_td_ela_sr), error= function(e) {return(NA)}  )
  # distance
  dist_beta_td_ela_sr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_td_ela_sr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  
  ##################################################################################
  # tiempo continuo
  
  
  # reduction d chosen
  pls.projections_tc_de <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d, scale = FALSE)
  data_tc_de <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections_tc_de$W))
  newdata_tc_de <- data.frame(as.matrix(Xtest) %*% pls.projections_tc_de$W)
  colnames(newdata_tc_de) <- colnames(data_tc_de)[-1]
  #Beta-regression d chosen
  data_beta_tc_de = data.frame(data_tc_de , Rtrain)
  formu_tc_de = as.formula(paste("ytrain", paste(names(data_beta_tc_de)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_tc_de <- tryCatch(betareg::betareg(formu_tc_de, data = data_beta_tc_de, link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
  
  # Predict over Xtest
  newdata_beta_tc_de = data.frame(newdata_tc_de,Rtest)
  ytest_pred.beta_tc_de = tryCatch(predict(beta.fit_tc_de, newdata_beta_tc_de), error= function(e) {return(NA)}  )
  # distance
  dist_beta_tc_de = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_de)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  #elastic net 
  data_ela_tc = data.frame(Xtrain, Rtrain)
  hyperparam <- kfoldCV.elastic(data_ela_tc, ytrain, nfolds)
  newdata_ela_tc <- data.frame(Xtest, Rtest)
  elastic.fit_tc <- glmnet::glmnet(x = data_ela_tc, y = ytrain, family = "gaussian", alpha = hyperparam$best.alpha, lambda = hyperparam$best.lambda)
  # Predict over Xtest
  ytest_pred.elastic_tc <- glmnet::predict.glmnet(elastic.fit_tc, as.matrix(newdata_ela_tc))
  # distance
  dist_elastic_tc = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.elastic_tc)$y), est.prob = "empirical", method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  
   # beta con las seleccionadas por elastic - tiempo continuo
  tmp_coeffs_tc <- coef(elastic.fit_tc, s = "lambda.min")
  var_selected_tc = tmp_coeffs_tc@Dimnames[[1]][tmp_coeffs_tc@i +1]
  
  Xtrain_tc_ela = Xtrain[,colnames(Xtrain) %in% var_selected_tc]
  Xtest_tc_ela <- Xtest[,colnames(Xtest) %in% var_selected_tc]
  
    
  #Beta con d chosen
   pls.projections_tc_ela_de <- chemometrics::pls1_nipals(Xtrain_tc_ela, ytrain, a = d, scale = FALSE)
  data_tc_ela_de <- as.data.frame(cbind(ytrain, as.matrix(Xtrain_tc_ela) %*% pls.projections_tc_ela_de$W))
  newdata_tc_ela_de <- data.frame(as.matrix(Xtest_tc_ela) %*% pls.projections_tc_ela_de$W)
  colnames(newdata_tc_ela_de) <- colnames(data_tc_ela_de)[-1]
  
  #Beta-regression 
  data_beta_tc_ela_de = data.frame(data_tc_ela_de, Rtrain)
  formu_tc_ela_de = as.formula(paste("ytrain", paste(names(data_beta_tc_ela_de)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_tc_ela_de <- tryCatch(betareg::betareg(formu_tc_ela_de , data = data_beta_tc_ela_de,  link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
  
  # Predict over Xtest
  newdata_beta_tc_ela_de = data.frame(newdata_tc_ela_de, Rtest)
  ytest_pred.beta_tc_ela_de = tryCatch(predict(beta.fit_tc_ela_de, newdata_beta_tc_ela_de), error= function(e) {return(NA)}  )
  # distance
  dist_beta_tc_ela_de = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_ela_de)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  
  # Beta con todas 
  
  #Beta-regression 
  data_beta_tc_ela_sr = data.frame(Xtrain_tc_ela, Rtrain)
  formu_tc_ela_sr = as.formula(paste("ytrain", paste(names(data_beta_tc_ela_sr)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_tc_ela_sr <- tryCatch(betareg::betareg(formu_tc_ela_sr , data = data_beta_tc_ela_sr, link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
  
  # Predict over Xtest
  newdata_beta_tc_ela_sr = data.frame(Xtest_tc_ela, Rtest)
  ytest_pred.beta_tc_ela_sr = tryCatch(predict(beta.fit_tc_ela_sr, newdata_beta_tc_ela_sr), error= function(e) {return(NA)}  )
  # distance
  dist_beta_tc_ela_sr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_ela_sr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  ####################################
  # beta tree
  dummy_corte_train = ifelse(ytrain<=0.2, 1, 0)
  dummy_corte_test = as.vector(ifelse(ytest<=0.2, 1, 0))

   
  # d chosen
  data_beta_tc_tree_de = data.frame(data_beta_tc_de, Rtrain, dummy_corte_train)
  beta.fit_tc_tree_de <- tryCatch(betareg::betatree(formu_tc_de, ~ dummy_corte_train, data = data_beta_tc_tree_de ,  link.phi = link_phi, link = link_mu ), error= function(e) {return(NA)}  )
  #predict
  newdata_beta_tc_tree_de = data.frame(newdata_tc_de, Rtest, dummy_corte_test)
  names(newdata_beta_tc_tree_de)[length(names(newdata_beta_tc_tree_de))]  = "dummy_corte_train"
  ytest_pred.beta_tc_tree_de =  predict(beta.fit_tc_tree_de, newdata_beta_tc_tree_de)
  # distance
  dist_beta_tc_tree_de = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_tree_de)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ) , error= function(e) {return(NA)}  )
  
 
  # elegidas por elastic, sin reducir
  
  # beta tree
  data_beta_tc_tree_ela_sr = data.frame(data_beta_tc_ela_sr, dummy_corte_train)
  formu_tc_tree_ela_sr = as.formula(paste("ytrain", paste(names(data_beta_tc_tree_ela_sr)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_tc_tree_ela_sr <- tryCatch(betareg::betatree(formu_tc_tree_ela_sr, ~ dummy_corte_train, data = data_beta_tc_tree_ela_sr,  link.phi = link_phi, link = link_mu   ), error= function(e) {return(NA)}  )
  
  # predict 
  newdata_beta_tc_tree_ela_sr = data.frame(newdata_beta_tc_ela_sr, dummy_corte_test )
  names(newdata_beta_tc_tree_ela_sr)[length(names(newdata_beta_tc_tree_ela_sr))]  = "dummy_corte_train"
  ytest_pred.beta_tc_tree_sr =  predict(beta.fit_tc_tree_ela_sr, newdata_beta_tc_tree_ela_sr)
  
  # distance
  dist_beta_tc_tree_ela_sr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_tree_sr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  # de otra forma:
  # dist_beta_tc_tree_ela_sr = as.numeric(philentropy::distance(rbind(ytest, ytest_pred.beta_tc_tree_sr), est.prob = "empirical", method = "hellinger"))
 
  #########################################################################
 
  # Beta boost
  # tiempo discreto
  # Xtrain_td ; Xtest_td
  # data_ela_td = data.frame(Xtrain_td,Ttrain, Rtrain)
  
  # Fit
  formu_boost_td = as.formula(paste("ytrain", paste(names(data_ela_td )[-1], collapse=" + "), sep=" ~ ")) 
  betaboost.fit_td <- mboost::glmboost(formu_boost_td, data = data_ela_td, family = betaboost::BetaReg())
  
  # con otros parametros
  #betaboost.fit_td <- mboost::glmboost(formu_boost_td, data = data_ela_td, family = betaboost::BetaReg(), control = mboost::boost_control(mstop = 120, nu = 0.01))
  
  # Predict over Xtest
  # newdata_ela_td <- data.frame(Xtest_td,Ttest,Rtest)
  ytest_pred.betaboost_td = predict(betaboost.fit_td, newdata = newdata_ela_td, type = "response")
  dist_betaboost_td  = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.betaboost_td)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  #tiempo continuo 
  
  # fit
  #data_ela_tc = data.frame(Xtrain, Rtrain)
  formu_boost_tc = as.formula(paste("ytrain", paste(names(data_ela_tc)[-1], collapse=" + "), sep=" ~ ")) 
  betaboost.fit_tc <- mboost::glmboost(formu_boost_tc, data = data_ela_tc, family = betaboost::BetaReg())
  # con otros parametros
  #betaboost.fit_tc <- mboost::glmboost(formu_boost_tc, data = data_ela_tc, family = betaboost::BetaReg(), control = mboost::boost_control(mstop = 120, nu = 0.01))
  
  # Predict over Xtest
  #newdata_ela_tc <- data.frame(Xtest, Rtest)
  ytest_pred.betaboost_tc = predict(betaboost.fit_tc, newdata = newdata_ela_tc, type = "response")
  dist_betaboost_tc  = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.betaboost_tc)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  
  ########################################################################3
  #  # lo de rodri
 #  ycat<-ifelse(y>=0.2,1,0)
 # 
 #  fitclass<-cv.glmnet(as.matrix(X), ycat, family = "binomial", type.measure = "class")
 # 
 #  fitclass = glmnet(as.matrix(X), as.matrix(ycat),
 #                    lambda=cv.glmnet(as.matrix(X), as.matrix(ycat))$lambda.1se)
 # 
 #  selected_vars <- as.matrix(fitclass$beta)>0
 # 
 # 
 # 
 # #predict
 #  newdata_beta_tc_tree_de = data.frame(newdata_tc_de, Rtest, dummie_corte_test)
 #  ytest_pred.beta_tc_de = tryCatch(predict(beta.fit_tc_tree_de, newdata_beta_tc_tree_de), error= function(e) {return(NA)}  )
 # 



  # Saving results
  results = data.frame( 
    "MSE elastic_td" = mean((ytest-ytest_pred.elastic_td)^2),
    "MSE elastic_tc" = mean((ytest-ytest_pred.elastic_tc)^2),
    
    
    "MSE beta_td_de" = mean((ytest-ytest_pred.beta_td_de)^2),
    "MSE beta_tc_de" = mean((ytest-ytest_pred.beta_tc_de)^2),
     
    "MSE beta_td_ela_de" = mean((ytest-ytest_pred.beta_td_ela_de)^2),
    "MSE beta_td_ela_sr" = mean((ytest-ytest_pred.beta_td_ela_sr)^2),
    
    "MSE beta_tc_ela_de" = mean((ytest-ytest_pred.beta_tc_ela_de)^2),
    "MSE beta_tc_ela_sr" = mean((ytest-ytest_pred.beta_tc_ela_sr)^2),
  
    "MSE beta_tc_tree_de" = mean((ytest-ytest_pred.beta_tc_tree_de)^2),
    "MSE beta_tc_tree_ela_sr" = mean((ytest-ytest_pred.beta_tc_tree_sr)^2),
    
    "MSE betaboost_td" = mean((ytest-ytest_pred.betaboost_td)^2),
    "MSE betaboost_tc" = mean((ytest-ytest_pred.betaboost_tc)^2),
    
    # distancias
    "dist elastic_tc" = dist_elastic_tc,
    "dist elastic_td" = dist_elastic_td,

    "dist beta_td_de" = dist_beta_td_de,
    "dist beta_tc_de" = dist_beta_tc_de,
    
    "dist beta_td_ela_de" = dist_beta_td_ela_de,
    "dist beta_td_ela_sr" = dist_beta_td_ela_sr,
    
    "dist beta_tc_ela_de" = dist_beta_tc_ela_de,
    "dist beta_tc_ela_sr" = dist_beta_tc_ela_sr,
    
    "dist beta_tc_tree_de" = dist_beta_tc_tree_de,
    "dist beta_tc_tree_ela_sr" = dist_beta_tc_tree_ela_sr,
    
    "dist betaboost_td" = dist_betaboost_td,
    "dist betaboost_tc" = dist_betaboost_tc,
    
    
    "n" = nrow(df),  
    "p"= length(colnames(df)[!((colnames(df) %in% c(colnames(df)[1:33])))]),
    "Total de paises" = length(unique(df$iso))
    
    
  )
  
  return(results)
  
}

main_function_tcyd(df,"mpi_Other", d=1, link_phi = "log",link_mu = "logit", distancia = "hellinger")

parts_10 = repetitions(df,"mpi_Other",d=2,link_phi = "log", link_mu = "logit",distancia = "hellinger",nreps=10)

parts_10_d4 = repetitions2(df,"mpi_Other",d=4,link_phi = "log", link_mu = "logit",distancia = "hellinger",nreps=10)

lapply(parts_10, function(x) mean(na.omit(x)))


#plot(ytest[ytest<0.2], ytest_pred.beta_tc_tree_sr[ytest<0.2] ); abline(0,1)
#plot(ytest[ytest<0.2], ytest_pred.elastic_tc[ytest<0.2] ); abline(0,1)



