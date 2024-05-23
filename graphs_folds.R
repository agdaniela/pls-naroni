# Crear 5 folds disjuntos - predecir en uno

Xtrain =trainData
Xtest = testData
main_function_pred = function(Xtrain, Xtest , target, d, link_phi, link_mu, distancia){
  
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
  #data <- as.data.frame(cbind(ytrain, Xtrain))
  # data <- as.data.frame(cbind(ytrain, Xtrain))
  
  ########################################## ----
  # # Discrete (dummy) time
  # Xtrain_td = subset(Xtrain, select=-c(year_trend))
  # Xtest_td = subset(Xtest, select=-c(year_trend))
  # 
  # # 1.  Using dimension reduction
  # 
  # #if (missing(d_pls) & missing(d_plsbeta) ) {
  # # if no argument for d is provided, then take optimal d
  # pls.directions = 30 
  # hyperparam <- kfoldCV.pls(Xtrain_td, ytrain, nfolds, pls.directions)
  # hyperparam_beta <- kfoldCV.plsbeta(Xtrain_td, ytrain, nfolds, pls.directions)
  # 
  # d_pls = hyperparam$d.min
  # d_plsbeta = hyperparam_beta$d.min
  # #  } else{
  # # If d is provided, take it for all DR
  # #   d_pls = d_pls
  # #    d_plsbeta = d_plsbeta
  # # }
  # 
  # # 1.1 PLS with optimal $d$ using linear predictor. 
  # 
  # # Data  
  # pls.projections_td_cr <- chemometrics::pls1_nipals(Xtrain_td, ytrain, a = d_pls, scale = FALSE)
  # data_td_cr <- as.data.frame(cbind(ytrain, as.matrix(Xtrain_td) %*% pls.projections_td_cr$W))
  # newdata_td_cr <- data.frame(as.matrix(Xtest_td) %*% pls.projections_td_cr$W)
  # colnames(newdata_td_cr) <- colnames(data_td_cr)[-1]
  # 
  # # Train and test data with dimension reduction and dummies of time and region - formula of fit
  # data_td_cr_dum = data.frame(data_td_cr, Ttrain, Rtrain)
  # formu_td_cr = as.formula(paste("ytrain", paste(names(data_td_cr_dum)[-1], collapse=" + "), sep=" ~ ")) 
  # newdata_td_cr_dum = data.frame(newdata_td_cr,Ttest,Rtest)
  # 
  # # Fit
  # pls.fit <- lm(ytrain ~ . , data_td_cr_dum)
  # # Predict over Xtest
  # ytest_pred.pls_td  <- tryCatch(predict(pls.fit, newdata_td_cr_dum), error= function(e) {return(NA)}  )
  # # Distance
  # dist_pls_td = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.pls_td)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  # 
  # # 1.2 PLS with fixed $d=1$ using non-parametric regression.
  # 
  # pls.projections_td_d1 <- chemometrics::pls1_nipals(Xtrain_td, ytrain, a = 1, scale = FALSE)
  # data_td_d1 <- as.data.frame(cbind(ytrain, as.matrix(Xtrain_td) %*% pls.projections_td_d1$W))
  # newdata_td_d1 <- data.frame(as.matrix(Xtest_td) %*% pls.projections_td_d1$W)
  # colnames(newdata_td_d1) <- colnames(data_td_d1)[-1]
  # # Fit
  # formu_td_d1 = as.formula(paste("ytrain", paste(names(data_td_d1)[-1], collapse=" + "), sep=" ~ ")) 
  # bw_td_d1 <- np::npregbw(formu_td_d1, data=data_td_d1) 
  # np_PLS_td_d1 <- np::npreg(bw_td_d1)
  # # Predict over Xtest
  # ytest_pred.np_td_d1 = tryCatch(predict(np_PLS_td_d1, newdata=newdata_td_d1, type="response"), error= function(e) {return(NA)}  )
  # # Distance
  # dist_pls_np_td = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.np_td_d1)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  # 
  # # 1.3 PLS with optimal $d$ using beta regression (For hyperparameter, training with the prediction model)
  # pls.projections_td_dbeta <- chemometrics::pls1_nipals(Xtrain_td, ytrain, a = d_plsbeta, scale = FALSE)
  # data_td_dbeta <- as.data.frame(cbind(ytrain, as.matrix(Xtrain_td) %*% pls.projections_td_dbeta$W))
  # newdata_td_dbeta <- data.frame(as.matrix(Xtest_td) %*% pls.projections_td_dbeta$W)
  # colnames(newdata_td_dbeta) <- colnames(data_td_dbeta)[-1]
  # 
  # # Fit
  # formu_td_dbeta = as.formula(paste("ytrain", paste(names(data_td_dbeta)[-1], collapse=" + "), sep=" ~ ")) 
  # beta.fit_td <- tryCatch(betareg::betareg(formu_td_dbeta , data = data_td_dbeta, link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
  # # Predict over Xtest
  # ytest_pred.beta_td_cr = tryCatch(betareg::predict(beta.fit_td, newdata_td_dbeta, type = "response"), error= function(e) {return(NA)}  )
  # # Distance
  # dist_beta_td_cr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_td_cr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  # 
  # # 1.4 PLS with optimal $d$ using beta tree model for prediction.
  # 
  # dummy_corte_train = ifelse(ytrain<=0.2, 1, 0)
  # dummy_corte_test = as.vector(ifelse(ytest<=0.2, 1, 0))
  # # Fit
  # data_beta_td_tree_cr = data.frame(data_td_dbeta, dummy_corte_train)
  # formu_td_dbeta_tree = as.formula(paste("ytrain", paste(names(data_beta_td_tree_cr)[-1], collapse=" + "), sep=" ~ ")) 
  # 
  # beta.fit_td_tree_cr <- tryCatch(betareg::betatree(formu_td_dbeta_tree, ~ dummy_corte_train, data = data_beta_td_tree_cr ,  link.phi = link_phi, link = link_mu ))
  # #predict
  # newdata_beta_td_tree_cr = data.frame(newdata_td_dbeta, dummy_corte_test)
  # names(newdata_beta_td_tree_cr)[length(names(newdata_beta_td_tree_cr))]  = "dummy_corte_train"
  # ytest_pred.beta_td_tree_cr =  tryCatch(betareg::predict(beta.fit_td_tree_cr, newdata_beta_td_tree_cr, type = "response"), error= function(e) {return(NA)}  )
  # # distance
  # dist_beta_td_tree_cr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_td_tree_cr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ) , error= function(e) {return(NA)}  )
  # 
  # 
  # # 2. Based on Regularized Regresion/Variable Selection
  # 
  # # Data without dimension reduction
  # data_td_sr = data.frame(Xtrain_td,Ttrain, Rtrain)
  # newdata_td_sr <- data.frame(Xtest_td,Ttest,Rtest)
  # 
  # # 2.1 Elastic Net.
  # #Fit
  # hyperparam <- kfoldCV.elastic(data_td_sr, ytrain, nfolds)
  # elastic.fit_td <- glmnet::glmnet(x = data_td_sr, y = ytrain, family = "gaussian", alpha = hyperparam$best.alpha, lambda = hyperparam$best.lambda)
  # # Predict over Xtest
  # ytest_pred.elastic_td <- glmnet::predict.glmnet(elastic.fit_td, as.matrix(newdata_td_sr))
  # 
  # # distance
  # dist_elastic_td = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.elastic_td)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  # 
  # # 2.2 Beta regression using selected covariates with elastic net.
  # tmp_coeffs_td <- coef(elastic.fit_td, s = "lambda.min")
  # var_selected_td = tmp_coeffs_td@Dimnames[[1]][tmp_coeffs_td@i +1]
  # Xtrain_td_ela = Xtrain_td[,colnames(Xtrain_td) %in% var_selected_td]
  # Xtest_td_ela <- Xtest_td[,colnames(Xtest_td) %in% var_selected_td]
  # # Fit
  # data_beta_td_ela = data.frame(Xtrain_td_ela, Rtrain)
  # formu_td_ela = as.formula(paste("ytrain", paste(names(data_beta_td_ela)[-1], collapse=" + "), sep=" ~ ")) 
  # beta.fit_td_ela <- tryCatch(betareg::betareg(formu_td_ela, data = data_beta_td_ela, link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
  # # Predict over Xtest
  # newdata_beta_td_ela = data.frame(Xtest_td_ela,Ttest,Rtest)
  # ytest_pred.beta_td_ela = tryCatch(betareg::predict(beta.fit_td_ela, newdata_beta_td_ela, type = "response"), error= function(e) {return(NA)}  )
  # # distance
  # dist_beta_td_ela = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_td_ela)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  # 
  # # 2.3 Beta tree using selected covariates with elastic net.
  # # Fit
  # data_beta_td_tree_ela = data.frame(data_beta_td_ela, dummy_corte_train)
  # formu_td_tree_ela = as.formula(paste("ytrain", paste(names(data_beta_td_tree_ela)[-1], collapse=" + "), sep=" ~ "))
  # beta.fit_td_tree_ela <- tryCatch(betareg::betatree(formu_td_tree_ela, ~ dummy_corte_train, data = data_beta_td_tree_ela,  link.phi = link_phi, link = link_mu   ), error= function(e) {return(NA)}  )
  # # predict
  # newdata_beta_td_tree_ela = data.frame(newdata_beta_td_ela, dummy_corte_test )
  # names(newdata_beta_td_tree_ela)[length(names(newdata_beta_td_tree_ela))]  = "dummy_corte_train"
  # ytest_pred.beta_td_tree_ela =  tryCatch(betareg::predict(beta.fit_td_tree_ela, newdata_beta_td_tree_ela, type = "response"), error= function(e) {return(NA)}  )
  # # distance
  # dist_beta_td_tree_ela = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_td_tree_ela)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  # 
  # # 2.3 Beta lasso  
  # 
  # #Fit
  # hyperparam <- kfoldCV.betalasso(data_td_sr, ytrain, nfolds)
  # betalasso.fit_td <- penalizedbeta::betareg_lasso(X = data_td_sr, y = ytrain, lambda = hyperparam$s.min)
  # # Predict over Xtest
  # ytest_pred.betalasso_td <- predict(betalasso.fit_td, as.matrix(newdata_td_sr))
  # # distance
  # dist_betalasso_td = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.betalasso_td)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  # 
  # # 3 Based on Model Selection
  # 
  # # 3.1 XGBoost lineal.
  # #Fit
  # hyperparam_xgb_td = kfoldCV.xgboost(data_td_sr, ytrain, nfolds)
  # # Predict over Xtest
  # ytest_pred.xgb_td <- tryCatch(predict(hyperparam_xgb_td$xgb.model, newdata_td_sr), error= function(e) {return(NA)}  )
  # # distance
  # dist_xgb_td = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.xgb_td)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  # 
  # # 3.2 Beta Boost.
  # # Fit
  # formu_boost_td = as.formula(paste("ytrain", paste(names(data_td_sr )[-1], collapse=" + "), sep=" ~ ")) 
  # betaboost.fit_td <- tryCatch(mboost::glmboost(formu_boost_td, data = data_td_sr, family = betaboost::BetaReg()), error= function(e) {return(NA)}  )
  # # Predict over Xtest
  # ytest_pred.betaboost_td = predict(betaboost.fit_td, newdata = newdata_td_sr, type = "response")
  # dist_betaboost_td  = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.betaboost_td)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  # 
  # 
  ##################################################################################################### ----
  # Continuous time
  
  # 1.  Using dimension reduction
  
  hyperparam <- kfoldCV.pls(Xtrain, ytrain, nfolds, pls.directions)
  hyperparam_beta <- kfoldCV.plsbeta(Xtrain, ytrain, nfolds, pls.directions)
  d_pls = hyperparam$d.min
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
  dummy_corte_train = ifelse(ytrain<=0.2, 1, 0)
  dummy_corte_test = as.vector(ifelse(ytest<=0.2, 1, 0))
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
  beta.fit_tc_tree_ela <- tryCatch(betareg::betatree(formu_tc_tree_ela, ~ dummy_corte_train, data = data_beta_tc_tree_ela,  link.phi = link_phi, link = link_mu   ), error= function(e) {return(NA)}  )
  # predict
  newdata_beta_tc_tree_ela = data.frame(newdata_beta_tc_ela, dummy_corte_test )
  names(newdata_beta_tc_tree_ela)[length(names(newdata_beta_tc_tree_ela))]  = "dummy_corte_train"
  ytest_pred.beta_tc_tree_ela =  tryCatch(predict(beta.fit_tc_tree_ela, newdata_beta_tc_tree_ela))
  # distance
  dist_beta_tc_tree_ela = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_tree_ela)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )

  # 2.3 Beta lasso  
  #Fit
  hyperparam <- kfoldCV.betalasso(data_tc_sr, ytrain, nfolds)
  betalasso.fit_tc <- tryCatch(penalizedbeta::betareg_lasso(X = data_tc_sr, y = ytrain, lambda = hyperparam$s.min), error= function(e) {return(NA)}  )
  # Predict over Xtest
  ytest_pred.betalasso_tc <- tryCatch(predict(betalasso.fit_tc, as.matrix(newdata_tc_sr)), error= function(e) {return(NA)}  )
  # distance
  dist_betalasso_tc = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.betalasso_tc)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
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



#Randomly shuffle the data
df_fold  = datas[[2]] 

df_fold  <- df_fold[sample(nrow(df_fold)),]

#Perform 5 fold cross validation
folds_index <- cut(seq(1,nrow(df_fold)),breaks=5,labels=FALSE) #indices

predichos = list()
for(i in 1:5){
  testIndexes <- which(folds_index == i,arr.ind=TRUE)
  testData <- df_fold[testIndexes, ]
  trainData <- df_fold[-testIndexes, ]
  nombre <- paste("yhats", i, sep="_")
  predichos[[nombre]] = main_function_pred(trainData,testData,"mpi_Other", d=1, link_phi = "log", link_mu = "logit", distancia = "hellinger")
}


View(predichos[["yhats_1"]]$predicted)
View(predichos[["yhats_2"]]$predicted) #ok
View(predichos[["yhats_3"]]$predicted) #ok
View(predichos[["yhats_4"]]$predicted) #ok
View(predichos[["yhats_5"]]$predicted) #ok

View(predichos$yhats_1$results)
View(predichos$yhats_2$errors)

saveRDS(predichos,"graphs_predichos_0.Rdata") #4cols + tc

#predichos = readRDS("graphs_predichos_0.Rdata")
# saveRDS(predichos,"graphs_predichos_1.Rdata")#tc
# saveRDS(predichos,"graphs_predichos_2.Rdata") #tc y td
# saveRDS(predichos,"graphs_predichos_3.Rdata") #boost

# View(predichos_0[["yhats_1"]]$predicted)
# View(predichos_0[["yhats_2"]]$predicted)
# View(predichos_0[["yhats_3"]]$predicted)
# View(predichos_0[["yhats_4"]]$predicted)
# View(predichos_0[["yhats_5"]]$predicted)



#Graficos
# df = datas[[2]]
#predichos_0 = readRDS("graphs_predichos_0.Rdata")
# predichos_0[["yhats_5"]]$results

# cambiar los indices en los fors!!!!!! 
library(dplyr)
library(ggplot2)
 
theme_set(
  theme_light() +
      theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        panel.border = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12), #legend.position = "top"
        )
)

best_methods = function(lista){
  results = data.frame()
  for (i in 1:5) {
    res = lista[[i]]$results
    results = rbind(results, res)
  }
  average = sapply(results, function(x) mean(x))
  average = average[-c(21:23)]
  best_mse = names(average[which.min(average[c(1:10)])])
  worst_mse = names(average[which.max(average[c(1:10)])])
  best_dist = names(average[c(11:20)])[which.min(average[c(11:20)])]
  worst_dist = names(average[c(11:20)])[which.max(average[c(11:20)])]
  print(paste("best mse = ",best_mse))
  print(paste("worst mse = ",worst_mse))
  print(paste("best dist = ",best_dist))
  print(paste("worst dist = ",worst_dist))
  return(average)
}

best_methods(predichos)

graph_data = function(df, lista){
  predicted = data.frame()
  for (i in 1:5) {
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
  
 # data_plot$yhat.pls_tc = NULL
 # data_plot$yhat.beta_tc_cr = NULL
   data_plot$yhat.beta_tc_tree_cr = NULL

  data_plot$yhat.elastic_tc = NULL
  data_plot$yhat.beta_tc_ela = NULL
  data_plot$yhat.beta_tc_tree_ela= NULL
 

  data_plot$yhat.xgb_tc = NULL
  data_plot$yhat.betaboost_tc = NULL

  
  
  if (division == "region"){
    data_plot = data_plot[,c(3,34:ncol(data_plot))]
    data_plot$year_trend = NULL  
    data_plot = reshape2::melt(data_plot,id.vars = "region_Other")
    
  }
  
  else if (division == "period"){
    data_plot = data_plot[,c(4,34:ncol(data_plot))]
    data_plot$year_Other = as.numeric(levels(data_plot$year_Other))[data_plot$year_Other]
    period <- seq(min(data_plot$year_Other), max(data_plot$year_Other), by = 5)
    data_plot$period <- findInterval(data_plot$year_Other,  period)
    
    data_plot$year_Other = NULL
    data_plot$year_trend = NULL
    
    data_plot = reshape2::melt(data_plot, id.vars = "period")
    
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
folds = graph_data(df, predichos)

# Densidades y vs yhat  
data_plot_all = plot_data(folds, "none")

ggplot(data_plot_all, aes(x=value, color = variable))  +
  xlim(-.001, 1) +
  geom_density(lwd = 1, linetype = 1) 
 
#histograms
#FA
ggplot(data_plot_all, aes(x=value, fill = (variable)))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity')

#FR
ggplot(data_plot_all, aes(value, fill = variable)) +
  geom_histogram(aes(y = after_stat(density * width)),
                 position = "identity", alpha = 0.5)

ggplot(data_plot_all, aes(value, fill = variable)) +
  geom_histogram(aes(y = after_stat(density * width)), position = "identity", alpha = 0.5) +
  facet_wrap(~ variable)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.


#boxplot
errors = graph_data_errors(predichos)
#names and numbers of columns
colnames(errors)
errors_graph = reshape2::melt(errors[,c(14,15,17,19,20)])
names(errors_graph) = c("Methods","Error")

ggplot(data = errors_graph, aes(x=Methods, y=Error)) + geom_boxplot(aes(fill=Methods))

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












