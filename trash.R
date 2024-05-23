#Beta con d chosen
# sacar
# pls.projections_td_ela_cr <- chemometrics::pls1_nipals(Xtrain_td_ela, ytrain, a = d, scale = FALSE)
# data_td_ela_cr <- as.data.frame(cbind(ytrain, as.matrix(Xtrain_td_ela) %*% pls.projections_td_ela_cr$W))
# newdata_td_ela_cr <- data.frame(as.matrix(Xtest_td_ela) %*% pls.projections_td_ela_cr$W)
# colnames(newdata_td_ela_cr) <- colnames(data_td_ela_cr)[-1]
# 
# #Beta-regression 
# data_beta_td_ela_cr = data.frame(data_td_ela_cr, Ttrain, Rtrain)
# formu_td_ela_cr = as.formula(paste("ytrain", paste(names(data_beta_td_ela_cr)[-1], collapse=" + "), sep=" ~ ")) 
# beta.fit_td_ela_cr <- tryCatch(betareg::betareg(formu_td_ela_cr , data = data_beta_td_ela_cr, link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
# 
# # Predict over Xtest
# newdata_beta_td_ela_cr = data.frame(newdata_td_ela_cr,Ttest,Rtest)
# ytest_pred.beta_td_ela_cr = tryCatch(predict(beta.fit_td_ela_cr, newdata_beta_td_ela_cr), error= function(e) {return(NA)}  )
# # distance
# dist_beta_td_ela_cr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_td_ela_cr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
# 

#Beta con d chosen
#  pls.projections_tc_ela_cr <- chemometrics::pls1_nipals(Xtrain_tc_ela, ytrain, a = d, scale = FALSE)
# data_tc_ela_cr <- as.data.frame(cbind(ytrain, as.matrix(Xtrain_tc_ela) %*% pls.projections_tc_ela_cr$W))
# newdata_tc_ela_cr <- data.frame(as.matrix(Xtest_tc_ela) %*% pls.projections_tc_ela_cr$W)
# colnames(newdata_tc_ela_cr) <- colnames(data_tc_ela_cr)[-1]
# 
# #Beta-regression 
# data_beta_tc_ela_cr = data.frame(data_tc_ela_cr, Rtrain)
# formu_tc_ela_cr = as.formula(paste("ytrain", paste(names(data_beta_tc_ela_cr)[-1], collapse=" + "), sep=" ~ ")) 
# beta.fit_tc_ela_cr <- tryCatch(betareg::betareg(formu_tc_ela_cr , data = data_beta_tc_ela_cr,  link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
# 
# # Predict over Xtest
# newdata_beta_tc_ela_cr = data.frame(newdata_tc_ela_cr, Rtest)
# ytest_pred.beta_tc_ela_cr = tryCatch(predict(beta.fit_tc_ela_cr, newdata_beta_tc_ela_cr), error= function(e) {return(NA)}  )
# # distance
# dist_beta_tc_ela_cr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_ela_cr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
# 


# de otra forma:
# dist_beta_tc_tree_ela_sr = as.numeric(philentropy::distance(rbind(ytest, ytest_pred.beta_tc_tree_sr), est.prob = "empirical", method = "hellinger"))


#  "MSE beta_td_ela_cr" = mean((ytest-ytest_pred.beta_td_ela_cr)^2),
#  "MSE beta_tc_ela_cr" = mean((ytest-ytest_pred.beta_tc_ela_cr)^2),
# "dist beta_td_ela_cr" = dist_beta_td_ela_cr,
#  "dist beta_tc_ela_cr" = dist_beta_tc_ela_cr,

# results$MSE.pls_td[rep] = NA
# results$MSE.pls_np_td[rep] = NA
# results$MSE.beta_td_cr[rep] = NA
# results$MSE.beta_td_tree_cr[rep] = NA
# 
# results$MSE.elastic_td[rep] = NA
# results$MSE.beta_td_ela[rep] = NA
# results$MSE.xgb_td[rep] = NA
# results$MSE.betaboost_td[rep] = NA
# results$MSE.beta_td_tree_ela[rep] = NA
# 
# results$MSE.pls_tc[rep] = NA
# results$MSE.pls_np_tc[rep] = NA
# results$MSE.beta_tc_cr[rep] = NA
# results$MSE.beta_tc_tree_cr[rep] = NA
# 
# results$MSE.elastic_tc[rep] = NA
# results$MSE.beta_tc_ela[rep] = NA
# results$MSE.xgb_tc[rep] = NA
# results$MSE.betaboost_tc[rep] = NA
# results$MSE.beta_tc_tree_ela[rep] = NA
# 
# #distancias
# results$dist.pls_td[rep] = NA
# results$dist.pls_np_td[rep] = NA
# results$dist.beta_td_cr[rep] = NA
# results$dist.beta_td_tree_cr[rep] = NA
# 
# results$dist.elastic_td[rep] = NA
# results$dist.beta_td_ela[rep] = NA
# results$dist.xgb_td[rep] = NA
# results$dist.betaboost_td[rep] = NA
# results$dist.beta_td_tree_ela[rep] = NA
# 
# results$dist.pls_tc[rep] = NA
# results$dist.pls_np_tc[rep] = NA
# results$dist.beta_tc_cr[rep] = NA
# results$dist.beta_tc_tree_cr[rep] = NA
# 
# results$dist.elastic_tc[rep] = NA
# results$dist.beta_tc_ela[rep] = NA
# results$dist.xgb_tc[rep] = NA
# results$dist.betaboost_tc[rep] = NA
# results$dist.beta_tc_tree_ela[rep] = NA
# 
# results$n[rep] = NA
# results$p[rep] = NA
# results$Total.de.paises[rep] = NA




main_function_pred = function(Xtrain, Xtest , target, d, link_phi, link_mu, distancia){
  
  predicted = data.frame()
  results = data.frame()
  # nfolds <- 5
  # 
  Xtrain$year_trend <- as.numeric(as.character(Xtrain$year_Other))
  Xtrain$year_trend <- Xtrain$year_trend - min(Xtrain$year_trend)
  
  Xtest$year_trend <- as.numeric(as.character(Xtest$year_Other))
  Xtest$year_trend <- Xtest$year_trend - min(Xtest$year_trend)
  
  # Train and test datasets
  ytrain <- Xtrain[, target] 
  Xtrain <- Xtrain[,-c(1:33)]
  Ttrain = Xtrain[, c(13:33)]
  Rtrain = Xtrain[, c(8:12)]
  
  ytest <- Xtest[, target]
  Xtest <- Xtest[,-c(1:33)]
  Ttest = Xtest[, c(13:33)]
  Rtest = Xtest[, c(8:12)]
  
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
  ytest_pred.beta_tc_tree_de =  tryCatch(predict(beta.fit_tc_tree_de, newdata_beta_tc_tree_de), error= function(e) {return(NA)}  )
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
  ytest_pred.beta_tc_tree_sr =  tryCatch(predict(beta.fit_tc_tree_ela_sr, newdata_beta_tc_tree_ela_sr), error= function(e) {return(NA)}  )
  
  # distance
  dist_beta_tc_tree_ela_sr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_tree_sr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  ##################################################################################
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
    
    
    "n" = nrow(df_fold),  
    "p"= length(colnames(df_fold)[!((colnames(df_fold) %in% c(colnames(df_fold)[1:33])))]),
    "Total de paises" = length(unique(df_fold$iso))
    
    
  )
  
  predicted = data.frame(
    
    "y_test" = ytest,
    
    "yhat elastic_td" = ytest_pred.elastic_td  ,
    "yhat elastic_tc" =  ytest_pred.elastic_tc  ,
    
    
    "yhat beta_td_de" = ytest_pred.beta_td_de  ,
    "yhat beta_tc_de" = ytest_pred.beta_tc_de  ,
    
    "yhat beta_td_ela_de" = ytest_pred.beta_td_ela_de  ,
    "yhat beta_td_ela_sr" = ytest_pred.beta_td_ela_sr ,
    
    "yhat beta_tc_ela_de" = ytest_pred.beta_tc_ela_de  ,
    "yhat beta_tc_ela_sr" = ytest_pred.beta_tc_ela_sr  ,
    
    "yhat beta_tc_tree_de" = ytest_pred.beta_tc_tree_de  ,
    "yhat beta_tc_tree_ela_sr" = ytest_pred.beta_tc_tree_sr, 
    
    "yhat betaboost_td" =  ytest_pred.betaboost_td ,
    "yhat betaboost_tc" =  ytest-ytest_pred.betaboost_tc 
    
  )
  
  return(list("results"=results,"predicted" = predicted))
  
}
# prueba
df = datas[[2]]
data <- random.split(df, 0.8)
Xtrain <- data$data_train
Xtest = data$data_test

prueba2 = main_function_pred(Xtrain,Xtest,target = "mpi_Other",d=1,  link_phi = "log", link_mu = "logit", distancia = "hellinger")
View(prueba2[["predicted"]] )
###################################################################

# antes de sacar tiempo discreto ----
main_function_tcyd = function(df, target, d_pls, d_plsbeta , link_phi, link_mu, distancia){
  
  #  
  # 
  results = data.frame()
  nfolds <- 5
  
  df$year_trend <- as.numeric(as.character(df$year_Other))
  df$year_trend <- df$year_trend - min(df$year_trend)
  
  # Train and test datasets
  data <- random.split(df, 0.8)
  ytrain <- data$data_train[, target] 
  Ttrain = data$data_train[, c(13:33)] #dummies de tiempo
  Rtrain = data$data_train[, c(8:12)] #regiones
  Xtrain <- data$data_train[,-c(1:33)]
  
  ytest <- data$data_test[, target]
  Ttest = data$data_test[, c(13:33)]
  Rtest = data$data_test[, c(8:12)]
  Xtest <- data$data_test[,-c(1:33)]
  # data <- as.data.frame(cbind(ytrain, Xtrain))
  pls.directions = 30 
  ########################################## ----
  #   # Discrete (dummy) time
  #   Xtrain_td = subset(Xtrain, select=-c(year_trend))
  #   Xtest_td = subset(Xtest, select=-c(year_trend))
  #   
  #   # 1.  Using dimension reduction
  #   
  #   #if (missing(d_pls) & missing(d_plsbeta) ) {
  #     # if no argument for d is provided, then take optimal d
  #    
  #     hyperparam <- kfoldCV.pls(Xtrain_td, ytrain, nfolds, pls.directions)
  #     hyperparam_beta <- kfoldCV.plsbeta(Xtrain_td, ytrain, nfolds, pls.directions)
  #     
  #     d_pls = hyperparam$d.min
  #     d_plsbeta = hyperparam_beta$d.min
  # #  } else{
  #     # If d is provided, take it for all DR
  #  #   d_pls = d_pls
  # #    d_plsbeta = d_plsbeta
  #  # }
  #   
  #   # 1.1 PLS with optimal $d$ using linear predictor. 
  #   
  #   # Data  
  #   pls.projections_td_cr <- chemometrics::pls1_nipals(Xtrain_td, ytrain, a = d_pls, scale = FALSE)
  #   data_td_cr <- as.data.frame(cbind(ytrain, as.matrix(Xtrain_td) %*% pls.projections_td_cr$W))
  #   newdata_td_cr <- data.frame(as.matrix(Xtest_td) %*% pls.projections_td_cr$W)
  #   colnames(newdata_td_cr) <- colnames(data_td_cr)[-1]
  #   
  #   # Train and test data with dimension reduction and dummies of time and region - formula of fit
  #   data_td_cr_dum = data.frame(data_td_cr, Ttrain, Rtrain)
  #   formu_td_cr = as.formula(paste("ytrain", paste(names(data_td_cr_dum)[-1], collapse=" + "), sep=" ~ ")) 
  #   newdata_td_cr_dum = data.frame(newdata_td_cr,Ttest,Rtest)
  #   
  #   # Fit
  #   pls.fit <- lm(ytrain ~ . , data_td_cr_dum)
  #   # Predict over Xtest
  #   ytest_pred.pls_td  <- tryCatch(predict(pls.fit, newdata_td_cr_dum), error= function(e) {return(NA)}  )
  #   # Distance
  #   dist_pls_td = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.pls_td)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  #   
  #   # 1.2 PLS with fixed $d=1$ using non-parametric regression.
  #   
  #   pls.projections_td_d1 <- chemometrics::pls1_nipals(Xtrain_td, ytrain, a = 1, scale = FALSE)
  #   data_td_d1 <- as.data.frame(cbind(ytrain, as.matrix(Xtrain_td) %*% pls.projections_td_d1$W))
  #   newdata_td_d1 <- data.frame(as.matrix(Xtest_td) %*% pls.projections_td_d1$W)
  #   colnames(newdata_td_d1) <- colnames(data_td_d1)[-1]
  #   # Fit
  #   formu_td_d1 = as.formula(paste("ytrain", paste(names(data_td_d1)[-1], collapse=" + "), sep=" ~ ")) 
  #   bw_td_d1 <- np::npregbw(formu_td_d1, data=data_td_d1) 
  #   np_PLS_td_d1 <- np::npreg(bw_td_d1)
  #   # Predict over Xtest
  #   ytest_pred.np_td_d1 = tryCatch(predict(np_PLS_td_d1, newdata=newdata_td_d1, type="response"), error= function(e) {return(NA)}  )
  #   # Distance
  #   dist_pls_np_td = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.np_td_d1)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  #   
  #   # 1.3 PLS with optimal $d$ using beta regression (For hyperparameter, training with the prediction model)
  #   pls.projections_td_dbeta <- chemometrics::pls1_nipals(Xtrain_td, ytrain, a = d_plsbeta, scale = FALSE)
  #   data_td_dbeta <- as.data.frame(cbind(ytrain, as.matrix(Xtrain_td) %*% pls.projections_td_dbeta$W))
  #   newdata_td_dbeta <- data.frame(as.matrix(Xtest_td) %*% pls.projections_td_dbeta$W)
  #   colnames(newdata_td_dbeta) <- colnames(data_td_dbeta)[-1]
  #   
  #   # Fit
  #   formu_td_dbeta = as.formula(paste("ytrain", paste(names(data_td_dbeta)[-1], collapse=" + "), sep=" ~ ")) 
  #   beta.fit_td <- tryCatch(betareg::betareg(formu_td_dbeta , data = data_td_dbeta, link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
  #   # Predict over Xtest
  #   ytest_pred.beta_td_cr = tryCatch(predict(beta.fit_td, newdata_td_dbeta), error= function(e) {return(NA)}  )
  #   # Distance
  #   dist_beta_td_cr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_td_cr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  #   
  #   # 1.4 PLS with optimal $d$ using beta tree model for prediction.
  #   
  #   dummy_corte_train = ifelse(ytrain<=0.2, 1, 0)
  #   dummy_corte_test = as.vector(ifelse(ytest<=0.2, 1, 0))
  #   # Fit
  #   data_beta_td_tree_cr = data.frame(data_td_dbeta, dummy_corte_train)
  #   beta.fit_td_tree_cr <- tryCatch(betareg::betatree(formu_td_dbeta, ~ dummy_corte_train, data = data_beta_td_tree_cr ,  link.phi = link_phi, link = link_mu ))
  #   #predict
  #   newdata_beta_td_tree_cr = data.frame(newdata_td_dbeta, dummy_corte_test)
  #   names(newdata_beta_td_tree_cr)[length(names(newdata_beta_td_tree_cr))]  = "dummy_corte_train"
  #   ytest_pred.beta_td_tree_cr =  tryCatch(predict(beta.fit_td_tree_cr, newdata_beta_td_tree_cr), error= function(e) {return(NA)}  )
  #   # distance
  #   dist_beta_td_tree_cr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_td_tree_cr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ) , error= function(e) {return(NA)}  )
  # 
  #   
  #   # 2. Based on Regularized Regresion/Variable Selection
  #   
  #   # Data without dimension reduction
  #   data_td_sr = data.frame(Xtrain_td,Ttrain, Rtrain)
  #   newdata_td_sr <- data.frame(Xtest_td,Ttest,Rtest)
  #   
  #   # 2.1 Elastic Net.
  #   #Fit
  #   hyperparam <- kfoldCV.elastic(data_td_sr, ytrain, nfolds)
  #   elastic.fit_td <- glmnet::glmnet(x = data_td_sr, y = ytrain, family = "gaussian", alpha = hyperparam$best.alpha, lambda = hyperparam$best.lambda)
  #   # Predict over Xtest
  #   ytest_pred.elastic_td <- glmnet::predict.glmnet(elastic.fit_td, as.matrix(newdata_td_sr))
  #   
  #   # distance
  #   dist_elastic_td = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.elastic_td)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  #   
  #   # 2.2 Beta regression using selected covariates with elastic net.
  #   tmp_coeffs_td <- coef(elastic.fit_td, s = "lambda.min")
  #   var_selected_td = tmp_coeffs_td@Dimnames[[1]][tmp_coeffs_td@i +1]
  #   Xtrain_td_ela = Xtrain_td[,colnames(Xtrain_td) %in% var_selected_td]
  #   Xtest_td_ela <- Xtest_td[,colnames(Xtest_td) %in% var_selected_td]
  #   # Fit
  #   data_beta_td_ela = data.frame(Xtrain_td_ela, Ttrain, Rtrain)
  #   formu_td_ela = as.formula(paste("ytrain", paste(names(data_beta_td_ela)[-1], collapse=" + "), sep=" ~ ")) 
  #   beta.fit_td_ela <- tryCatch(betareg::betareg(formu_td_ela, data = data_beta_td_ela, link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
  #   # Predict over Xtest
  #   newdata_beta_td_ela = data.frame(Xtest_td_ela,Ttest,Rtest)
  #   ytest_pred.beta_td_ela = tryCatch(predict(beta.fit_td_ela, newdata_beta_td_ela), error= function(e) {return(NA)}  )
  #   # distance
  #   dist_beta_td_ela = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_td_ela)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  #   
  #   # 2.3 Beta tree using selected covariates with elastic net.
  #   # Fit
  #   data_beta_td_tree_ela = data.frame(data_beta_td_ela, dummy_corte_train)
  #   formu_td_tree_ela = as.formula(paste("ytrain", paste(names(data_beta_td_tree_ela)[-1], collapse=" + "), sep=" ~ "))
  #   beta.fit_td_tree_ela <- tryCatch(betareg::betatree(formu_td_tree_ela, ~ dummy_corte_train, data = data_beta_td_tree_ela,  link.phi = link_phi, link = link_mu   ), error= function(e) {return(NA)}  )
  #   # predict
  #   newdata_beta_td_tree_ela = data.frame(newdata_beta_td_ela, dummy_corte_test )
  #   names(newdata_beta_td_tree_ela)[length(names(newdata_beta_td_tree_ela))]  = "dummy_corte_train"
  #   ytest_pred.beta_td_tree_ela =  tryCatch(predict(beta.fit_td_tree_ela, newdata_beta_td_tree_ela), error= function(e) {return(NA)}  )
  #   # distance
  #   dist_beta_td_tree_ela = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_td_tree_ela)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  #   
  #   # 2.3 Beta lasso  
  #  
  #    #Fit
  #   hyperparam <- kfoldCV.betalasso(data_td_sr, ytrain, nfolds)
  #   betalasso.fit_td <- penalizedbeta::betareg_lasso(X = data_td_sr, y = ytrain, lambda = hyperparam$s.min)
  #   # Predict over Xtest
  #   ytest_pred.betalasso_td <- predict(betalasso.fit_td, as.matrix(newdata_td_sr))
  #   # distance
  #   dist_betalasso_td = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.betalasso_td)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  #   
  #   # 3 Based on Model Selection
  #   
  #   # 3.1 XGBoost lineal.
  #   #Fit
  #   hyperparam_xgb_td = kfoldCV.xgboost(data_td_sr, ytrain, nfolds)
  #   # Predict over Xtest
  #   ytest_pred.xgb_td <- tryCatch(predict(hyperparam_xgb_td$xgb.model, newdata_td_sr), error= function(e) {return(NA)}  )
  #   # distance
  #   dist_xgb_td = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.xgb_td)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  #   
  #   # 3.2 Beta Boost.
  #   # Fit
  #   formu_boost_td = as.formula(paste("ytrain", paste(names(data_td_sr )[-1], collapse=" + "), sep=" ~ ")) 
  #   betaboost.fit_td <- tryCatch(mboost::glmboost(formu_boost_td, data = data_td_sr, family = betaboost::BetaReg()), error= function(e) {return(NA)}  )
  #   # Predict over Xtest
  #   ytest_pred.betaboost_td = predict(betaboost.fit_td, newdata = newdata_td_sr, type = "response")
  #   dist_betaboost_td  = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.betaboost_td)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
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
  
  # # 1.2 PLS with fixed $d=1$ using non-parametric regression.
  # pls.projections_tc_d1 <- chemometrics::pls1_nipals(Xtrain, ytrain, a = 1, scale = FALSE)
  # data_tc_d1 <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections_tc_d1$W))
  # newdata_tc_d1 <- data.frame(as.matrix(Xtest) %*% pls.projections_tc_d1$W)
  # colnames(newdata_tc_d1) <- colnames(data_tc_d1)[-1]
  # # Fit
  # formu_tc_d1 = as.formula(paste("ytrain", paste(names(data_tc_d1)[-1], collapse=" + "), sep=" ~ ")) 
  # bw_tc_d1 <- np::npregbw(formu_tc_d1, data=data_tc_d1) 
  # np_PLS_tc_d1 <- tryCatch(np::npreg(bw_tc_d1), error= function(e) {return(NA)}  )
  # # Predict over Xtest
  # ytest_pred.np_tc_d1 = tryCatch(predict(np_PLS_tc_d1, newdata=newdata_tc_d1, type="response"), error= function(e) {return(NA)}  )
  # # Distance
  # dist_pls_np_tc = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.np_tc_d1)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
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
  ytest_pred.beta_tc_cr = tryCatch(betareg::predict(beta.fit_tc_cr, newdata_tc_dbeta), error= function(e) {return(NA)}  )
  # distance
  dist_beta_tc_cr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_cr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  
  # 1.4 PLS with optimal $d$ using beta tree model for prediction.
  
  # Fit
  dummy_corte_train = ifelse(ytrain<=0.2, 1, 0)
  dummy_corte_test = as.vector(ifelse(ytest<=0.2, 1, 0))
  
  data_beta_tc_tree_cr = data.frame(data_tc_dbeta,dummy_corte_train)
  formu_tc_dbeta_tree = as.formula(paste("ytrain", paste(names(data_beta_tc_tree_cr)[-1], collapse=" + "), sep=" ~ ")) 
  
  beta.fit_tc_tree_cr <- tryCatch(betareg::betatree(formu_tc_dbeta_tree, ~ dummy_corte_train, data = data_beta_tc_tree_cr ,  link.phi = link_phi, link = link_mu ), error= function(e) {return(NA)}  )
  #predict
  newdata_beta_tc_tree_cr = data.frame(newdata_tc_dbeta, dummy_corte_test)
  names(newdata_beta_tc_tree_cr)[length(names(newdata_beta_tc_tree_cr))]  = "dummy_corte_train"
  ytest_pred.beta_tc_tree_cr =  tryCatch(predict(beta.fit_tc_tree_cr, newdata_beta_tc_tree_cr), error= function(e) {return(NA)}  )
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
  ytest_pred.beta_tc_ela = tryCatch(predict(beta.fit_tc_ela, newdata_beta_tc_ela), error= function(e) {return(NA)}  )
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
    
    # 1.  Using dimension reduction
    
    # 1.1 PLS with optimal $d$ using linear predictor. 
    # 1.2 PLS with fixed $d=1$ using non-parametric regression.
    # 1.3 PLS with optimal $d$ using beta regression (For hyperparameter, training with the prediction model)
    # 1.4 PLS with optimal $d$ using beta tree model for prediction.
    
    
    # 2. Based on Regularized Regresion/Variable Selection
    
    # 2.1 Elastic Net.
    # 2.2 Beta regression using selected covariates with elastic net.
    # 2.3 Beta tree using selected covariates with elastic net.
    # 2.3 Beta lasso (Falta probar. NACHO)
    
    # 3 Based on Model Selection
    
    # 3.1 XGBoost lineal.
    # 3.2 Beta Boost.
    # "MSE pls_td" = mean((ytest-ytest_pred.pls_td)^2),
    # "MSE pls_np_td" = mean((ytest-ytest_pred.np_td_d1)^2),
    # "MSE beta_td_cr" = mean((ytest-ytest_pred.beta_td_cr)^2),
    # "MSE beta_td_tree_cr" = mean((ytest-ytest_pred.beta_td_tree_cr)^2),
    # 
    # "MSE elastic_td" = mean((ytest-ytest_pred.elastic_td)^2),
    # "MSE beta_td_ela" = mean((ytest-ytest_pred.beta_td_ela)^2),
    # "MSE beta_td_tree_ela" = mean((ytest-ytest_pred.beta_td_tree_ela)^2),
    # "MSE betalasso_td" = mean((ytest-ytest_pred.betalasso_td)^2),
    # 
    # "MSE xgb_td" = mean((ytest-ytest_pred.xgb_td)^2),
    # "MSE betaboost_td" = mean((ytest-ytest_pred.betaboost_td)^2),
    # 
    
    "MSE pls_tc" = mean((ytest-ytest_pred.pls_tc)^2),
    # "MSE pls_np_tc" = mean((ytest-ytest_pred.np_tc_d1)^2),
    "MSE beta_tc_cr" = mean((ytest-ytest_pred.beta_tc_cr)^2),
    "MSE beta_tc_tree_cr" = mean((ytest-ytest_pred.beta_tc_tree_cr)^2),
    
    "MSE elastic_tc" = mean((ytest-ytest_pred.elastic_tc)^2),
    "MSE beta_tc_ela" = mean((ytest-ytest_pred.beta_tc_ela)^2),
    "MSE beta_tc_tree_ela" = mean((ytest-ytest_pred.beta_tc_tree_ela)^2),
    "MSE betalasso_tc" = mean((ytest-ytest_pred.betalasso_tc)^2),
    
    "MSE xgb_tc" = mean((ytest-ytest_pred.xgb_tc)^2),
    "MSE betaboost_tc" = mean((ytest-ytest_pred.betaboost_tc)^2),
    
    
    "dist pls_td" = dist_pls_td,
    "dist pls_np_td" = dist_pls_np_td,
    "dist beta_td_cr" = dist_beta_td_cr,
    "dist beta_td_tree_cr" = dist_beta_td_tree_cr,
    
    "dist elastic_td" = dist_elastic_td,
    "dist beta_td_ela" = dist_beta_td_ela,
    "dist beta_td_tree_ela" = dist_beta_td_tree_ela,
    "dist betalasso_td" = dist_betalasso_td,
    
    "dist xgb_td" = dist_xgb_td,
    "dist betaboost_td" = dist_betaboost_td,
    
    "dist pls_tc" = dist_pls_tc,
    "dist pls_np_tc" = dist_pls_np_tc,
    "dist beta_tc_cr" = dist_beta_tc_cr,
    "dist beta_tc_tree_cr" = dist_beta_tc_tree_cr,
    
    "dist elastic_tc" = dist_elastic_tc,
    "dist beta_tc_ela" = dist_beta_tc_ela,
    "dist beta_tc_tree_ela" = dist_beta_tc_tree_ela,
    "dist betalasso_tc" = dist_betalasso_tc,
    
    "dist xgb_tc" = dist_xgb_tc,
    "dist betaboost_tc" = dist_betaboost_tc,
    
    "n" = nrow(df),  
    "p"= length(colnames(df)[!((colnames(df) %in% c(colnames(df)[1:33])))]),
    "Total de paises" = length(unique(df$iso))
    # agregar dimensiones y elegidas por elastic
    
  )
  
  return(results)
  
}


############
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
  
  # # 1.2 PLS with fixed $d=1$ using non-parametric regression.
  # pls.projections_tc_d1 <- chemometrics::pls1_nipals(Xtrain, ytrain, a = 1, scale = FALSE)
  # data_tc_d1 <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections_tc_d1$W))
  # newdata_tc_d1 <- data.frame(as.matrix(Xtest) %*% pls.projections_tc_d1$W)
  # colnames(newdata_tc_d1) <- colnames(data_tc_d1)[-1]
  # # Fit
  # formu_tc_d1 = as.formula(paste("ytrain", paste(names(data_tc_d1)[-1], collapse=" + "), sep=" ~ ")) 
  # bw_tc_d1 <- np::npregbw(formu_tc_d1, data=data_tc_d1) 
  # np_PLS_tc_d1 <- tryCatch(np::npreg(bw_tc_d1), error= function(e) {return(NA)}  )
  # # Predict over Xtest
  # ytest_pred.np_tc_d1 = tryCatch(predict(np_PLS_tc_d1, newdata=newdata_tc_d1, type="response"), error= function(e) {return(NA)}  )
  # # Distance
  # dist_pls_np_tc = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.np_tc_d1)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
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
    
    # "MSE pls_td" = mean((ytest-ytest_pred.pls_td)^2),
    # "MSE pls_np_td" = mean((ytest-ytest_pred.np_td_d1)^2),
    # "MSE beta_td_cr" = mean((ytest-ytest_pred.beta_td_cr)^2),
    # "MSE beta_td_tree_cr" = mean((ytest-ytest_pred.beta_td_tree_cr)^2),
    # 
    # "MSE elastic_td" = mean((ytest-ytest_pred.elastic_td)^2),
    # "MSE beta_td_ela" = mean((ytest-ytest_pred.beta_td_ela)^2),
    # "MSE beta_td_tree_ela" = mean((ytest-ytest_pred.beta_td_tree_ela)^2),
    # "MSE betalasso_td" = mean((ytest-ytest_pred.betalasso_td)^2),
    # 
    # "MSE xgb_td" = mean((ytest-ytest_pred.xgb_td)^2),
    # "MSE betaboost_td" = mean((ytest-ytest_pred.betaboost_td)^2),
    # 
    
    "MSE pls_tc" = mean((ytest-ytest_pred.pls_tc)^2),
    # "MSE pls_np_tc" = mean((ytest-ytest_pred.np_tc_d1)^2),
    "MSE beta_tc_cr" = mean((ytest-ytest_pred.beta_tc_cr)^2),
    "MSE beta_tc_tree_cr" = mean((ytest-ytest_pred.beta_tc_tree_cr)^2),
    
    "MSE elastic_tc" = mean((ytest-ytest_pred.elastic_tc)^2),
    "MSE beta_tc_ela" = mean((ytest-ytest_pred.beta_tc_ela)^2),
    "MSE beta_tc_tree_ela" = mean((ytest-ytest_pred.beta_tc_tree_ela)^2),
    "MSE betalasso_tc" = mean((ytest-ytest_pred.betalasso_tc)^2),
    
    "MSE xgb_tc" = mean((ytest-ytest_pred.xgb_tc)^2),
    "MSE betaboost_tc" = mean((ytest-ytest_pred.betaboost_tc)^2),
    
    
    # "dist pls_td" = dist_pls_td,
    # "dist pls_np_td" = dist_pls_np_td,
    # "dist beta_td_cr" = dist_beta_td_cr,
    # "dist beta_td_tree_cr" = dist_beta_td_tree_cr,
    # 
    # "dist elastic_td" = dist_elastic_td,
    # "dist beta_td_ela" = dist_beta_td_ela,
    # "dist beta_td_tree_ela" = dist_beta_td_tree_ela,
    # "dist betalasso_td" = dist_betalasso_td,
    # 
    # "dist xgb_td" = dist_xgb_td,
    # "dist betaboost_td" = dist_betaboost_td,
    # 
    "dist pls_tc" = dist_pls_tc,
    # "dist pls_np_tc" = dist_pls_np_tc,
    "dist beta_tc_cr" = dist_beta_tc_cr,
    "dist beta_tc_tree_cr" = dist_beta_tc_tree_cr,
    
    "dist elastic_tc" = dist_elastic_tc,
    "dist beta_tc_ela" = dist_beta_tc_ela,
    "dist beta_tc_tree_ela" = dist_beta_tc_tree_ela,
    "dist betalasso_tc" = dist_betalasso_tc,
    
    "dist xgb_tc" = dist_xgb_tc,
    "dist betaboost_tc" = dist_betaboost_tc,
    
    
    
    
    "n" = nrow(df),  
    "p"= length(colnames(df)[!((colnames(df) %in% c(colnames(df)[1:33])))]),
    "Total de paises" = length(unique(df$iso))
    
    
  )
  
  predicted = data.frame(
    
    "y_test" = ytest,
    
    # "yhat pls_td" =  ytest_pred.pls_td,
    # "yhat pls_np_td" = ytest_pred.np_td_d1,
    # "yhat beta_td_cr" = ytest_pred.beta_td_cr,
    # "yhat beta_td_tree_cr" = ytest_pred.beta_td_tree_cr,
    # 
    # "yhat elastic_td" = ytest_pred.elastic_td,
    # "yhat beta_td_ela" = ytest_pred.beta_td_ela,
    # "yhat beta_td_tree_ela" = ytest_pred.beta_td_tree_ela,
    # "yhat betalasso_td" = ytest_pred.betalasso_td,
    # 
    # "yhat xgb_td" = ytest_pred.xgb_td,
    # "yhat betaboost_td" = ytest_pred.betaboost_td,
    # 
    # 
    "yhat pls_tc" = ytest_pred.pls_tc,
    #"yhat pls_np_tc" = ytest_pred.np_tc_d1,
    "yhat beta_tc_cr" = ytest_pred.beta_tc_cr,
    "yhat beta_tc_tree_cr" = ytest_pred.beta_tc_tree_cr,
    
    "yhat elastic_tc" = ytest_pred.elastic_tc,
    "yhat beta_tc_ela" = ytest_pred.beta_tc_ela,
    "yhat beta_tc_tree_ela" = ytest_pred.beta_tc_tree_ela,
    "yhat betalasso_tc" = ytest_pred.betalasso_tc,
    
    "yhat xgb_tc" = ytest_pred.xgb_tc,
    "yhat betaboost_tc" = ytest_pred.betaboost_tc
    
  )
  
  
  return(list("results"=results,"predicted" = predicted))
  
}
plot_data = function(df, division){
  
  data_plot = df
  
  # choose methods by comment
  data_plot$yhat.pls_td= NULL
  data_plot$yhat.pls_np_td= NULL
  data_plot$yhat.beta_td_cr= NULL
  data_plot$yhat.beta_td_tree_cr= NULL
  
  data_plot$yhat.elastic_td= NULL
  data_plot$yhat.beta_td_ela= NULL
  data_plot$yhat.beta_td_tree_ela= NULL
  data_plot$yhat.betalasso_td= NULL
  
  data_plot$yhat.xgb_td = NULL
  data_plot$yhat.betaboost_td= NULL
  
  
  data_plot$yhat.pls_tc = NULL
  data_plot$yhat.pls_np_tc= NULL
  #data_plot$yhat.beta_tc_cr = NULL
  #data_plot$yhat.beta_tc_tree_cr = NULL
  
  data_plot$yhat.elastic_tc = NULL
  data_plot$yhat.beta_tc_ela = NULL
  #data_plot$yhat.beta_tc_tree_ela= NULL
  data_plot$yhat.betalasso_tc = NULL
  
  data_plot$yhat.xgb_tc = NULL
  #data_plot$yhat.betaboost_tc = NULL
  
  
  
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


# Densidades por periodos de tiempo, de 5 en 5

data_plot_per = plot_data(folds, "period")

ggplot(data_plot_per, aes(x=value, color = variable)) + 
  geom_density(lwd = 1, linetype = 1) +
  facet_wrap(~period, scales = "free") 





















