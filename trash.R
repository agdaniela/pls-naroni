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
