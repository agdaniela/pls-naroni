# source("main_function_se.R")
# source("main_function_unaobs.R")
# source("main_function_dum.R")
#source("main_function_beta.R")

 
#Repeticiones
 

 
# for (i in 8:8){
#   print(paste("df = ", i))
#   
#   results = repetitions(datas[[i]], "mpi_Other", 2, "main_function_dum")
#   
#   name_file = paste0("results_se_dum_",i, ".Rdata")
#   
#   saveRDS(results, file = name_file)
#   
# }
# 

#funciones = list(main_function_tcyd = main_function_tcyd  )

repetitions = function(df, target, d,link_phi,link_mu, distancia, nreps){
  results = list()
  nreps = nreps
  
  
  for (rep in 1:nreps) {
    print(paste("rep = ",rep))  

    main <- tryCatch(main_function_tcyd(df,target,d,link_phi,link_mu,distancia) , error= function(e) {return(list())}  )
    if (length(main) != 0) {
      print("ok con la rep")

      # 1. Predicción con reducción de dimensiones
      # 1.1 PLS lineal
      # 1.2 PLS NP
      # 1.3 PLS beta regression
      # 
      # 2. Predicción con variable / feature and model selection 
      # 2.1 Elastic Net
      # 2.2 Beta combinado con elastic net
      # 2.3 XGBoost lineal
      # 2.4 BetaBoost
      # 2.5 Betatree
      
      results$MSE.pls_td[rep] = main$MSE.pls_td
      results$MSE.pls_np_td[rep] = main$MSE.pls_np_td
      results$MSE.beta_td_cr[rep] = main$MSE.beta_td_cr
      results$MSE.beta_td_tree_cr[rep] = main$MSE.beta_td_tree_cr
      
      results$MSE.elastic_td[rep] = main$MSE.elastic_td
      results$MSE.beta_td_ela[rep] = main$MSE.beta_td_ela
      results$MSE.xgb_td[rep] = main$MSE.xgb_td
      results$MSE.betaboost_td[rep] = main$MSE.betaboost_td
      results$MSE.beta_td_tree_ela[rep] = main$MSE.beta_td_tree_ela
      
      
      results$MSE.pls_tc[rep] = main$MSE.pls_tc
      results$MSE.pls_np_tc[rep] = main$MSE.pls_np_tc
      results$MSE.beta_tc_cr[rep] = main$MSE.beta_tc_cr
      results$MSE.beta_tc_tree_cr[rep] = main$MSE.beta_tc_tree_cr
      
      results$MSE.elastic_tc[rep] = main$MSE.elastic_tc
      results$MSE.beta_tc_ela[rep] = main$MSE.beta_tc_ela
      results$MSE.xgb_tc[rep] = main$MSE.xgb_tc
      results$MSE.betaboost_tc[rep] = main$MSE.betaboost_tc
      results$MSE.beta_tc_tree_ela[rep] = main$MSE.beta_tc_tree_ela
      
      #distancias
      results$dist.pls_td[rep] = main$dist.pls_td
      results$dist.pls_np_td[rep] = main$dist.pls_np_td
      results$dist.beta_td_cr[rep] = main$dist.beta_td_cr
      results$dist.beta_td_tree_cr[rep] = main$dist.beta_td_tree_cr
      
      
      results$dist.elastic_td[rep] = main$dist.elastic_td
      results$dist.beta_td_ela[rep] = main$dist.beta_td_ela
      results$dist.xgb_td[rep] = main$dist.xgb_td
      results$dist.betaboost_td[rep] = main$dist.betaboost_td
      results$dist.beta_td_tree_ela[rep] = main$dist.beta_td_tree_ela
      
      results$dist.pls_tc[rep] = main$dist.pls_tc
      results$dist.pls_np_tc[rep] = main$dist.pls_np_tc
      results$dist.beta_tc_cr[rep] = main$dist.beta_tc_cr
      results$dist.beta_tc_tree_cr[rep] = main$dist.beta_tc_tree_cr
      
      results$dist.elastic_tc[rep] = main$dist.elastic_tc
      results$dist.beta_tc_ela[rep] = main$dist.beta_tc_ela
      results$dist.xgb_tc[rep] = main$dist.xgb_tc
      results$dist.betaboost_tc[rep] = main$dist.betaboost_tc
      results$dist.beta_tc_tree_ela[rep] = main$dist.beta_tc_tree_ela
      
 
      
      
      
      results$n[rep] = main$n
      results$p[rep] = main$p
      results$Total.de.paises[rep] = main$Total.de.paises
       
      
    }else{
      paste("Algo está mal con la rep", i)
      
      


      next
    }


    
  }
  return(results)
}  

















