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

   # main <-  main_function_tcyd(df,target,d,link_phi,link_mu,distancia)
                     
      results$MSE.elastic_td[rep] = main$MSE.elastic_td
      results$MSE.elastic_tc[rep] = main$MSE.elastic_tc
      
      results$MSE.xgb_td[rep] = main$MSE.xgb_td
      results$MSE.xgb_tc[rep] = main$MSE.xgb_tc
      
      results$MSE.beta_td_cr[rep] = main$MSE.beta_td_cr
      results$MSE.beta_tc_cr[rep] = main$MSE.beta_tc_cr
  
      results$MSE.beta_td_ela_sr[rep] = main$MSE.beta_td_ela_sr
      
    #  results$MSE.beta_tc_ela_cr[rep] = main$MSE.beta_tc_ela_cr
      results$MSE.beta_tc_ela_sr[rep] = main$MSE.beta_tc_ela_sr
      
      results$MSE.beta_tc_tree_cr[rep] = main$MSE.beta_tc_tree_cr
      results$MSE.beta_tc_tree_ela_sr[rep] = main$MSE.beta_tc_tree_ela_sr
      
      results$MSE.betaboost_td[rep] = main$MSE.betaboost_td
      results$MSE.betaboost_tc[rep] = main$MSE.betaboost_tc
      
      #distancias
      results$dist.elastic_td[rep] = main$dist.elastic_td
      results$dist.elastic_tc[rep] = main$dist.elastic_tc
      
      results$dist.xgb_td[rep] = main$dist.xgb_td
      results$dist.xgb_tc[rep] = main$dist.xgb_tc
      
      results$dist.beta_td_cr[rep] = main$dist.beta_td_cr
      results$dist.beta_tc_cr[rep] = main$dist.beta_tc_cr
      
     # results$dist.beta_td_ela_cr[rep] = main$dist.beta_td_ela_cr
      results$dist.beta_td_ela_sr[rep] = main$dist.beta_td_ela_sr
      
      #results$dist.beta_tc_ela_cr[rep] = main$dist.beta_tc_ela_cr
      results$dist.beta_tc_ela_sr[rep] = main$dist.beta_tc_ela_sr
      
      results$dist.beta_tc_tree_cr[rep] = main$dist.beta_tc_tree_cr
      results$dist.beta_tc_tree_ela_sr[rep] = main$dist.beta_tc_tree_ela_sr
      
      results$dist.betaboost_td[rep] = main$dist.betaboost_td
      results$dist.betaboost_tc[rep] = main$dist.betaboost_tc
      
      results$n[rep] = main$n
      results$p[rep] = main$p
      results$Total.de.paises[rep] = main$Total.de.paises
       
      
    }else{
      paste("Algo está mal con la rep", i)
      results$MSE.elastic_td[rep] = NA
      results$MSE.elastic_tc[rep] = NA

      results$MSE.beta_td_cr[rep] = NA
      results$MSE.beta_tc_cr[rep] = NA

      #results$MSE.beta_td_ela_cr[rep] = NA
      results$MSE.beta_td_ela_sr[rep] = NA

      #results$MSE.beta_tc_ela_cr[rep] = NA
      results$MSE.beta_tc_ela_sr[rep] = NA

      results$MSE.beta_tc_tree_cr[rep] = NA
      results$MSE.beta_tc_tree_ela_sr[rep] = NA

      results$dist.elastic_td[rep] = NA
      results$dist.elastic_tc[rep] = NA

      results$dist.beta_td_cr[rep] = NA
      results$dist.beta_tc_cr[rep] = NA

      #results$dist.beta_td_ela_cr[rep] = NA
      results$dist.beta_td_ela_sr[rep] = NA

      #results$dist.beta_tc_ela_cr[rep] = NA
      results$dist.beta_tc_ela_sr[rep] = NA

      results$dist.beta_tc_tree_cr[rep] = NA
      results$dist.beta_tc_tree_ela_sr[rep] = NA


      results$n[rep] = NA
      results$p[rep] = NA
      results$Total.de.paises[rep] = NA


      next
    }


    
  }
  return(results)
}  

















