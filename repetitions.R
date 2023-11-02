# source("main_function_se.R")
# source("main_function_unaobs.R")
# source("main_function_dum.R")
#source("main_function_beta.R")

 
#Repeticiones
 
#funcion para repetir cambiando entre sin estandarizar, dejando una obs en test e incluyendo las variables 

#funciones = list(main_function_tcyd = main_function_tcyd, main_function = main_function, main_function_unaobs=main_function_unaobs, main_function_dum = main_function_dum, main_function_unaobs_dum = main_function_unaobs_dum )


# repetitions = function(df, target, nreps, fun){
#   results = list()
#   nreps = nreps
#   
#   
#   for (rep in 1:nreps) {
#     print(paste("rep = ",rep)) #OJO QUE CAMBIE LA MAIN PARA HACER TEST EN UNA
#     
#     main <- tryCatch(funciones[[fun]](df,target) , error= function(e) {return(list())}  )
#     if (length(main) != 0) {
#       print("ok con la rep")
#       
#       
#       results$MSE.elastic[rep] = main$MSE.elastic
#       results$MSE.pls_d1[rep] = main$MSE.pls_d1
#       results$MSE.pls_opt[rep] = main$MSE.pls_opt
#       results$MSE.pls_np_d1[rep] = main$MSE.pls_np_d1
#       results$MSE.pls_np_opt[rep] = main$MSE.pls_np_opt
#       results$MSE.xgBoost[rep] = main$MSE.xgBoost
#       
#       results$MAE.elastic[rep] = main$MAE.elastic
#       results$MAE.pls_d1[rep] = main$MAE.pls_d1
#       results$MAE.pls_opt[rep] = main$MAE.pls_opt
#       results$MAE.pls_np_d1[rep] = main$MAE.pls_np_d1
#       results$MAE.pls_np_opt[rep] = main$MAE.pls_np_opt
#       results$MAE.xgBoost[rep] = main$MAE.xgBoost
#       
#       results$n[rep] = main$n
#       results$p[rep] = main$p
#       results$Total.de.paises[rep] = main$Total.de.paises
#       results$d.optimo[rep] = main$d.optimo
#       results$variables.elastic[rep] = main$variables.elastic
#       
#       }else{
#         paste("Algo está mal con la rep", i)
#         results$MSE.elastic[rep] = NA
#         results$MSE.pls_d1[rep] = NA
#         results$MSE.pls_opt[rep] = NA
#         results$MSE.pls_np_d1[rep] = NA
#         results$MSE.pls_np_opt[rep] = NA
#         results$MSE.xgBoost[rep] = NA
#         
#         results$MAE.elastic[rep] = NA
#         results$MAE.pls_d1[rep] = NA
#         results$MAE.pls_opt[rep] = NA
#         results$MAE.pls_np_d1[rep] = NA
#         results$MAE.pls_np_opt[rep] = NA
#         results$MAE.xgBoost[rep] = NA
#         
#         results$n[rep] = NA
#         results$p[rep] = NA
#         results$Total.de.paises[rep] = NA
#         results$d.optimo[rep] = NA
#         results$variables.elastic[rep] = NA
#         
#         next
#         }
#     
#     
#   
#   }
#   return(results)
# }  

 
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

repetitions2 = function(df, target, d,link_phi,link_mu, distancia, nreps){
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
      
      results$MSE.beta_td_de[rep] = main$MSE.beta_td_de
      results$MSE.beta_tc_de[rep] = main$MSE.beta_tc_de
  
      results$MSE.beta_td_ela_de[rep] = main$MSE.beta_td_ela_de
      results$MSE.beta_td_ela_sr[rep] = main$MSE.beta_td_ela_sr
      
      results$MSE.beta_tc_ela_de[rep] = main$MSE.beta_tc_ela_de
      results$MSE.beta_tc_ela_sr[rep] = main$MSE.beta_tc_ela_sr
      
      results$MSE.beta_tc_tree_de[rep] = main$MSE.beta_tc_tree_de
      results$MSE.beta_tc_tree_ela_sr[rep] = main$MSE.beta_tc_tree_ela_sr
      
      #distancias
      results$dist.elastic_td[rep] = main$dist.elastic_td
      results$dist.elastic_tc[rep] = main$dist.elastic_tc
      
      results$dist.beta_td_de[rep] = main$dist.beta_td_de
      results$dist.beta_tc_de[rep] = main$dist.beta_tc_de
      
      results$dist.beta_td_ela_de[rep] = main$dist.beta_td_ela_de
      results$dist.beta_td_ela_sr[rep] = main$dist.beta_td_ela_sr
      
      results$dist.beta_tc_ela_de[rep] = main$dist.beta_tc_ela_de
      results$dist.beta_tc_ela_sr[rep] = main$dist.beta_tc_ela_sr
      
      results$dist.beta_tc_tree_de[rep] = main$dist.beta_tc_tree_de
      results$dist.beta_tc_tree_ela_sr[rep] = main$dist.beta_tc_tree_ela_sr
      
       
      results$n[rep] = main$n
      results$p[rep] = main$p
      results$Total.de.paises[rep] = main$Total.de.paises
       
      
    }else{
      paste("Algo está mal con la rep", i)
      results$MSE.elastic_td[rep] = NA
      results$MSE.elastic_tc[rep] = NA

      results$MSE.beta_td_de[rep] = NA
      results$MSE.beta_tc_de[rep] = NA

      results$MSE.beta_td_ela_de[rep] = NA
      results$MSE.beta_td_ela_sr[rep] = NA

      results$MSE.beta_tc_ela_de[rep] = NA
      results$MSE.beta_tc_ela_sr[rep] = NA

      results$MSE.beta_tc_tree_de[rep] = NA
      results$MSE.beta_tc_tree_ela_sr[rep] = NA

      results$dist.elastic_td[rep] = NA
      results$dist.elastic_tc[rep] = NA

      results$dist.beta_td_de[rep] = NA
      results$dist.beta_tc_de[rep] = NA

      results$dist.beta_td_ela_de[rep] = NA
      results$dist.beta_td_ela_sr[rep] = NA

      results$dist.beta_tc_ela_de[rep] = NA
      results$dist.beta_tc_ela_sr[rep] = NA

      results$dist.beta_tc_tree_de[rep] = NA
      results$dist.beta_tc_tree_ela_sr[rep] = NA


      results$n[rep] = NA
      results$p[rep] = NA
      results$Total.de.paises[rep] = NA


      next
    }


    
  }
  return(results)
}  

















