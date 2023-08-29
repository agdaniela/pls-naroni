source("resultados.R")


#holis 
#Repeticiones
#df = datas[[1]]

#funcion de prueba

repetitions = function(df, target, nreps){
  results = list()
  nreps = nreps
  
  
  for (rep in 1:nreps) {
    print(paste("rep = ",rep)) #OJO QUE CAMBIE LA MAIN PARA HACER TEST EN UNA
    
    main <- tryCatch(main_function_unaobs(df,target) , error= function(e) {return(list())}  )
    if (length(main) != 0) {
      print("ok con la rep")
      
      
      results$MSE.elastic[rep] = main$MSE.elastic
      results$MSE.pls_d1[rep] = main$MSE.pls_d1
      results$MSE.pls_opt[rep] = main$MSE.pls_opt
      results$MSE.pls_np_d1[rep] = main$MSE.pls_np_d1
      results$MSE.pls_np_opt[rep] = main$MSE.pls_np_opt
      results$MSE.xgBoost[rep] = main$MSE.xgBoost
      
      results$MAE.elastic[rep] = main$MAE.elastic
      results$MAE.pls_d1[rep] = main$MAE.pls_d1
      results$MAE.pls_opt[rep] = main$MAE.pls_opt
      results$MAE.pls_np_d1[rep] = main$MAE.pls_np_d1
      results$MAE.pls_np_opt[rep] = main$MAE.pls_np_opt
      results$MAE.xgBoost[rep] = main$MAE.xgBoost
      
      results$n[rep] = main$n
      results$p[rep] = main$p
      results$Total.de.paises[rep] = main$Total.de.paises
      results$d.optimo[rep] = main$d.optimo
      results$variables.elastic[rep] = main$variables.elastic
      
      }else{
        paste("Algo está mal con la rep", i)
        results$MSE.elastic[rep] = NA
        results$MSE.pls_d1[rep] = NA
        results$MSE.pls_opt[rep] = NA
        results$MSE.pls_np_d1[rep] = NA
        results$MSE.pls_np_opt[rep] = NA
        results$MSE.xgBoost[rep] = NA
        
        results$MAE.elastic[rep] = NA
        results$MAE.pls_d1[rep] = NA
        results$MAE.pls_opt[rep] = NA
        results$MAE.pls_np_d1[rep] = NA
        results$MAE.pls_np_opt[rep] = NA
        results$MAE.xgBoost[rep] = NA
        
        results$n[rep] = NA
        results$p[rep] = NA
        results$Total.de.paises[rep] = NA
        results$d.optimo[rep] = NA
        results$variables.elastic[rep] = NA
        
        next
        }
    
    
  
  }
  return(results)
}  

 
for (i in 6:6){
  print(paste("df = ", i))
  
  results = repetitions(datas[[i]], "mpi_Other", 50)
  
  name_file = paste0("results_se_df",i, ".Rdata")
  
  saveRDS(results, file = name_file)
  
}

# Para dejar en test lo de una obs

for (i in 2:2){
  print(paste("df = ", i))
  
  results = repetitions(datas[[i]], "mpi_Other", 50)
  
  name_file = paste0("results_se_df",i, ".Rdata")
  
  saveRDS(results, file = name_file)
  
}





















