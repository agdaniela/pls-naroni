source("resultados.R")



#Repeticiones
df = datas[[1]]

 

repetitions = function(df, target, nreps){
  results = list()
  nreps = nreps
  
  
  for (rep in 1:nreps) {
    print(paste("rep = ",rep))
    main = main_function(df,target)
    
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
    
    }
  
  
  return(results)
}


#res4 = repetitions(df,"mpi_Other",10)
#save(res4, file = "df2_10reps")

repetitions_alldfs = function(df_init, df_final, target, nreps){
  results_total = list()
  
  for (i in c(df_init:df_final)) {
    df <- datas[[i]]
    name <- paste('df',i,sep='_')
    
    results_df =  repetitions(df, target, nreps)
    
    results_total[[name]] <- results_df
    
     }
  
  return(results_total)
  
}

#try = repetitions_alldfs(5, 6, "mpi_Other",2) #replace (8,"mpi_Other",50)

#resultados

resultsdf1_8 = repetitions_alldfs(1, 8, "mpi_Other",50); saveRDS(resultsdf1_8, file = "resultsdf1_8.Rdata")

#readRDS("resultsdf1_8.Rdata")



#una prueba para ver que funcione:
#try2 = repetitions(df,"mpi_Other",1); saveRDS(try2, file = "try2.Rdata")
#try2 = readRDS("try2.Rdata")



