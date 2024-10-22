library(dplyr)
library(ggplot2)


#######################################################################
#EXPERIMENTO 2
#######################################################################
#TABLAS

# df2
predichos10 = readRDS("predichos_mpi.Rdata")
predichos_a = readRDS("predichos_a.Rdata")
predichos_h = readRDS("predichos_h.Rdata")
# df1
predichos_mpi_df1 = readRDS("predichos_mpi_df1.Rdata")
predichos_a_df1 = readRDS("predichos_a_df1.Rdata")
predichos_h_df1 = readRDS("predichos_h_df1.Rdata")
# df13
predichos_mpi_df13 = readRDS("predichos_mpi_df13.Rdata")
predichos_a_df13 = readRDS("predichos_a_df13.Rdata")
predichos_h_df13 = readRDS("predichos_h_df13.Rdata")

lapply(9:16, function(x) predichos10[[1]]$results[x])

lapply(1:10, function(y) lapply(9:16, function(x) predichos10[[y]]$results[x]))

lapply(1:10, function(y) lapply(9:16, function(x) predichos10[[y]]$predicted[x]))

l = lapply(1:10, function(y) lapply(9:16, function(x) predichos10[[y]]$results[x]))
dists_fold = data.frame(matrix(unlist(l), nrow=length(l), byrow=TRUE))
colnames(dists_fold) = c("dist pls_tc", 
                         "dist beta_tc_cr",
                         "dist beta_tc_tree_cr","dist elastic_tc",     "dist beta_tc_ela",              "dist beta_tc_tree_ela",                         "dist xgb_tc",
                         "dist betaboost_tc" )
sapply(dists_fold, function(x) mean(na.omit(x)))

combis = rbind(sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds$y_test)$y,density(folds[,x])$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE))), sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds$y_test, from=0,to=1)$y,density(folds[,x], from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE))), sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds$y_test, from=0,to=1)$y,density(folds[,x])$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE))),sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds$y_test)$y,density(folds[,x], from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE))))
#recalculating distances
#df2
colnames(folds)[35:43]
as.numeric(philentropy::distance(rbind(density(folds$y_test, from = 0,to = 1,)$y,density(folds$yhat.pls_tc,from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE))
as.numeric(philentropy::distance(rbind(density(folds$y_test, from = 0,to = 1,)$y,density(folds[,36],from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE))

# MPI
#df2  
dist_mpi_df2 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds$y_test, from = 0,to = 1,)$y,density(folds[,x],from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))
as.numeric(philentropy::distance(rbind(density(folds$y_test, from = 0,to = 1,)$y,density(folds[,40],from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE))


# df1 
folds_df1 = graph_data(df_1, predichos_mpi_df1)
dist_mpi_df1 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_df1$y_test, from = 0,to = 1,)$y,density(na.omit(folds_df1[,x]),from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# df13 
folds_df13 = graph_data(df_13, predichos_mpi_df13)
dist_mpi_df13 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_df13$y_test, from = 0,to = 1,)$y,density(na.omit(folds_df13[,x]),from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# A
#df2  
dist_a_df2 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_a$y_test, from = 0.3,to = 1,)$y,density(folds_a[,x],from=0.3,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# df1 
folds_a_df1 = graph_data(df_1, predichos_a_df1)
dist_a_df1 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_a_df1$y_test, from = 0.3,to = 1,)$y,density(na.omit(folds_a_df1[,x]),from=0.3,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# df13 
folds_a_df13 = graph_data(df_13, predichos_a_df13)
dist_a_df13 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_a_df13$y_test, from = 0.3,to = 1,)$y,density(na.omit(folds_a_df13[,x]),from=0.3,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# H
#df2  
dist_h_df2 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_h$y_test, from = 0,to = 1,)$y,density(na.omit(folds_h[,x]),from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# df1 
folds_h_df1 = graph_data(df_1, predichos_h_df1)
dist_h_df1 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_h_df1$y_test, from = 0,to = 1,)$y,density(na.omit(folds_h_df1[,x]),from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# df13 
folds_h_df13 = graph_data(df_13, predichos_h_df13)
dist_h_df13 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_h_df13$y_test, from = 0,to = 1,)$y,density(na.omit(folds_h_df13[,x]),from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))


tabla_exp2 = function(lista, df){
  results = data.frame()
  for (i in 1:10) {
    res = lista[[i]]$results
    results = rbind(results, res)
    average = sapply(results, function(x) mean(na.omit(x)))
    average$n = nrow(df)
    average$p = length(colnames(df)[!((colnames(df) %in% c(colnames(df)[1:33])))])
    average$Total.de.paises = length(unique(df$iso))
    
  }
  return(average)
}


#mpi
View(cbind(tabla_exp2(predichos_mpi_df1,df_1),tabla_exp2(predichos10,df),tabla_exp2(predichos_mpi_df13,df_13)))
results_mpi = data.frame(cbind(tabla_exp2(predichos_mpi_df1,df_1),tabla_exp2(predichos10,df),tabla_exp2(predichos_mpi_df13,df_13)))
colnames(results_mpi) = c("df1","df2","df13")

cbind(dist_mpi_df1,dist_mpi_df2,dist_mpi_df13)

results_mpi[9:16,] = cbind(dist_mpi_df1,dist_mpi_df2,dist_mpi_df13)

results_a = data.frame(cbind(tabla_exp2(predichos_a_df1,df_1),tabla_exp2(predichos_a,df),tabla_exp2(predichos_a_df13,df_13)))
colnames(results_a) = c("df1","df2","df13")
results_a[9:16,] = cbind(dist_a_df1,dist_a_df2,dist_a_df13)

results_h = data.frame(cbind(tabla_exp2(predichos_h_df1,df_1),tabla_exp2(predichos_h,df),tabla_exp2(predichos_h_df13,df_13)))
colnames(results_h) = c("df1","df2","df13")
results_h[9:16,] = cbind(dist_h_df1,dist_h_df2,dist_h_df13)

xtable::xtable(results_mpi, digits = 4)
xtable::xtable(results_a, digits = 4)
xtable::xtable(results_h, digits = 4)

#######################################################################
#PLOTS
#######################################################################




best_methods = function(lista){
  results = data.frame()
  for (i in 1:10) {
    res = lista[[i]]$results
    results = rbind(results, res)
  }
  average = sapply(results, function(x) mean(x))
  average = average[-c(21:23)]
  best_mse = names(average[which.min(average[c(1:8)])])
  worst_mse = names(average[which.max(average[c(1:8)])])
  best_dist = names(average[c(9:17)])[which.min(average[c(9:17)])]
  worst_dist = names(average[c(9:17)])[which.max(average[c(9:17)])]
  print(paste("best mse = ",best_mse))
  print(paste("worst mse = ",worst_mse))
  print(paste("best dist = ",best_dist))
  print(paste("worst dist = ",worst_dist))
  return(average)
}

best_methods(predichos10)

graph_data = function(df, lista){
  predicted = data.frame()
  for (i in 1:10) {
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
  
  data_plot$yhat.pls_tc = NULL
  data_plot$yhat.beta_tc_cr = NULL
  data_plot$yhat.beta_tc_tree_cr = NULL
  
  data_plot$yhat.elastic_tc = NULL
  data_plot$yhat.beta_tc_ela = NULL
  data_plot$yhat.beta_tc_tree_ela= NULL
  
  
  #data_plot$yhat.xgb_tc = NULL
  #data_plot$yhat.betaboost_tc = NULL
  
  ######################################
  
  if (division == "region"){
    data_plot = data_plot[,c(3,34:ncol(data_plot))]
    data_plot$year_trend = NULL  
    data_plot = reshape2::melt(data_plot,id.vars = "region_Other")
    
    
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
folds = graph_data(df, predichos10)

# Densidades y vs yhat  
data_plot_all = plot_data(folds, "none")

theme_set(
  theme_light(base_size = 18) +
    theme(
      axis.line.x = element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      axis.line.y = element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = 0.5),
      panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = 0.5) ,
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 18),
      legend.position = "bottom",
      legend.text = element_text(size = 18), #
      
    )
)

densities_plot = ggplot(data_plot_all, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("Ytrue","Elastic Net","Beta (elastic)","Beta-Tree (elastic)"))+
  guides(linetype = "none")


# Histogramas
my_strip_labels <- as_labeller(c(
  "y_test" = "Ytrue",
  "yhat.pls_tc" = "Linear-PLS",
  "yhat.beta_tc_cr" = "Beta-PLS"
))

my_strip_labels <- as_labeller(c(
  "y_test" = "Ytrue",
  "yhat.elastic_tc" = "elastic",
  "yhat.beta_tc_ela" = "beta ela",
  "yhat.beta_tree_tc_cr" = "beta tree ela"
))


ggplot(data_plot_all, aes(value, fill = variable)) +
  geom_histogram(aes(y = after_stat(density * width)), position = "identity", alpha = 0.5, show.legend = F) +
  labs(x = "", y = "Relative Frequency", color = "")+
  xlim(-.001, 0.75) +
  facet_wrap(~ variable,
             labeller = my_strip_labels,  # add labels
             strip.position = "bottom") +
  theme(
    strip.placement = "outside",
    strip.text.x = element_text(
      size = 18, color = "black"),
    strip.background = element_blank(),
    panel.border = element_rect(fill = "transparent", # Necesario para agregar el borde
                                color = "black", linewidth = 0.5)
  )




# #histograms
# #FA
# ggplot(data_plot_all, aes(x=value, fill = (variable)))+
#   geom_histogram( color='#e9ecef', alpha=0.6, position='identity')
# 
# #FR
# ggplot(data_plot_all, aes(value, fill = variable)) +
#   geom_histogram(aes(y = after_stat(density * width)),
#                  position = "identity", alpha = 0.5)
# 
# ggplot(data_plot_all, aes(value, fill = variable)) +
#   geom_histogram(aes(y = after_stat(density * width)), position = "identity", alpha = 0.5) +
#   facet_wrap(~ variable)
# #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.




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



################################################
#ELASTIC
################################################
#A
folds_a = graph_data(df,predichos_a)
data_plot_a = plot_data(folds_a,"none")

densities_plot_A = ggplot(data_plot_a, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_A + 
  labs(x = "A", y = "Density", color = "") +
  xlim(0.32, 1) +
  scale_color_discrete(labels = c("A-true","Elastic Net","Beta (elastic)","Beta-Tree (elastic)"))+
  guides(linetype = "none")

# cant menores a 0.33
sapply(1:ncol(folds_a), function(x) nrow(folds_a[folds_a[,x]<0.33,]))

#H
folds_h = graph_data(df,predichos_h)
data_plot_h = plot_data(folds_h,"none")

densities_plot_H = ggplot(data_plot_h, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_H + 
  labs(x = "H", y = "Density", color = "") +
  # xlim(0.32, 1) +
  scale_color_discrete(labels = c("H-true","Elastic Net","Beta (elastic)","Beta-Tree (elastic)"))+
  guides(linetype = "none")


#MPI  HXA


# Y-hat A*H
View(graph_data(df,predichos_a))
View(graph_data(df,predichos10))

A_hat = graph_data(df,predichos_a)
A_hat = A_hat[,35:43]
H_hat = graph_data(df,predichos_h)
H_hat = H_hat[,35:43]
MPI_hat = graph_data(df,predichos10)
MPI_hat_hxa = A_hat * H_hat
#MPI_hat_hxa$y_test = MPI_hat$y_test
# colnames(MPI_hat_hxa)[1]  = "y_test"               
# colnames(MPI_hat_hxa)[2] =  "yhat.pls_tc_hxa"          
# colnames(MPI_hat_hxa)[3] ="yhat.beta_tc_cr_hxa"      
# colnames(MPI_hat_hxa)[4] ="yhat.beta_tc_tree_cr_hxa" 
# colnames(MPI_hat_hxa)[5] ="yhat.elastic_tc_hxa"      
# colnames(MPI_hat_hxa)[6] ="yhat.beta_tc_ela_hxa"     
# colnames(MPI_hat_hxa)[7] ="yhat.beta_tc_tree_ela_hxa"
# colnames(MPI_hat_hxa)[8] ="yhat.xgb_tc_hxa"          
# colnames(MPI_hat_hxa)[9] = "yhat.betaboost_tc_hxa"
# MPI_hat_hxa$y_test = NULL
View(MPI_hat_hxa)
View(MPI_hat)

data_plot_hxa = reshape2::melt(MPI_hat_hxa[,c(1,5,6,7)])

densities_plot = ggplot(data_plot_hxa, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI (HxA)", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("HxA-true","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","a","b","c"))+
  guides(linetype = "none")


################################################
# Lineal
################################################
#A
folds_a = graph_data(df,predichos_a)
data_plot_a = plot_data(folds_a,"none")
View(data_plot_a)

densities_plot_A = ggplot(data_plot_a, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_A + 
  labs(x = "A", y = "Density", color = "") +
  xlim(0.32, 1) +
  scale_color_discrete(labels = c("A-true","Linear-PLS","Beta-PLS","Beta-Tree-PLS"))+
  guides(linetype = "none")

# cant menores a 0.33
sapply(1:ncol(folds_a), function(x) nrow(folds_a[folds_a[,x]<0.33,]))

#H
folds_h = graph_data(df,predichos_h)
data_plot_h = plot_data(folds_h,"none")
View(data_plot_h)

densities_plot_H = ggplot(data_plot_h, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_H + 
  labs(x = "H", y = "Density", color = "") +
  # xlim(0.32, 1) +
  scale_color_discrete(labels = c("H-true","Linear-PLS","Beta-PLS","Beta-Tree-PLS"))+
  guides(linetype = "none")


#MPI  HXA


# Y-hat A*H
View(graph_data(df,predichos_a))
View(graph_data(df,predichos10))

A_hat = graph_data(df,predichos_a)
A_hat = A_hat[,35:43]
H_hat = graph_data(df,predichos_h)
H_hat = H_hat[,35:43]
MPI_hat = graph_data(df,predichos10)
MPI_hat_hxa = A_hat * H_hat
#MPI_hat_hxa$y_test = MPI_hat$y_test
# colnames(MPI_hat_hxa)[1]  = "y_test"               
# colnames(MPI_hat_hxa)[2] =  "yhat.pls_tc_hxa"          
# colnames(MPI_hat_hxa)[3] ="yhat.beta_tc_cr_hxa"      
# colnames(MPI_hat_hxa)[4] ="yhat.beta_tc_tree_cr_hxa" 
# colnames(MPI_hat_hxa)[5] ="yhat.elastic_tc_hxa"      
# colnames(MPI_hat_hxa)[6] ="yhat.beta_tc_ela_hxa"     
# colnames(MPI_hat_hxa)[7] ="yhat.beta_tc_tree_ela_hxa"
# colnames(MPI_hat_hxa)[8] ="yhat.xgb_tc_hxa"          
# colnames(MPI_hat_hxa)[9] = "yhat.betaboost_tc_hxa"
# MPI_hat_hxa$y_test = NULL
View(MPI_hat_hxa)
View(MPI_hat)

data_plot_hxa = reshape2::melt(MPI_hat_hxa[,c(1,2,3,4)])
View(data_plot_hxa)

densities_plot = ggplot(data_plot_hxa, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI (HxA)", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("HxA-true","Linear-PLS","Beta-PLS","Beta-Tree-PLS"))+
  guides(linetype = "none")

# MPI

folds = graph_data(df, predichos10)
data_plot_all = plot_data(folds, "none")
View(data_plot_all)
unique(data_plot_all$variable)

densities_plot = ggplot(data_plot_all, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("Ytrue","Linear-PLS","Beta-PLS","Beta-Tree-PLS"))+
  guides(linetype = "none")



################################################
# Boost
################################################
#A
folds_a = graph_data(df,predichos_a)
data_plot_a = plot_data(folds_a,"none")
unique(data_plot_a$variable)

densities_plot_A = ggplot(data_plot_a, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_A + 
  labs(x = "A", y = "Density", color = "") +
  xlim(0.32, 1) +
  scale_color_discrete(labels = c("A-true","XGBoost","Betaboost"))+
  guides(linetype = "none")

# cant menores a 0.33
sapply(1:ncol(folds_a), function(x) nrow(folds_a[folds_a[,x]<0.33,]))

#H
folds_h = graph_data(df,predichos_h)
data_plot_h = plot_data(folds_h,"none")
unique(data_plot_h$variable)

densities_plot_H = ggplot(data_plot_h, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_H + 
  labs(x = "H", y = "Density", color = "") +
  # xlim(0.32, 1) +
  scale_color_discrete(labels = c("H-true","XGBoost","Betaboost"))+
  guides(linetype = "none")


#MPI  HXA


# Y-hat A*H
View(graph_data(df,predichos_a))
View(graph_data(df,predichos10))

A_hat = graph_data(df,predichos_a)
A_hat = A_hat[,35:43]
H_hat = graph_data(df,predichos_h)
H_hat = H_hat[,35:43]
MPI_hat = graph_data(df,predichos10)
MPI_hat_hxa = A_hat * H_hat
#MPI_hat_hxa$y_test = MPI_hat$y_test
# colnames(MPI_hat_hxa)[1]  = "y_test"               
# colnames(MPI_hat_hxa)[2] =  "yhat.pls_tc_hxa"          
# colnames(MPI_hat_hxa)[3] ="yhat.beta_tc_cr_hxa"      
# colnames(MPI_hat_hxa)[4] ="yhat.beta_tc_tree_cr_hxa" 
# colnames(MPI_hat_hxa)[5] ="yhat.elastic_tc_hxa"      
# colnames(MPI_hat_hxa)[6] ="yhat.beta_tc_ela_hxa"     
# colnames(MPI_hat_hxa)[7] ="yhat.beta_tc_tree_ela_hxa"
# colnames(MPI_hat_hxa)[8] ="yhat.xgb_tc_hxa"          
# colnames(MPI_hat_hxa)[9] = "yhat.betaboost_tc_hxa"
# MPI_hat_hxa$y_test = NULL
View(MPI_hat_hxa)
View(MPI_hat)

data_plot_hxa = reshape2::melt(MPI_hat_hxa[,c(1,8,9)])
unique(data_plot_hxa$variable)

densities_plot = ggplot(data_plot_hxa, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI (HxA)", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("HxA-true","XGBoost","Betaboost"))+
  guides(linetype = "none")

# MPI

folds = graph_data(df, predichos10)
data_plot_all = plot_data(folds, "none")
View(data_plot_all)
unique(data_plot_all$variable)

densities_plot = ggplot(data_plot_all, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("Ytrue","XGBoost","Betaboost"))+
  guides(linetype = "none")

