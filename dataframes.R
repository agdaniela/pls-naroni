########################################################################################

source("dataframes_funciones_nuevas.R")

########################################################################################
# Datos ----
plsdata = dataprep("bigdata.txt")

########################################################################################
#Se usa igual que antes, con selectrawdfs(plsdata,num) tenemos un dataframe de la tablita con NAS
# y con selectdfs(plsdata,num) tenemos el mismo pero sin NAs
# ahora los WBI empiezan en la columna 14
########################################################################################
selectrawdfs = function(df,num){
  # (1) Data original ----
  if (num == 1){
    datafr = df
  }
  # (2) Datos menos los paises que tienen una sola observacion (años) ----
  else if (num == 2){
    datafr = df[(duplicated(df$country)|duplicated(df$country, fromLast=TRUE)),]
  }
  # (3) Datos menos los paises que  tienen mas de 60 NAS ----
  else if (num == 3){
    datafr = data3(df)
  }
  # (4) Datos menos los paises/obs que haga a la/s variables tener cierta cantidad de un NAs ----
  else {
    data = data3(df)
    datafr = Reduce(generico5, 1:(num-3), init = data) #repito (num-3) la funcion para ir generando los dfs
    
  }
  
  return(datafr)
} 


########################################################################################
# Función para sacar dataframes  1 - 25 sin NAS

selectdfs = function(df, num){
  connas = selectrawdfs(df, num)
  datafr = sacarnas(connas)
  return(datafr)
}


########################################################################################

olvidadosrawdfs = function(df,num){
  # (1) Data original ----
  if (num == 1){
    nodata = data.frame()
  }
  # (2) Datos menos los paises que tienen una sola observacion (años) ----
  else if (num == 2){
    nodata = df[!(duplicated(df$country)|duplicated(df$country, fromLast=TRUE)),]
  }
  # (3) Datos menos los paises que  tienen mas de 60 NAS ----
  else if (num == 3){
    nodata = nodata3(df)
  }
  # (4) Datos menos los paises/obs que haga a la/s variables tener cierta cantidad de un NAs ----
  else {
    
    data = selectrawdfs(df,num)
    nodata = df  %>% anti_join(data)
  }
  
  return(nodata)
} 

#checks  
#try = olvidadosrawdfs(plsdata, 25) 
#length(unique(try$country))

#######################################################################################
# Función para sacar los paises dejados en dataframes  1 - 25 sin NAS
olvidadosdfs = function(df, num){
  connas = olvidadosrawdfs(df, num)
  datafr = sacarnas(connas)
  return(datafr)
}

#checks  
#try = olvidadosrawdfs(plsdata, 10) 
#try2 = olvidadosdfs(plsdata,10)





 