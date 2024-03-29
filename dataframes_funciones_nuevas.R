############################################################################################
source("dataframes_analisis_variables.R")

dataprep = function(file){
  df = read.delim(file, header = TRUE, sep = " ", dec = ".")
  df$iso = as.factor(df$iso)
  df$country = as.factor(df$country)
  df$year <- as.numeric(df$year)
  df = df[,-7]
  regs = read.csv("dataframes_regions.csv")
  regions = regs[(regs$Country.Code %in% paste((df$iso))), c(1,2)] 
  regions = cbind(regions, aggregate(year ~ iso, data = df, FUN = length)[,2])
  regions = regions[rep(1:nrow(regions), regions$`aggregate(year ~ iso, data = df, FUN = length)[, 2]`),]
  df$region = regions$Region
  df = df[, c("iso", "country", "region", colnames(df)[3:1484])]
  df = categoriavar(df)
  df = dummiesregion(df)
  df = dummiesyear(df)
  return(df)
}

############################################################################################
# funciones para sacar paises conforme a los nas

datasacandopaises = function(df, paises, paisadc = NULL){
  if (is.null(paisadc) == FALSE){
    paises = c(paises, paisadc)
  }
  nuevadata = df[!(df$country %in% paises),]
  return(nuevadata)
}

data3 = function(df){
  data = df[(duplicated(df$country)|duplicated(df$country, fromLast=TRUE)),]
  datafr = datasacandopaises(data, paisesasacar(data, 2000))
  return(datafr)
} 

nodata3 = function(df){
  datafr = rbind(df[!(duplicated(df$country)|duplicated(df$country, fromLast=TRUE)),], df[df$country %in% paisesasacar(df,2000),])
  return(datafr)
} 

# si vemos resumenporpais(selectrawsdfs(plsdata,2)) vemos que tienen muchos nas, por los que en este paso saco los paises con mas de 600
# se podria aumentar

paisesasacar =function(df, cantdeNAs){
  respais = resumenporpais(df)
  datapaises = respais[respais[,6] >= cantdeNAs,]
  paises = c( paste(datapaises[,4]))
  
  
  return(paises)
}



variablesconNAs =function(df, cantdeNAs){
  res = resumenporvariable(df)
  res$names = rownames(res)
  datavariables = res[res[,1] == cantdeNAs,]
  variables = paste(datavariables[,2])
  return(variables)
}

datossolona = function(df, cantNAS){
  vars = variablesconNAs(df,cantNAS)
  datos = cbind(df[,1:4],df[,(colnames(df) %in% vars),])
  datosunna = datos[!complete.cases(datos), ]
  return(datosunna)
}




 

##############################################################################################
# Resumen por fila
#NAS por fila

resumenporfila = function(df){
  nasperrow = data.frame("cantidadNAs" = apply(df, 1, function(x) sum(is.na(x))))
  nasporfila = cbind(df$iso, df$country, nasperrow)
  
  return(nasporfila)
}
##############################################################################################
#Resumen por variable

 

resumenporvariable = function(df){
  naporvar = sapply(df, function(y) sum(is.na(y)))  #cantidad de NAs por variable
  res = data.frame("nas" = naporvar ) #ordenamos las variables por porcentaje de NAs
  resumen = res %>% arrange(res$nas) #ordenamos las variables por porcentaje de NAs
  
  return(resumen)
}



##############################################################################################
#Funciones para sacar paises en función de los NAS

resumenporpais = function(df){
  obs = aggregate(year_Other ~ country_Other, data = df, FUN = length) #contamos la cantidad de años por pais
  nasporfila = resumenporfila(df) #tomamos los nas por fila
  nasporpais2 = aggregate(cantidadNAs ~ df$country, data = nasporfila, FUN = sum)[2] #sumamos los nas por pais
  
  res = cbind("pais"=obs[,1], "cant años" = obs[,2], "NAs por pais" = nasporpais2)
  
  varorden = res[order(res[,3]),] #ordenamos las variables por cantidad de NAs
  
  resumen = cbind(res, varorden[,colnames(varorden)]) #ponemos todo en el mismo coso
  
  return(resumen)
  
}

############################################################################################

# funcion que limpia los paises conforme a la cantidad de nas de las variables

library(dplyr)

generico5 = function(df,times){
  
  for (cantNAS in 1:max(resumenporvariable(df)[,1])) {
    
    if(sum(is.na(df)) == 0){
      print("Ya no hay mas Nas")
      break 
    }else if (identical(paste(datossolona(df,cantNAS)$country), character(0))){
      print(paste("Tengo que tomar variables con", cantNAS+1, "NAs"))
      next 
      
    }else{ 
      
      paises = paste(datossolona(df,cantNAS)$country) #lista de paises con ciertos nas
      resu = resumenporpais(df)[resumenporpais(df)$pais %in% paises,] #cantidad de observaciones de la lista de paises
      obsdelpais = numeric()
      for (pais in paises) {
        obs = resu[resu$pais == pais,][,2]
        obsdelpais = c(obsdelpais,obs)
      }
      years = datossolona(df,cantNAS)$year #año de la observacion
      combi = data.frame(paises,obsdelpais, years) #combinacion de todos
      
      sacados = data.frame()
      for (i in 1:nrow(combi)) {
        
        if(combi$obsdelpais[i] == 2){
          sacar = df[(df$country %in% combi$paises[i]),] #saco el pais entero
          sacados = rbind(sacados,sacar) #saco la observacion correspondiente
          
        }else{
          sacar = df[(df$country == combi$paises[i] & df$year == combi$years[i] ),]
          sacados = rbind(sacados,sacar) #saco la observacion correspondiente
        }
        
      }
      datos = df %>% anti_join(sacados)
      # si despues de todo esto queda algun pais con 1 sola observacion, lo saco
      if(sum(!(duplicated(datos$country)|duplicated(datos$country, fromLast=TRUE))) != 0){
        datos = datos[(duplicated(datos$country)|duplicated(datos$country, fromLast=TRUE)),]
      }
      
      print(paste("Se tomaron variables con", cantNAS, "NAs"))
      break
    }
  }
  return(datos)
} 




##############################################################################################

# Resumen general (tablita) ----
resumen = function(df){
  
  res = data.frame("Total de observaciones" = nrow(df), 
                   "Total de predictores (WBI)"= length(colnames(df)[!((colnames(df) %in% c("iso","country","region","year","MPI","H","A",colnames(df)[8:13])))]),
                   "Total de paises" = length(unique(df$iso)),
                   "Total de columnas" = ncol(df),
                   "Total de NAS" = sum(is.na(df)),
                   "Total de predictores sin NAS" = length(colnames(df))-13)
   
  return(res)
}




#la tablita para el overleaf

tablita1 = function(df,num){
  tabla = data.frame()
  for (i in 1:num) {
    fila1 = resumen(selectrawdfs(df,i))
    tabla = rbind(tabla,fila1)
  }
  numdedf = seq(1:nrow(tabla))
  tabla = cbind(numdedf,tabla)
   
  return(tabla)
}


tablita2 = function(df,num){
  tabla = data.frame()
  for (i in 1:num) {
    fila1 = resumen(selectdfs(df,i))
    tabla = rbind(tabla,fila1)
  }
  numdedf = seq(1:nrow(tabla))
  tabla = cbind(numdedf,tabla)
  
  return(tabla)
}


# sacando los NAs

sacarnas = function(df){
  datafr = df[ , apply(df, 2, function(x) !any(is.na(x)))]
  
  return(datafr)
}


#tabla1 = tablita1(plsdata,29)
#tabla2 = tablita2(plsdata,29)
#tabla1$Total.de.predictores.sin.NAS = tabla2$Total.de.predictores.sin.NAS
#tabla = tabla1
#View(tabla)

