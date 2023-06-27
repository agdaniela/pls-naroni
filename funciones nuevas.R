############################################################################################
dataprep = function(file){
  df = read.delim(file, header = TRUE, sep = " ", dec = ".")
  df$iso = as.factor(df$iso)
  df$country = as.factor(df$country)
  df$year <- as.numeric(df$year)
  df = df[,-7]
  regs = read.csv("regions.csv")
  regions = regs[(regs$Country.Code %in% paste((df$iso))), c(1,2)] 
  regions = cbind(regions, aggregate(year ~ iso, data = df, FUN = length)[,2])
  regions = regions[rep(1:nrow(regions), regions$`aggregate(year ~ iso, data = df, FUN = length)[, 2]`),]
  df$region = regions$Region
  df = df[, c("iso", "country", "region", colnames(df)[3:156] )]
  
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
  datafr = datasacandopaises(data, paisesasacar(data, 60))
  return(datafr)
} 




variablesconNAs =function(df, cantdeNAs){
  res = resumenporvariable(df)
  datavariables = res[res[,4] == cantdeNAs,]
  variables = c( paste(datavariables[,3]))
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
  pertotal = round((naporvar/249)*100,2) #Cantidad de NAs en porcentaje del total
  
  
  res = data.frame("nas" = naporvar, "porcentaje"=pertotal) #ordenamos las variables por porcentaje de NAs
  
  varorden = res[order(res[,2]),] #ordenamos las variables por porcentaje de NAs
  
  resumen = cbind(res, rownames(varorden),varorden[,colnames(varorden)]) #ponemos todo en el mismo coso
  
  return(resumen)
}


##############################################################################################
#Funciones para sacar paises en función de los NAS

resumenporpais = function(df){
  obs = aggregate(year ~ country, data = df, FUN = length) #contamos la cantidad de años por pais
  nasporfila = resumenporfila(df) #tomamos los nas por fila
  nasporpais2 = aggregate(cantidadNAs ~ df$country, data = nasporfila, FUN = sum)[2] #sumamos los nas por pais
  
  res = cbind("pais"=obs[,1], "cant años" = obs[,2], "NAs por pais" = nasporpais2)
  
  varorden = res[order(res[,3]),] #ordenamos las variables por cantidad de NAs
  
  resumen = cbind(res, varorden[,colnames(varorden)]) #ponemos todo en el mismo coso
  
  return(resumen)
  
}

paisesasacar =function(df, cantdeNAs){
  respais = resumenporpais(df)
  datapaises = respais[respais[,6] >= cantdeNAs,]
  paises = c( paste(datapaises[,4]))
  
  
  return(paises)
}

############################################################################################

# funcion que limpia los paises conforme a la cantidad de nas de las variables

library(dplyr)

generico5 = function(df,times){
  
  for (cantNAS in 1:max(resumenporvariable(df)[,4])) {
    
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


