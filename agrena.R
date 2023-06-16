############################################################################################
# Preparación de los datos ----

dataprep = function(file){
  df = read.delim(file, header = TRUE, sep = " ", dec = ".")
  df$iso = as.factor(df$iso)
  df$country = as.factor(df$country)
  df$year = lubridate::ymd(df$year, truncated = 2L)
  df = df[,-7]
  
  return(df)
}


############################################################################################
# Agregación de NAs por grupo/s ----
aggregateNAsOver <- function(df, ic, fc, groupvar) {
  ###
  # Funci?n para agregar NAS por grupo y para varias variables
  #- df: dataframe
  #- ic: (int) n?mero de columna (variable) inicial
  #- if: (int) n?mero de columna (variable) final
  #- groupvar: variable para agrupar
  ###
  NASdata = data.frame(Group = sort(unique(df[[groupvar]])))
  c = colnames(df)
  for(i in seq(ic,fc)) {                                   
    newcolumn = aggregate(formula(paste(c[i], "~", groupvar )), data=df, function(x) {sum(is.na(x))}, na.action = NULL)[2]
    
    NASdata[ , ncol(NASdata) + 1] = newcolumn                   
  }
  
  return(NASdata)
  
}


aggregateNAsOver2 <- function(df, ic, fc, groupvar1, groupvar2) {
  ###
  # Funci?n para agregar NAS mostrando dos grupos y para varias variables
  #- df: dataframe
  #- ic: (int) n?mero de columna (variable) inicial
  #- if: (int) n?mero de columna (variable) final
  #- groupvar1 y 2: variables para agrupar
  ###
  NASdata = data.frame(Group1 = df[[groupvar1]], Group2 = df[[groupvar2]])
  c = colnames(df)
  for(i in seq(ic,fc)) {                                   
    newcolumn = aggregate(formula(paste(c[i], "~", groupvar1, "+", groupvar2 )), data=df, function(x) {sum(is.na(x))}, na.action = NULL)[3]
    
    NASdata[ , ncol(NASdata) + 1] = newcolumn                   
  }
  
  return(NASdata)
  
}

############################################################################################
# Porcentajes de variables con cierta cantidad de NAs [) ----


cuantiadeNAS <- function(df, variable, ip, fp) {
  
  count = nrow(df[df[[variable]] >= ip  & df[[variable]] <fp , ])
  
  return(count)
  
}
 
cuantiadeNAS2 <- function(df, numcol, ip, fp) {
  
  count = nrow(df[df[,numcol] >= ip  & df[,numcol] <fp , ])
  
  return(count)
  
}

############################################################################################

# Resumen general ----
resumen = function(df){
  
  res = list("Total de observaciones" = nrow(df), "Total de columnas" = ncol(df),
             "Total de variables"= ncol(df)-3,
             "Total de paises" = length(unique(df$iso)), "Total de NAS" = sum(is.na(df)))
  return(res)
}


# Resumen por fila ----
#NAS por fila

resumenporfila = function(df){
  nasperrow = data.frame("cantidadNAs" = apply(df, 1, function(x) sum(is.na(x))))
  nasporfila = cbind(df$iso, df$country, nasperrow)
  
  return(nasporfila)
}


# Resumen por pais ----
#NAS por pais

resumenporpais = function(df){
  obs = aggregate(year ~ country, data = df, FUN = length) #contamos la cantidad de años por pais
  nasporfila = resumenporfila(df) #tomamos los nas por fila
  nasporpais2 = aggregate(cantidadNAs ~ df$country, data = nasporfila, FUN = sum)[2] #sumamos los nas por pais
  
  res = cbind("pais"=obs[,1], "cant años" = obs[,2], "NAs por pais" = nasporpais2)
  
  varorden = res[order(res[,3]),] #ordenamos las variables por cantidad de NAs
  
  resumen = cbind(res, varorden[,colnames(varorden)]) #ponemos todo en el mismo coso
  
  return(resumen)
  
}

resumenporyear = function(df){
  obs = aggregate(year ~ country, data = df, FUN = length) #contamos la cantidad de años por pais
  nasporfila = resumenporfila(df) #tomamos los nas por fila
  nasporpais2 = aggregate(cantidadNAs ~ df$country, data = nasporfila, FUN = sum)[2] #sumamos los nas por pais
  
  res = cbind("pais"=obs[,1], "cant años" = obs[,2], "NAs por pais" = nasporpais2)
  
  varorden = res[order(res[,2]),] #ordenamos las variables por cantidad de NAs
  
  resumen = cbind(res, varorden[,colnames(varorden)]) #ponemos todo en el mismo coso
  
  return(resumen)
  
}

# Resumen ampliado ----
# Cantidad de NAS por pais y tambien por variable
resumenampliado = function(df){
  res = resumenporpais(df)
  nas = aggregateNAsOver(df, 4, 156, "country")
  
  resumen = cbind(res, nas[,colnames(nas)])  
  
  return(resumen)
}


 
#Resumen por variable----

resumenporvariable = function(df){
  naporvar = sapply(df, function(y) sum(is.na(y)))  #cantidad de NAs por variable
  pertotal = round((naporvar/249)*100,2) #Cantidad de NAs en porcentaje del total
  
  
  res = data.frame("nas" = naporvar, "porcentaje"=pertotal) #ordenamos las variables por porcentaje de NAs
  
  varorden = res[order(res[,2]),] #ordenamos las variables por porcentaje de NAs
  
  resumen = cbind(res, rownames(varorden),varorden[,colnames(varorden)]) #ponemos todo en el mismo coso
  
  return(resumen)
}


 













