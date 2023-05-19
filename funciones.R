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

##############################################################################################
# Resumen por fila
#NAS por fila

resumenporfila = function(df){
  nasperrow = data.frame("cantidadNAs" = apply(df, 1, function(x) sum(is.na(x))))
  nasporfila = cbind(df$iso, df$country, nasperrow)
  
  return(nasporfila)
}

##############################################################################################
# Resumen por año 

resumenporyear = function(df){
  obs = aggregate(year ~ country, data = df, FUN = length) #contamos la cantidad de años por pais
  nasporfila = resumenporfila(df) #tomamos los nas por fila
  nasporpais2 = aggregate(cantidadNAs ~ df$country, data = nasporfila, FUN = sum)[2] #sumamos los nas por pais
  
  res = cbind("pais"=obs[,1], "cant años" = obs[,2], "NAs por pais" = nasporpais2)
  
  varorden = res[order(res[,2]),] #ordenamos las variables por cantidad de NAs
  
  resumen = cbind(res, varorden[,colnames(varorden)]) #ponemos todo en el mismo coso
  
  return(resumen)
  
}

##############################################################################################

# Lista de paises que tienen una sola observacion
paisesasacarporyear =function(df){
  respais = resumenporyear(df)
  datapaises = respais[respais[,5] == 1,]
  paises = c( paste(datapaises[,4]))
  return(paises)
}

#paises con mas de dos observaciones

paisessinunaobs = function(df){
  paisesunaobs = paisesasacarporyear(df)
  dfdosomas = df[!(df$country %in% paisesunaobs),]
  
  return(dfdosomas)
  
}

paisesconunaobs = function(df){
  paisesunaobs = paisesasacarporyear(df)
  dfdosomas = df[(df$country %in% paisesunaobs),]
  
  return(dfdosomas)
  
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

datasacandopaises = function(df, paises, paisadc = NULL){
  if (is.null(paisadc) == FALSE){
    paises = c(paises, paisadc)
  }
  nuevadata = df[!(df$country %in% paises),]
  return(nuevadata)
}


############################################################################################
# Datos sacando conforme a los NAs

# (3) Datos menos los paises que  tienen mas de 60 NAS ----
data3 = function(df){
  datafr = datasacandopaises(paisessinunaobs(df), paisesasacar(paisessinunaobs(df), 60))
  return(datafr)
} 

nodata3 = function(df){
  datafr = rbind(paisesconunaobs(df), df[(df$country %in% paisesasacar(paisessinunaobs(df), 60)),])
  return(datafr)
} 
 

# (4) Datos menos los paises/obs que haga a la/s variables tener un NA (8 variables) ----
data4 = function (df){
  data = data3(df)
  datafr = data[!(data$country %in% "Nicaragua"),]
  
  return(datafr)
}

nodata4 = function (df){
  data = nodata3(df)
  datafr = rbind(data, df[(df$country %in% "Nicaragua"),])
 
  return(datafr)
}

# (5) Idem (5 variables con 1 NA) ----
data5 = function(df){
  datafr = data4(df)
  datafr = datafr[!(datafr$country == "Burundi" & datafr$year == "2005-01-01" ),]
  datafr = datafr[!(datafr$country == "Belize" & datafr$year == "2006-01-01" ),]
  datafr = datafr[!(datafr$country == "Lesotho" & datafr$year == "2009-01-01" ),]
  datafr = datafr[!(datafr$country == "Senegal" & datafr$year == "2019-01-01" ),]
  return(datafr)
}

nodata5 = function (df){
  data = nodata4(df)
  datafr = data
  
  return(datafr)
}

# (6) Idem (2 variables con 1 NA) ----
data6 = function(df){
  datafr = data5(df)
  datafr = datafr[!(datafr$country %in% "Burundi"),]
  datafr = datafr[!(datafr$country %in% "Libya"),]
  
  return(datafr)
}

nodata6 = function (df){
  data = nodata5(df)
  datafr = rbind(data, df[(df$country %in% c("Burundi","Libya" )),])
  return(datafr)
}
 
# (7) Idem (1variables con 1 NA) ---- 
data7 = function(df){
  datafr = data6(df)
  datafr = datafr[!(datafr$country == "Rwanda" & datafr$year == "2020-01-01" ),]
return(datafr)
}

nodata7 = function (df){
  data = nodata6(df)
  datafr = data
  return(datafr)
}

# (8) Idem (35 variables con 2 NA) ---- 

data8 = function(df){
  datafr = data7(df)
  datafr =  datafr[!(datafr$country %in% c("Argentina", "Armenia", "Lao PDR", "Lesotho", "Mozambique", "Rwanda", "Trinidad and Tobago" )),]
  datafr = datafr[!(datafr$country == "Mexico" & datafr$year == "2020-01-01" ),]
  
  return(datafr)
}

nodata8 = function (df){
  data = nodata7(df)
  datafr = rbind(data, df[(df$country %in% c("Argentina", "Armenia", "Lao PDR", "Lesotho", "Mozambique", "Rwanda", "Trinidad and Tobago" )),])
  return(datafr)
}


# (9) idem (6 variables con 1 NA) ----
data9 = function(df){
  datafr = data8(df)
  datafr = datafr[!(datafr$country %in% c("Angola", "Guinea-Bissau")),]
  datafr = datafr[!(datafr$country == "Dominican Republic" & datafr$year == "2000-01-01" ),]
  
  return(datafr)
}

nodata9 = function (df){
  data = nodata8(df)
  datafr = rbind(data, df[(df$country %in% c("Angola", "Guinea-Bissau" )),])
  return(datafr)
}

# (10) Idem (1 variables con 1 NA) ----
data10 = function(df){
  datafr = data9(df)
  datafr = datafr[!(datafr$country %in% c("Belize")),]

  return(datafr)
}

nodata10 = function (df){
  data = nodata9(df)
  datafr = rbind(data, df[(df$country %in% c("Belize" )),])
  return(datafr)
}

# (11) Idem (2 variables con 3 NA) ----
data11 = function(df){
  datafr = data10(df)
  datafr = datafr[!(datafr$country %in% c("Ghana")),] 
  datafr = datafr[!(datafr$country == "Nigeria" & datafr$year == "2003-01-01" ),]
  
  
  return(datafr)
}

nodata11 = function (df){
  data = nodata10(df)
  datafr = rbind(data, df[(df$country %in% c("Ghana" )),])
  return(datafr)
}

# (12) Idem (4 variables con 4 NA) ----
data12 = function(df){
  datafr = data11(df)
  datafr = datafr[!(datafr$country %in% c("China", "Kenya", "Cambodia", "Tajikistan")),]
  datafr = datafr[!(datafr$country == "Bangladesh" & datafr$year == "2007-01-01" ),]
  datafr = datafr[!(datafr$country == "Peru" & datafr$year == "2004-01-01" ),]
  datafr = datafr[!(datafr$country == "Philippines" & datafr$year == "2005-01-01" ),]
  
  return(datafr)
}

nodata12 = function (df){
  data = nodata11(df)
  datafr = rbind(data, df[(df$country %in% c("China", "Kenya", "Cambodia", "Tajikistan")),])
  return(datafr)
}
 
# (13) Idem (1 variables con 1 NA) ----
data13 = function(df){
  datafr = data12(df)
  datafr = datafr[!(datafr$country == "Philippines" & datafr$year == "2003-01-01" ),]
  
  return(datafr)
}

nodata13 = function (df){
  data = nodata12(df)
  datafr = data
  return(datafr)
}

# (14) Idem (1 variables con 3 NA) ----
data14 = function(df){
  datafr = data13(df)
  datafr = datafr[!(datafr$country %in% c("Georgia", "Zambia")),]
  datafr = datafr[!(datafr$country == "Mongolia" & datafr$year == "2018-01-01" ),]
  
  return(datafr)
}

nodata14 = function (df){
  data = nodata13(df)
  datafr = rbind(data, df[(df$country %in% c("Georgia", "Zambia")),])
  return(datafr)
}

# (15) Idem (3 variables con 1 NA) ----
data15 = function(df){
  datafr = data14(df)
  datafr = datafr[!(datafr$country %in% c("Mongolia")),]
  
  return(datafr)
}

nodata15 = function (df){
  data = nodata14(df)
  datafr = rbind(data, df[(df$country %in% c("Mongolia")),])
  return(datafr)
}

# (16) Idem (2 variables con 4 NA) ----
data16 = function(df){
  datafr = data15(df)
  datafr = datafr[!(datafr$country %in% c("Jamaica")),]
  datafr = datafr[!(datafr$country == "Dominican Republic" & datafr$year == "2019-01-01" ),]
  
  return(datafr)
}
nodata16 = function (df){
  data = nodata15(df)
  datafr = rbind(data, df[(df$country %in% c("Jamaica")),])
  return(datafr)
}  

# (17) Idem (2 variables con 3 NA) ----
data17 = function(df){
  datafr = data16(df)
  datafr = datafr[!(datafr$country %in% c("Dominican Republic")),]
  datafr = datafr[!(datafr$country == "Mali" & datafr$year == "2015-01-01" ),]
  
  return(datafr)
}
nodata17 = function (df){
  data = nodata16(df)
  datafr = rbind(data, df[(df$country %in% c("Dominican Republic")),])
  return(datafr)
}

# (18) Idem (9 variables con 4 NA) ----
data18 = function(df){
  datafr = data17(df)
  datafr = datafr[!(datafr$country == "Bangladesh" & datafr$year == "2019-01-01" ),]
  datafr = datafr[!(datafr$country == "Mali" & datafr$year == "2013-01-01" ),]
  datafr = datafr[!(datafr$country == "Nepal" & datafr$year == "2006-01-01" ),]
  datafr = datafr[!(datafr$country == "Togo" & datafr$year == "2006-01-01" ),]
  
  
  return(datafr)
}

nodata18 = function (df){
  data = nodata17(df)
  datafr = data
  return(datafr)
}

# (19) Idem (1 variables con 2 NA) ----
data19 = function(df){
  datafr = data18(df)
  datafr = datafr[!(datafr$country == "Nepal" & datafr$year == "2016-01-01" ),]
  datafr = datafr[!(datafr$country == "Nepal" & datafr$year == "2019-01-01" ),]
  return(datafr)
}
nodata19 = function (df){
  data = nodata18(df)
  datafr = data
  return(datafr)
}

# (20) idem (1 variables con 2 NA) ----
data20 = function(df){
  datafr = data19(df)
  datafr = datafr[!(datafr$country %in% c("Mali")),]
  datafr = datafr[!(datafr$country == "Cameroon" & datafr$year == "2018-01-01" ),]
  
  return(datafr)
}

nodata20 = function (df){
  data = nodata19(df)
  datafr = rbind(data, df[(df$country %in% c("Mali")),])
  return(datafr)
}

# (21) idem (1 variables con 3 NA)----
data21 = function(df){
  datafr = data20(df)
  datafr = datafr[!(datafr$country %in% c("Madagascar")),]
  datafr = datafr[!(datafr$country == "Nigeria" & datafr$year == "2017-01-01" ),]
  datafr = datafr[!(datafr$country == "Nigeria" & datafr$year == "2018-01-01" ),]
  
  return(datafr)
}
nodata21 = function (df){
  data = nodata20(df)
  datafr = rbind(data, df[(df$country %in% c("Madagascar")),])
  return(datafr)
}

# (22) idem (1 variables con 2 NA)  ----
data22 = function(df){
  datafr = data21(df)
  datafr = datafr[!(datafr$country %in% c("Pakistan")),]
  datafr = datafr[!(datafr$country == "Cameroon" & datafr$year == "2004-01-01" ),]
  
  return(datafr)
}
nodata22 = function (df){
  data = nodata21(df)
  datafr = rbind(data, df[(df$country %in% c("Pakistan")),])
  return(datafr)
}
# (23) idem (1 variables con 6 NA)  ----
data23 = function(df){
  datafr = data22(df)
  datafr = datafr[!(datafr$country %in% c("Benin")),]
  datafr = datafr[!(datafr$country %in% c("Togo")),]
  
  
  return(datafr)
}
nodata23 = function (df){
  data = nodata22(df)
  datafr = rbind(data, df[(df$country %in% c("Benin", "Togo")),])
  return(datafr)
}

# (24) idem (18 variables con 7 NA) ----
data24 = function(df){
  datafr = data23(df)
  datafr = datafr[!(datafr$country %in% c("Honduras", "Kazakhstan", "Sri Lanka", "Morocco", "Tunisia")),]
  datafr = datafr[!(datafr$country == "Jordan" & datafr$year == "2018-01-01" ),]
  
  return(datafr)
}
nodata24 = function (df){
  data = nodata23(df)
  datafr = rbind(data, df[(df$country %in% c("Honduras", "Kazakhstan", "Sri Lanka", "Morocco", "Tunisia")),])
  return(datafr)
}
# (25) datos sin las ultimas 4 variables  ----
data25 = function(df){
  datafr = data24(df)
  datafr$Arms.imports..SIPRI.trend.indicator.values. = NULL
  datafr$Energy.imports..net....of.energy.use. = NULL
  datafr$Agricultural.nitrous.oxide.emissions....of.total. = NULL
  datafr$Agricultural.methane.emissions....of.total. = NULL
  return(datafr)
}
nodata25 = function (df){
  data = nodata24(df)
  datafr = data
  return(datafr)
}

 
 





############################################################################################

# sacando los NAs

sacarnas = function(df){
  datafr = df[ , apply(df, 2, function(x) !any(is.na(x)))]
  
  return(datafr)
}













