############################################################################################
# Preparación de los datos ----

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


##############################################################################################
# Funcion con el nombre de las variables con cierta cantidad de NAS
variablesconNAs =function(df, cantdeNAs){
  res = resumenporvariable(df)
  datavariables = res[res[,4] == cantdeNAs,]
  variables = c( paste(datavariables[,3]))
  return(variables)
}

#funcion que me muestra las columnas que tienen cierta cantidad de NA
datosconunna = function(df, cantNAS){
  vars = variablesconNAs(df,cantNAS)
  datos = cbind(df[,1:4],df[,(colnames(df) %in% vars),])
  return(datos)
}

#funcion que me muestra las filas (paises) cuyas columnas tienen cierta cant de NA
datossolona = function(df, cantNAS){
  vars = variablesconNAs(df,cantNAS)
  datos = cbind(df[,1:4],df[,(colnames(df) %in% vars),])
  datosunna = datos[!complete.cases(datos), ]
  return(datosunna)
}


############################################################################################
# Datos sacando conforme a los NAs

# (2) Datos menos los paises que  tienen mas de 60 NAS ----
unadata2 = function(df){
  datafr = datasacandopaises(df, paisesasacar(df, 60))
  return(datafr)
} 

unanodata2 = function(df){
  datafr = rbind(df, df[(df$country %in% paisesasacar(df, 60)),])
  return(datafr)
} 


# (3) Datos menos los paises/obs que haga a la/s variables tener un NA (11 variables) ----
View(resumenporvariable(unadata2(plsdata)))
# encontramos las variables con un NAs
variablesconNAs(unadata2(plsdata),1) 
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata2(plsdata),1))
# saco las observaciones, no importa si el pais tiene dos observaciones

unadata3 = function (df){
  datafr = unadata2(df)
  datafr = datafr[!(datafr$country == "Nicaragua" & datafr$year == 2001 ),]
  datafr = datafr[!(datafr$country == "Sudan" & datafr$year == 2010 ),]
  
  return(datafr)
}

View(unadata3(plsdata))
View(resumenporvariable(unadata3(plsdata)))

# (4) Idem (4 variables con 1 NA) ----
View(resumenporvariable(unadata3(plsdata)))
# encontramos las variables con un NAs
variablesconNAs(unadata3(plsdata),1) 
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata3(plsdata),1))


unadata4 = function(df){
  datafr = unadata3(df)
  datafr = datafr[!(datafr$country == "Burundi" & datafr$year == 2005 ),]
  datafr = datafr[!(datafr$country == "Lesotho" & datafr$year == 2009 ),]
  datafr = datafr[!(datafr$country == "Myanmar" & datafr$year == 2000 ),]
  return(datafr)
}

View(unadata4(plsdata))
View(resumenporvariable(unadata4(plsdata)))


# (5) Idem (1 variables con 1 NA) ----
View(resumenporvariable(unadata4(plsdata)))
# encontramos las variables con un NAs
variablesconNAs(unadata4(plsdata),1) 
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata4(plsdata),1))
# controlo que los paises tengan mas de 2 
View(datosconunna(unadata4(plsdata),1))


unadata5 = function(df){
  datafr = unadata4(df)
  datafr = datafr[!(datafr$country == "Burundi" & datafr$year == 2010 ),]

  return(datafr)
}


 
# (6) Idem (5 variables con 2 NA) ---- 
View(resumenporvariable(unadata5(plsdata)))
# encontramos las variables con un NAs
variablesconNAs(unadata5(plsdata),2) 
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata5(plsdata),2))


unadata6 = function(df){
  datafr = unadata5(df)
  datafr = datafr[!(datafr$country == "United Arab Emirates" & datafr$year == 2003 ),]
  datafr = datafr[!(datafr$country == "Armenia" & datafr$year == 2005 ),]
  datafr = datafr[!(datafr$country == "Armenia" & datafr$year == 2010 ),]
  datafr = datafr[!(datafr$country == "Burkina Faso" & datafr$year == 2006 ),]
  datafr = datafr[!(datafr$country == "Belize" & datafr$year == 2006 ),]
  datafr = datafr[!(datafr$country == "Kiribati" & datafr$year == 2019 ),]
  datafr = datafr[!(datafr$country == "Senegal" & datafr$year == 2019 ),]
  
  return(datafr)
}


# (7) Idem (2 variables con 1 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata6(plsdata),1) 
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata6(plsdata),1))


unadata7 = function(df){
  datafr = unadata6(df)
  datafr = datafr[!(datafr$country == "Libya" & datafr$year == 2014 ),]
  datafr = datafr[!(datafr$country == "Rwanda" & datafr$year == 2020 ),]
 
  return(datafr)
}

View(unadata7(plsdata))


# (8) Idem (14 variables con 2 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata7(plsdata),2) 
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata7(plsdata),2))


unadata8 = function(df){
  datafr = unadata7(df)
  datafr = datafr[!(datafr$country == "Afghanistan" & datafr$year == 2011 ),]
  datafr = datafr[!(datafr$country == "Mozambique" & datafr$year == 2003 ),]
  datafr = datafr[!(datafr$country == "Nicaragua" & datafr$year == 2007 ),]
  datafr = datafr[!(datafr$country == "Rwanda" & datafr$year == 2005 ),]
  
  return(datafr)
}


variablesconNAs(unadata8(plsdata),2) 

# (9) Idem (11 variables con 2 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata8(plsdata),2) 
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata8(plsdata),2))


unadata9 = function(df){
  datafr = unadata8(df)
  datafr = datafr[!(datafr$country == "Estonia" & datafr$year == 2003 ),]
  datafr = datafr[!(datafr$country == "Lao PDR" & datafr$year == 2017 ),]
  datafr = datafr[!(datafr$country == "Latvia" & datafr$year == 2003 ),]
  datafr = datafr[!(datafr$country == "Samoa" & datafr$year == 2020 ),]
  
  return(datafr)
}


variablesconNAs(unadata9(plsdata),2) 


# (10) Idem (6 variables con 1 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata9(plsdata),1) 
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata9(plsdata),1))


unadata10 = function(df){
  datafr = unadata9(df)
  datafr = datafr[!(datafr$country == "Angola" & datafr$year == 2001 ),]
  datafr = datafr[!(datafr$country == "Dominican Republic" & datafr$year == 2000 ),]
  datafr = datafr[!(datafr$country == "Guinea-Bissau" & datafr$year == 2006 ),]

  return(datafr)
}


variablesconNAs(unadata10(plsdata),1) 


# (11) Idem (5 variables con 2 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata10(plsdata),2) 
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata10(plsdata),2))


unadata11 = function(df){
  datafr = unadata10(df)
  datafr = datafr[!(datafr$country == "Argentina" & datafr$year == 2020 ),]
  datafr = datafr[!(datafr$country == "Mexico" & datafr$year == 2020 ),]
  datafr = datafr[!(datafr$country == "Seychelles" & datafr$year == 2019 ),]
  datafr = datafr[!(datafr$country == "Tonga" & datafr$year == 2019 ),]
  
  return(datafr)
}


variablesconNAs(unadata11(plsdata),2) 

# (12) Idem (2 variables con 2 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata11(plsdata),2) 
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata11(plsdata),2))


unadata12 = function(df){
  datafr = unadata11(df)
  datafr = datafr[!(datafr$country == "Belize" & datafr$year == 2011 ),]
  datafr = datafr[!(datafr$country == "Lesotho" & datafr$year == 2004 ),]
  datafr = datafr[!(datafr$country == "Maldives" & datafr$year == 2017 ),]

  return(datafr)
}


variablesconNAs(unadata12(plsdata),2) 

# (13) Idem (10 variables con 3 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata12(plsdata),3) 
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata12(plsdata),3))


unadata13 = function(df){
  datafr = unadata12(df)
  datafr = datafr[!(datafr$country == "Ghana" & datafr$year == 2008 ),]
  datafr = datafr[!(datafr$country == "Ghana" & datafr$year == 2011 ),]
  datafr = datafr[!(datafr$country == "Guinea-Bissau" & datafr$year == 2019 ),]
  datafr = datafr[!(datafr$country == "Lao PDR" & datafr$year == 2006 ),]
  datafr = datafr[!(datafr$country == "Lesotho" & datafr$year == 2018 ),]
  datafr = datafr[!(datafr$country == "Nigeria" & datafr$year == 2003 ),]
  datafr = datafr[!(datafr$country == "Papua New Guinea" & datafr$year == 2018 ),]
  datafr = datafr[!(datafr$country == "Trinidad and Tobago" & datafr$year == 2006 ),]
  datafr = datafr[!(datafr$country == "Trinidad and Tobago" & datafr$year == 2011 ),]
  
  return(datafr)
}


variablesconNAs(unadata13(plsdata),3) 

# (14) Idem (2 variables con 4 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata13(plsdata),4) 
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata13(plsdata),4))


unadata14 = function(df){
  datafr = unadata13(df)
  datafr = datafr[!(datafr$country == "Croatia" & datafr$year == 2003 ),]
  datafr = datafr[!(datafr$country == "Hungary" & datafr$year == 2003 ),]
  datafr = datafr[!(datafr$country == "Slovenia" & datafr$year == 2003 ),]
  datafr = datafr[!(datafr$country == "Uruguay" & datafr$year == 2003 ),]
  
  return(datafr)
}


variablesconNAs(unadata14(plsdata),4) 


# (15) Idem (1 variables con 5 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata14(plsdata),5) 
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata14(plsdata),5))


unadata15 = function(df){
  datafr = unadata14(df)
  datafr = datafr[!(datafr$country == "Bangladesh" & datafr$year == 2007 ),]
  datafr = datafr[!(datafr$country == "Bosnia and Herzegovina" & datafr$year == 2006 ),]
  datafr = datafr[!(datafr$country == "Cambodia" & datafr$year == 2004 ),]
  datafr = datafr[!(datafr$country == "Peru" & datafr$year == 2003 ),]
  datafr = datafr[!(datafr$country == "Philippines" & datafr$year == 2002 ),]
  
  return(datafr)
}


variablesconNAs(unadata15(plsdata),5) 


# (16) Idem (1 variables con 3 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata15(plsdata),3)
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata15(plsdata),3))


unadata16 = function(df){
  datafr = unadata15(df)
  datafr = datafr[!(datafr$country == "Cambodia" & datafr$year == 2005 ),]
  datafr = datafr[!(datafr$country == "Peru" & datafr$year == 2004 ),]
  datafr = datafr[!(datafr$country == "Philippines" & datafr$year == 2003 ),]
  
  return(datafr)
}


variablesconNAs(unadata16(plsdata),3) 


# (17) Idem (5 variables con 6 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata16(plsdata),6)
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata16(plsdata),6))


unadata17 = function(df){
  datafr = unadata16(df)
  datafr = datafr[!(datafr$country == "Angola" & datafr$year == 2016 ),]
  datafr = datafr[!(datafr$country == "Burundi" & datafr$year == 2017 ),]
  datafr = datafr[!(datafr$country == "China" & datafr$year == 2002 ),]
  datafr = datafr[!(datafr$country == "China" & datafr$year == 2012 ),]
  datafr = datafr[!(datafr$country == "Georgia" & datafr$year == 2005 ),]
  datafr = datafr[!(datafr$country == "Kenya" & datafr$year == 2003 ),]
  datafr = datafr[!(datafr$country == "Kenya" & datafr$year == 2009 ),]
  datafr = datafr[!(datafr$country == "Madagascar" & datafr$year == 2018 ),]
  datafr = datafr[!(datafr$country == "Mali" & datafr$year == 2018 ),]
  datafr = datafr[!(datafr$country == "Mongolia" & datafr$year == 2005 ),]
  datafr = datafr[!(datafr$country == "Namibia" & datafr$year == 2007 ),]
  datafr = datafr[!(datafr$country == "Nigeria" & datafr$year == 2017 ),]
  datafr = datafr[!(datafr$country == "Nigeria" & datafr$year == 2018 ),]
  datafr = datafr[!(datafr$country == "Nepal" & datafr$year == 2019 ),]
  datafr = datafr[!(datafr$country == "Tajikistan" & datafr$year == 2005 ),]
  datafr = datafr[!(datafr$country == "Tajikistan" & datafr$year == 2017 ),]
  datafr = datafr[!(datafr$country == "Zambia" & datafr$year == 2018 ),]
  
  return(datafr)
}


variablesconNAs(unadata17(plsdata),6) 


# (18) Idem (4 variables con 1 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata17(plsdata),1)
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata17(plsdata),1))


unadata18 = function(df){
  datafr = unadata17(df)
  datafr = datafr[!(datafr$country == "Mongolia" & datafr$year == 2010 ),]
  datafr = datafr[!(datafr$country == "Zambia" & datafr$year == 2007 ),]
  
  return(datafr)
}


variablesconNAs(unadata18(plsdata),1) 

# (19) Idem ( 1 variables con 4 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata18(plsdata),4)
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata18(plsdata),4))


unadata19 = function(df){
  datafr = unadata18(df)
  datafr = datafr[!(datafr$country == "Cameroon" & datafr$year == 2004 ),]
  datafr = datafr[!(datafr$country == "Ghana" & datafr$year == 2018 ),]
  datafr = datafr[!(datafr$country == "Madagascar" & datafr$year == 2004 ),]
  datafr = datafr[!(datafr$country == "Pakistan" & datafr$year == 2018 ),]
  
  return(datafr)
}


variablesconNAs(unadata19(plsdata),4) 

# (20) Idem ( 1 variables con 4 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata19(plsdata),5)
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata19(plsdata),5))


unadata20 = function(df){
  datafr = unadata19(df)
  datafr = datafr[!(datafr$country == "Belize" & datafr$year == 2006 ),]
  datafr = datafr[!(datafr$country == "Dominican Republic" & datafr$year == 2019 ),]
  datafr = datafr[!(datafr$country == "Jamaica" & datafr$year == 2012 ),]
  datafr = datafr[!(datafr$country == "Jamaica" & datafr$year == 2014 ),]
  datafr = datafr[!(datafr$country == "Jamaica" & datafr$year == 2018 ),]
  
  return(datafr)
}


variablesconNAs(unadata20(plsdata),5) 

# (21) Idem ( 1 variables con 4 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata20(plsdata),1)
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata20(plsdata),1))


unadata21 = function(df){
  datafr = unadata20(df)
  datafr = datafr[!(datafr$country == "Belize" & datafr$year == 2016 ),]
  
  return(datafr)
}


variablesconNAs(unadata21(plsdata),1) 


# (22) Idem ( 1 variables con 4 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata21(plsdata),3)
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata21(plsdata),3))


unadata22 = function(df){
  datafr = unadata21(df)
  datafr = datafr[!(datafr$country == "Dominican Republic" & datafr$year == 2007 ),]
  datafr = datafr[!(datafr$country == "Dominican Republic" & datafr$year == 2013 ),]
  datafr = datafr[!(datafr$country == "Mali" & datafr$year == 2015 ),]
  
  return(datafr)
}


variablesconNAs(unadata22(plsdata),3) 


# (22) Idem ( 1 variables con 4 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata21(plsdata),3)
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata21(plsdata),3))


unadata22 = function(df){
  datafr = unadata21(df)
  datafr = datafr[!(datafr$country == "Dominican Republic" & datafr$year == 2007 ),]
  datafr = datafr[!(datafr$country == "Dominican Republic" & datafr$year == 2013 ),]
  datafr = datafr[!(datafr$country == "Mali" & datafr$year == 2015 ),]
  
  return(datafr)
}


variablesconNAs(unadata22(plsdata),3) 


# (23) Idem ( 9 variables con 5 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata22(plsdata),5)
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata22(plsdata),5))


unadata23 = function(df){
  datafr = unadata22(df)
  datafr = datafr[!(datafr$country == "Bangladesh" & datafr$year == 2019 ),]
  datafr = datafr[!(datafr$country == "Algeria" & datafr$year == 2019 ),]
  datafr = datafr[!(datafr$country == "Mali" & datafr$year == 2013 ),]
  datafr = datafr[!(datafr$country == "Nepal" & datafr$year == 2006 ),]
  datafr = datafr[!(datafr$country == "Togo" & datafr$year == 2006 ),]
  
  return(datafr)
}


variablesconNAs(unadata23(plsdata),5) 

View(resumenporvariable(unadata23(plsdata)))


# (24) Idem ( 2 variables con 1 NA) ---- 
# encontramos las variables con un NAs
variablesconNAs(unadata23(plsdata),1)
# vemos los paises que tienen el NA de la columa
View(datossolona(unadata23(plsdata),1))


unadata24 = function(df){
  datafr = unadata23(df)
  datafr = datafr[!(datafr$country == "Cameroon" & datafr$year == 2018 ),]
  datafr = datafr[!(datafr$country == "Nepal" & datafr$year == 2016 ),]

  return(datafr)
}


variablesconNAs(unadata24(plsdata),1) 

View(resumenporvariable(unadata24(plsdata)))
