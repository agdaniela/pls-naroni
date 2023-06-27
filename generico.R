############################################################################################


datos1 = plsdata
datos2 = paisessinunaobs(plsdata)
datos3 = datasacandopaises(paisessinunaobs(datos2), paisesasacar(paisessinunaobs(datos2), 60))

                          
                           
############################################################################################
datossolona = function(df, cantNAS){
  vars = variablesconNAs(df,cantNAS)
  datos = cbind(df[,1:4],df[,(colnames(df) %in% vars),])
  datosunna = datos[!complete.cases(datos), ]
  return(datosunna)
}

variablesconNAs =function(df, cantdeNAs){
  res = resumenporvariable(df)
  datavariables = res[res[,4] == cantdeNAs,]
  variables = c( paste(datavariables[,3]))
  return(variables)
}

data3 = function(df){
  data = df[(duplicated(df$country)|duplicated(df$country, fromLast=TRUE)),]
  datafr = datasacandopaises(data, paisesasacar(data, 60))
  return(datafr)
} 

#paisessinunaobs = function(df){
 # paisesunaobs = paisesasacarporyear(df)
  #dfdosomas = df[!(df$country %in% paisesunaobs),]
  
#  return(dfdosomas)
  
#}



############################################################################################

# (4) Datos menos los paises/obs que haga a la/s variables tener un NA (8 variables) ----
data4 = function (df){
  data = data3(df)
  datafr = data[!(data$country %in% "Nicaragua"),]
  return(datafr)
}

 
pais = paste(datossolona(datos3,1)[1,2])
 

resumenporpais(datos3)$pais == pais

View(resumenporpais(datos3)[resumenporpais(datos3)$pais == pais,])

if (resumenporpais(datos3)[resumenporpais(datos3)$pais == pais,][1,2] == 2){
  print("ok")
} else {
  print("ojo al piojo")
}



############################################################################################
#vemos que es Nicaragua que tiene dos observaciones por lo que se va completo 
#sacamos nicaragua
datos4 = datos3[!(datos3$country %in% "Nicaragua"),]
datos4 = datos4[!(datos4$country == "Senegal" & datos4$year == 2019 ),]
# datos4 deberia tener 149



generico1 = function(df, cantNa){
  pais = paste(datossolona(df,cantNa)[1,2])
  obsdelpais = resumenporpais(df)[resumenporpais(df)$pais == pais,][1,2]
  if(obsdelpais == 2){ #si tiene dos obs se va el pais entero
    data = df[!(df$country %in% pais),]
  }else{
    print("ojo al piojo")
  }
  return(data)
 }

#try1 = generico1(data3,1)  
#all.equal(data4,try1) #ok
############################################################################################

#buscamos de nuevo variables con 1 nas
variablesconNAs(data4,1)#5
# vemos los paises que tienen el NA de la columa
View(datossolona(data4,1))
#probamos primero con un solo pais
pais = paste(datossolona(data4,1)[1,2])
# controlamos que estos paises no tengan solo dos observaciones
View(resumenporpais(data4))
resumenporpais(data3)[resumenporpais(data3)$pais == pais,][1,2]
# este tiene 3 

View(resumenporpais(datos4))
View(datossolona(datos4,1))
datos5 = datos4[!(datos4$country == "Burundi" & datos4$year == 2005 ),]
datos5 = datos5[!(datos5$country == "Belize" & datos5$year == 2006 ),]
datos5 = datos5[!(datos5$country == "Lesotho" & datos5$year == 2009 ),]
datos5 = datos5[!(datos5$country %in% "Libya"),]

try1 = data4[!(data4$country == pais & data4$year == datossolona(data4,1)[1,4] ),]
try2 = data4[!(data4$country == paises[1] & data4$year == datossolona(data4,1)[1,4] ),]

datis = datossolona(data4,1)

all.equal(data4,try1) #ok

#hay que hacer el loop con los los paises 

try = data4
df_total = data.frame()
for (i in 1:nrow(datis)){
  sacar = subset(data4, country == paste(datis["country"][i,1]) & year == datis["year"][i,1])
  df_total <- rbind(df_total,sacar)
}
# me da las cuatro filas que tengo que sacar

library(dplyr)
try = data4 %>% anti_join(df_total) 

all.equal(data5,try) #si funciona

generico2 = function(df, cantNa){
  datis = datossolona(df,cantNa)
  pais = paste(datossolona(df,cantNa)[1,2])
  obsdelpais = resumenporpais(df)[resumenporpais(df)$pais == pais,][1,2]
  if(obsdelpais == 2){ 
    data = df[!(df$country %in% pais),]
  }else{
    df_total = data.frame()
    for (i in 1:length(pais)){
      sacar = subset(df, country == paste(datis["country"][i,1]) & year == datis["year"][i,1])
      df_total <- rbind(df_total,sacar)
    }
  }
  data = df %>% anti_join(df_total) 
  return(data)
}


#try2 = generico2(data4,1) #o
#all.equal(data5,try2) #ok

#funciona


############################################################################################
#ahora tengo que poner en loop las observaciones por pais e ir probando


paises = paste(datossolona(data4,1)[,2]) #lista de paises
paises
obsdelpais = resumenporpais(data4)[resumenporpais(data4)$pais %in% paises,][,2] #cantidad de observaciones de la lista de paises
obsdelpais
combi = data.frame(paises,obsdelpais)
combi

for (i in combi[,2]){
  
  if(i == 2){ 
    print("tiene dos obs")
  }else{
    print("tiene mas de dos obs")
  }
}

#ok 

for (i in combi[,2]){ 
  if(i == 2){ 
    # aca hay que buscar el lugar de la fila para el cual i == 2, para ponerlo en paises
    # no hace falta, ya está abajo
    try3 = data4[!(data4$country %in% pais),]
    print("tiene dos obs")
    
  }else{
    print("tiene mas de dos obs")
    
  }
}




# aca tomamos el pais correspondiente 
for (i in 1:nrow(combi)) {
    
  if(combi$obsdelpais[i] == 2){
    print(paste(combi$paises[i], "tiene dos observaciones"))
  }else{
    print(paste(combi$paises[i], "tiene mas de dos observaciones"))
  }
    
}
  
############################################################################################


datos5 = datos4[!(datos4$country == "Burundi" & datos4$year == 2005 ),]
datos5 = datos5[!(datos5$country == "Belize" & datos5$year == 2006 ),]
datos5 = datos5[!(datos5$country == "Lesotho" & datos5$year == 2009 ),]
datos5 = datos5[!(datos5$country %in% "Libya"),]

paises = paste(datossolona(data4,1)[,2]) #lista de paises
paises
obsdelpais = resumenporpais(data4)[resumenporpais(data4)$pais %in% paises,][,2] #cantidad de observaciones de la lista de paises
obsdelpais
anios = datossolona(data4,1)$year
combi = data.frame(paises,obsdelpais, anios)


library(dplyr)
generico3 = function(df, cantNa){
  paises = paste(datossolona(df,cantNa)$country) #lista de paises con ciertos nas
  obsdelpais = resumenporpais(df)[resumenporpais(df)$pais %in% paises,][,2] #cantidad de observaciones de la lista de paises
  years = datossolona(df,cantNa)$year #año de la observacion
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
  data = df %>% anti_join(sacados)
  
    return(data)
}

try = generico3(data4,1) #ok
all.equal(data4,try) #ok





############################################################################################

View(resumenporvariable(data3))
max(resumenporvariable(data3)[,4])

#aca lo que hace es, si el df no tiene 1 de Nas, que siga nomas
generico4 = function(df){
  for (cantNAS in 1:max(resumenporvariable(data3)[,4])) {
    if(identical(paste(datossolona(df,cantNAS)$country), character(0))){
      print(paste("Tengo que tomar variables con", cantNAS+1, "NAs"))
      next 
    }else{ 
      datos = generico3(df,cantNAS)
      print(paste("Se tomaron variables con", cantNAS, "NAs"))
      break
    }
  }
  return(datos)
}  

try = generico4(data4)
 
  
############################################################################################
#todo en una funcion
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

try = generico5(datos4)
all.equal(datos5,try)

#ojo cuando se llegue al final
View(resumenporvariable(ojo))
try3 = generico5(selectrawdfs(plsdata,25))

 

# ahora lo que hacemos es anidar la funcion
try4 = Reduce(generico5, 1:2, init = datos3)
all.equal(datos5,try4) #ok


selecdfsconna = function(df,num){
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
  # (4) Datos menos los paises/obs que haga a la/s variables tener un NA (8 variables) ----
  else {
    data = data3(df)
    datafr = Reduce(generico5, 1:(num-3), init = data) #repito (num-3) la funcion para ir generando los dfs
    
  }
  return(datafr)
} 


all.equal(datos3,selecdfsconna(plsdata,3))

try6 = selecdfsconna(plsdata,25)
try7 = selecdfsconna(plsdata,26)


View(resumenporvariable(try6))
View(datossolona(try6,16))
try = Reduce(generico5, 1:1, init = datos3) #datos4

# aca nos damos cuenta de que, cuando saca dos obs, si el pais tiene 3, me queda una sola
# por lo que, hay que chequear si luego de sacar una obs, me queda el pais con dos


#bolean que indica si el pais tiene una sola obs en el dataframe
!(duplicated(try6$country)|duplicated(try6$country, fromLast=TRUE))
#datos de este pais con una obs
try6[!(duplicated(try6$country)|duplicated(try6$country, fromLast=TRUE)),]
#sacarlo del dataframe (me quedo con los paises que son duplicados, mas de dos obs)
try6[(duplicated(try6$country)|duplicated(try6$country, fromLast=TRUE)),]

#try = plsdata[(duplicated(plsdata$country)|duplicated(plsdata$country, fromLast=TRUE)),]
#all.equal(try,datos2) #ok
#si no hay duplicados el dataframe es el mismo y el anterior es vacio
#View(datos2[!(duplicated(datos2$country)|duplicated(datos2$country, fromLast=TRUE)),])

#para el if, veo si hay algun pais con una sola obs:
sum(!(duplicated(try6$country)|duplicated(try6$country, fromLast=TRUE)))
# si es == 0 es porque no hay paises con una sola obs



if(sum(!(duplicated(try6$country)|duplicated(try6$country, fromLast=TRUE))) != 0){
  try6 = try6[(duplicated(try6$country)|duplicated(try6$country, fromLast=TRUE)),]
  
}

if(sum(!(duplicated(datos2$country)|duplicated(datos2$country, fromLast=TRUE))) != 0){
  datos2 = datos2[(duplicated(datos2$country)|duplicated(datos2$country, fromLast=TRUE)),]
  
}






