############################################################################################



data1 = plsdata
data2 = paisessinunaobs(plsdata)

#datos sin paises con mas de 60 NAS
data3 = function(df){
  datafr = datasacandopaises(paisessinunaobs(df), paisesasacar(paisessinunaobs(df), 60))
  return(datafr)
} 


data3 = data3(data2)
############################################################################################

# (4) Datos menos los paises/obs que haga a la/s variables tener un NA (8 variables) ----
data4 = function (df){
  data = data3(df)
  datafr = data[!(data$country %in% "Nicaragua"),]
  return(datafr)
}



View(resumenporvariable(plsdata)) # ocho variable con un NA
variablesconNAs(data3,1) #8
# vemos los paises que tienen el NA de la columa
View(datossolona(data3,1))
pais = paste(datossolona(data3,1)[1,2])

# vemos que onda las otras observaciones

View(datosconunna(data3,1))
View(resumenporpais(data3))

resumenporpais(data3)$pais == pais

View(resumenporpais(data3)[resumenporpais(data3)$pais == pais,])

if (resumenporpais(data3)[resumenporpais(data3)$pais == pais,][1,2] == 2){
  print("ok")
} else {
  print("ojo al piojo")
}



############################################################################################
#vemos que es Nicaragua que tiene dos observaciones por lo que se va completo 
#sacamos nicaragua
data4 = data3[!(data3$country %in% "Nicaragua"),]
 

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

try1 = generico1(data3,1)  
all.equal(data4,try1) #ok
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


# todos tienen más de 2 por lo que hay que eliminar solo ese año para cada uno
plsdata602 = plsdata601[!(plsdata601$country == "Burundi" & plsdata601$year == "2005-01-01" ),]

data5 = data4[!(data4$country == "Burundi" & data4$year == 2005 ),]
data5 = data5[!(data5$country == "Belize" & data5$year == 2006 ),]
data5 = data5[!(data5$country == "Lesotho" & data5$year == 2009 ),]
data5 = data5[!(data5$country == "Senegal" & data5$year == 2019 ),]


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

data5 = data4[!(data4$country == "Burundi" & data4$year == 2005 ),]
data5 = data5[!(data5$country == "Belize" & data5$year == 2006 ),]
data5 = data5[!(data5$country == "Lesotho" & data5$year == 2009 ),]
data5 = data5[!(data5$country == "Senegal" & data5$year == 2019 ),]

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

generico5 = function(df){
  for (cantNAS in 1:max(resumenporvariable(data3)[,4])) {
    if(identical(paste(datossolona(df,cantNAS)$country), character(0))){
      print(paste("Tengo que tomar variables con", cantNAS+1, "NAs"))
      next 
      
    }else{ 
      
      paises = paste(datossolona(df,cantNAS)$country) #lista de paises con ciertos nas
      obsdelpais = resumenporpais(df)[resumenporpais(df)$pais %in% paises,][,2] #cantidad de observaciones de la lista de paises
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
      
      print(paste("Se tomaron variables con", cantNAS, "NAs"))
      break
    }
  }
  return(datos)
} 

try = generico5(data4)

#ojo cuando se llegue al final

ojo = data25(plsdata)




































