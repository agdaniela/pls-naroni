########################################################################################

source("funciones.R")

########################################################################################
# Datos ----
plsdata = dataprep("plsdata.txt")

########################################################################################
# Función para sacar dataframes  1 - 25 con NAs

selectrawdfs = function(df, num){
  # (1) Data original ----
  if (num == 1){
    datafr = df
  }
  # (2) Datos menos los paises que tienen una sola observacion (años) ----
  else if (num == 2){
    datafr = paisessinunaobs(df)
  }
  # (3) Datos menos los paises que  tienen mas de 60 NAS ----
  else if (num == 3){
    datafr = data3(df)
  }
  # (4) Datos menos los paises/obs que haga a la/s variables tener un NA (8 variables) ----
  else if (num == 4){
    datafr = data4(df)
  }
  # (5) Idem (5 variables con 1 NA) ----
  else if (num == 5){
    datafr = data5(df)
  }
  # (6) Idem (2 variables con 1 NA) ----
  else if (num == 6){
    datafr = data6(df)
  }
  # (7) Idem (1variables con 1 NA) ----
  else if (num == 7){
    datafr = data7(df)
  }
  
  # (8) Idem (35 variables con 2 NA) ----
  else if (num == 8){
    datafr = data8(df)
  }
  
  # (9) idem (6 variables con 1 NA) ----
  else if (num == 9){
    datafr = data9(df)
  }
  # (10) Idem (1 variables con 1 NA) ----
  else if (num == 10){
    datafr = data10(df)
  }
  # (11) Idem (2 variables con 3 NA) ----
  else if (num == 11){
    datafr = data11(df)
  }
  # (12) Idem (4 variables con 4 NA) ----
  else if (num == 12){
    datafr = data12(df)
  }
  # (13) Idem (1 variables con 1 NA) ----
  else if (num == 13){
    datafr = data13(df)
  }
  # (14) Idem (1 variables con 3 NA) ----
  else if (num == 14){
    datafr = data14(df)
  }
  # (15) Idem (3 variables con 1 NA) ----
  else if (num == 15){
    datafr = data15(df)
  }
  # (16) Idem (2 variables con 4 NA) ----
  else if (num == 16){
    datafr = data16(df)
  }
  # (17) Idem (2 variables con 3 NA) ----
  else if (num == 17){
    datafr = data17(df)
  }
  # (18) Idem (9 variables con 4 NA) ----
  else if (num == 18){
    datafr = data18(df)
  }
  # (19) Idem (1 variables con 2 NA) ----
  else if (num == 19){
    datafr = data19(df)
  }
  # (20) idem (1 variables con 2 NA) ----
  else if (num == 20){
    datafr = data20(df)
  }
  # (21) idem (1 variables con 3 NA)----
  else if (num == 21){
    datafr = data21(df)
  }
  # (22) idem (1 variables con 2 NA)  ----
  else if (num == 22){
    datafr = data22(df)
  }
  # (23) idem (1 variables con 6 NA)  ----
  else if (num == 23){
    datafr = data23(df)
  }
  # (24) idem (18 variables con 7 NA) ----
  else if (num == 24){
    datafr = data24(df)
  }
  # (25) Data sacando las  ́ultimas variables----
  else if (num == 25){
    datafr = data25(df)
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


#checks  
#try = sacarnas(plsdata)
#try2 = selectdfs(plsdata,1)
#all.equal(try2, try)
#try = sacarnas(selectrawdfs(plsdata,5))
#try2 = selectdfs(plsdata,5)
#all.equal(try2, try)


########################################################################################
# Función para sacar los paises dejados en dataframes  1 - 25 con NAS

olvidadosrawdfs = function(df, num){
  # (1) Data original ----
  if (num == 1){
    datafr = df
  }
  # (2) Datos menos los paises que tienen una sola observacion (años) ----
  else if (num == 2){
    datafr = paisesconunaobs(df)
  }
  # (3) Datos menos los paises que  tienen mas de 60 NAS ----
  else if (num == 3){
    datafr = nodata3(df)
  }
  # (4) Datos menos los paises/obs que haga a la/s variables tener un NA (8 variables) ----
  else if (num == 4){
    datafr = nodata4(df)
  }
  # (5) Idem (5 variables con 1 NA) ----
  else if (num == 5){
    datafr = nodata5(df)
  }
  # (6) Idem (2 variables con 1 NA) ----
  else if (num == 6){
    datafr = nodata6(df)
  }
  # (7) Idem (1variables con 1 NA) ----
  else if (num == 7){
    datafr = nodata7(df)
  }
  
  # (8) Idem (35 variables con 2 NA) ----
  else if (num == 8){
    datafr = nodata8(df)
  }
  
  # (9) idem (6 variables con 1 NA) ----
  else if (num == 9){
    datafr = nodata9(df)
  }
  # (10) Idem (1 variables con 1 NA) ----
  else if (num == 10){
    datafr = nodata10(df)
  }
  # (11) Idem (2 variables con 3 NA) ----
  else if (num == 11){
    datafr = nodata11(df)
  }
  # (12) Idem (4 variables con 4 NA) ----
  else if (num == 12){
    datafr = nodata12(df)
  }
  # (13) Idem (1 variables con 1 NA) ----
  else if (num == 13){
    datafr = nodata13(df)
  }
  # (14) Idem (1 variables con 3 NA) ----
  else if (num == 14){
    datafr = nodata14(df)
  }
  # (15) Idem (3 variables con 1 NA) ----
  else if (num == 15){
    datafr = nodata15(df)
  }
  # (16) Idem (2 variables con 4 NA) ----
  else if (num == 16){
    datafr = nodata16(df)
  }
  # (17) Idem (2 variables con 3 NA) ----
  else if (num == 17){
    datafr = nodata17(df)
  }
  # (18) Idem (9 variables con 4 NA) ----
  else if (num == 18){
    datafr = nodata18(df)
  }
  # (19) Idem (1 variables con 2 NA) ----
  else if (num == 19){
    datafr = nodata19(df)
  }
  # (20) idem (1 variables con 2 NA) ----
  else if (num == 20){
    datafr = nodata20(df)
  }
  # (21) idem (1 variables con 3 NA)----
  else if (num == 21){
    datafr = nodata21(df)
  }
  # (22) idem (1 variables con 2 NA)  ----
  else if (num == 22){
    datafr = nodata22(df)
  }
  # (23) idem (1 variables con 6 NA)  ----
  else if (num == 23){
    datafr = nodata23(df)
  }
  # (24) idem (18 variables con 7 NA) ----
  else if (num == 24){
    datafr = nodata24(df)
  }
  # (25) Data sacando las  ́ultimas variables----
  else {
    datafr = nodata25(df)
  }
  return(datafr)
  
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





 