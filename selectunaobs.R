# dataframes incluyendo los paises con una observacion, sacando NAS conforme a la cantidad de NAs por variable


source("unaobs.R")

########################################################################################
# Datos ----
plsdata = dataprep("plsdata.txt")

########################################################################################
unaobselectraw = function(df, num){
  # (1) Data original ----
  if (num == 1){
    datafr = df
  }
  # (2) Datos menos los paises que  tienen mas de 60 NAS ----
  else if (num == 2){
    datafr = unadata2(df)
  }
  # (3) Datos menos los paises/obs que haga a la/s variables tener un NA (11 variables) ----
  else if (num == 3){
    datafr = unadata3(df)
  }
  # (4)  (4 variables con 1 NA) ----
  else if (num == 4){
    datafr = unadata4(df)
  }
  # (5) Idem  ----
  else if (num == 5){
    datafr = unadata5(df)
  }
  # (6) Idem  ----
  else if (num == 6){
    datafr = unadata6(df)
  }
  # (7) Idem  ----
  else if (num == 7){
    datafr = unadata7(df)
  }
  
  # (8) Idem (35 variables con 2 NA) ----
  else if (num == 8){
    datafr = unadata8(df)
  }
  
  # (9) idem (6 variables con 1 NA) ----
  else if (num == 9){
    datafr = unadata9(df)
  }
  # (10) Idem (1 variables con 1 NA) ----
  else if (num == 10){
    datafr = unadata10(df)
  }
  # (11) Idem (2 variables con 3 NA) ----
  else if (num == 11){
    datafr = unadata11(df)
  }
  # (12) Idem (4 variables con 4 NA) ----
  else if (num == 12){
    datafr = unadata12(df)
  }
  # (13) Idem (1 variables con 1 NA) ----
  else if (num == 13){
    datafr = unadata13(df)
  }
  # (14) Idem (1 variables con 3 NA) ----
  else if (num == 14){
    datafr = unadata14(df)
  }
  # (15) Idem (3 variables con 1 NA) ----
  else if (num == 15){
    datafr = unadata15(df)
  }
  # (16) Idem (2 variables con 4 NA) ----
  else if (num == 16){
    datafr = unadata16(df)
  }
  # (17) Idem (2 variables con 3 NA) ----
  else if (num == 17){
    datafr = unadata17(df)
  }
  # (18) Idem (9 variables con 4 NA) ----
  else if (num == 18){
    datafr = unadata18(df)
  }
  # (19) Idem (1 variables con 2 NA) ----
  else if (num == 19){
    datafr = unadata19(df)
  }
  # (20) idem (1 variables con 2 NA) ----
  else if (num == 20){
    datafr = unadata20(df)
  }
  # (21) idem (1 variables con 3 NA)----
  else if (num == 21){
    datafr = unadata21(df)
  }
  # (22) idem (1 variables con 2 NA)  ----
  else if (num == 22){
    datafr = unadata22(df)
  }
  # (23) idem (1 variables con 6 NA)  ----
  else if (num == 23){
    datafr = unadata23(df)
  }
  # (24) idem (18 variables con 7 NA) ----
  else if (num == 24){
    datafr = unadata24(df)
  }
  # (25) idem (18 variables con 7 NA) ----
  else if (num == 25){
    datafr = unadata25(df)
  }# (26) idem (18 variables con 7 NA) ----
  else if (num == 26){
    datafr = unadata26(df)
  }# (27) idem (18 variables con 7 NA) ----
  else if (num == 27){
    datafr = unadata27(df)
  }# (28) idem (18 variables con 7 NA) ----
  else if (num == 28){
    datafr = unadata28(df)
  }
  # (29) Data sacando las  ́ultimas variables----
  else {
    datafr = unadata29(df)
  }
  return(datafr)
  
}

unaobselectdfs = function(df, num){
  connas = unaobselectraw(df, num)
  datafr = sacarnas(connas)
  return(datafr)
}

#View(unaobselectdfs(plsdata,11))
