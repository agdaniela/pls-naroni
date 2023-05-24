# agregando la columna region 



regiones = read.csv("Metadata_Country_API_TX.VAL.AGRI.ZS.UN_DS2_en_csv_v2_5454891.csv", sep = ",")

regions = regiones[,c(1,2)]



paises  = paste(unique(plsdata$iso))


regions = regions[, ]

