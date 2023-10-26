
####################################################################################################

categoriavar = function(df){
  vars = read.csv("dataframes_variables.csv")
  vars = vars[!duplicated(vars$variable),] #saco dupes
  vars = data.frame(lapply(vars, function(x) {gsub(",", ".", x)})) #saco comas de mas
  vars = data.frame(lapply(vars, function(x) {gsub(":", ".", x)})) #saco : de mas
  vars = data.frame(lapply(vars, function(x) {gsub("\\+", ".", x)})) #saco : de mas
  vars = data.frame(lapply(vars, function(x) {gsub("\\/", ".", x)})) #saco : de mas
  vars = data.frame(lapply(vars, function(x) {gsub("\\'", ".", x)})) #saco : de mas
  #cambio algunos raros
  vars[29,1] = colnames(df)[29]  
  vars[835,1] = "women.making.their.own.informed.decisions.regarding.sexual.relations..contraceptive.use.and.reproductive.health.care.....of.women.age.15.49."
  vars[1309,1] = "statistical.performance.indicators..spi...pillar.3.data.products.score...scale.0.100." 
  vars[141,1] = "disaster.risk.reduction.progress.score..1.5.scale..5.best."
  #le saco las mayus
  vars$variable <- tolower(vars$variable)
  colnames(df) <- tolower(colnames(df))
  #busco variables similares en vars y les asigno la misma categoria porque si
  faltantesvars = data.frame(variable = c("net.oda.provided..total..constant.2020.us..","net.oda.provided..total....of.gni.", "net.oda.provided..total..current.us..", "net.oda.provided.to.the.least.developed.countries....of.gni.", "net.oda.provided..to.the.least.developed.countries..current.us..",
                                          "control.of.corruption..standard.error", "control.of.corruption..percentile.rank..upper.bound.of.90..confidence.interval" , "control.of.corruption..percentile.rank..lower.bound.of.90..confidence.interval", 
                                          "control.of.corruption..percentile.rank", "control.of.corruption..number.of.sources", "control.of.corruption..estimate",
                                          "government.effectiveness..standard.error","government.effectiveness..percentile.rank..upper.bound.of.90..confidence.interval" ,"government.effectiveness..percentile.rank..lower.bound.of.90..confidence.interval",                             
                                          "government.effectiveness..percentile.rank","government.effectiveness..number.of.sources", "government.effectiveness..estimate",
                                          "regulatory.quality..standard.error","regulatory.quality..percentile.rank..upper.bound.of.90..confidence.interval","regulatory.quality..percentile.rank..lower.bound.of.90..confidence.interval" ,                                  
                                          "regulatory.quality..percentile.rank","regulatory.quality..number.of.sources","regulatory.quality..estimate",                                                                                  
                                          "rule.of.law..standard.error","rule.of.law..percentile.rank..upper.bound.of.90..confidence.interval","rule.of.law..percentile.rank..lower.bound.of.90..confidence.interval",                                          
                                          "rule.of.law..percentile.rank","rule.of.law..number.of.sources","rule.of.law..estimate",                                                                                         
                                          "political.stability.and.absence.of.violence.terrorism..standard.error","political.stability.and.absence.of.violence.terrorism..percentile.rank..upper.bound.of.90..confidence.interval","political.stability.and.absence.of.violence.terrorism..percentile.rank..lower.bound.of.90..confidence.interval",
                                          "political.stability.and.absence.of.violence.terrorism..percentile.rank","political.stability.and.absence.of.violence.terrorism..number.of.sources","political.stability.and.absence.of.violence.terrorism..estimate",
                                          "voice.and.accountability..standard.error","voice.and.accountability..percentile.rank..upper.bound.of.90..confidence.interval","voice.and.accountability..percentile.rank..lower.bound.of.90..confidence.interval",
                                          "voice.and.accountability..percentile.rank","voice.and.accountability..number.of.sources","voice.and.accountability..estimate",
                                          "firms.visited.or.required.meetings.with.tax.officials....of.firms."),
                             tipo = c(rep("Aid.Effectiveness",5), rep("Public.Sector",36), "Private.Sector" ) ) 
  vars = rbind(vars,faltantesvars)
  # ahora creo un dataframe que saca las de vars en colnames plsdata
  variables = vars[(vars$variable %in% colnames(df)),]  
  # cambio el orden para que quede igual que el de la base
  variables = variables[match(colnames(df), variables$variable),]
  # ahora queda con _, reeemplazo los nombres de las columnas
  colnames(df) = paste(colnames(df), variables$tipo, sep = "_")
  return(df)
}

#plsdata = categoriavar(plsdata)

#control
#length(colnames(plsdata)[!(colnames(plsdata) %in% variables$variable)])
#length(variables$variable[!(variables$variable %in%  colnames(plsdata))])
#colnames(plsdata)[!(colnames(plsdata) %in% variables$variable)]
 


####################################################################################################
dummiesregion = function(df){
  for (i in unique(df$region_Other)){
    df[[paste(i, "Other", sep = "_")]] = NA
  }
  
  for (i in colnames(df)[1486:1491]) {
    for (j in 1:nrow(df)){
      if (paste(df$region_Other[j], "Other", sep = "_") == i){
        df[[i]][j] = 1
      }else{
        df[[i]][j] = 0
      }
    }
  }
  df <- df[, c(1:7,1486:1491,8:1485)]
  return(df)
}

#plsdata=dummiesregion(plsdata)

#dummies por region
#creamos las columnas
#for (i in unique(plsdata$region_Other)){
 # plsdata[[paste(i, "Other", sep = "_")]] = NA
#}

#le damos valor
#for (i in colnames(plsdata)[1486:1491]) {
 # for (j in 1:nrow(plsdata)){
#    if (paste(plsdata$region_Other[j], "Other", sep = "_") == i){
 #     plsdata[[i]][j] = 1
  #  }else{
   #   plsdata[[i]][j] = 0
  #  }
#  }
#}
#las cambiamos de orden
#plsdata <- plsdata[, c(1:7,1486:1491,8:1485)]




####################################################################################################
# para hacer subsets con las variables
#estos son booleans
#endsWith(colnames(plsdata), 'Other') 
#grepl('Other', colnames(plsdata))

#View(plsdata[, grepl('Other', colnames(plsdata))])
#View(plsdata[, grepl('Agriculture...Rural.Development', colnames(plsdata))])
#View(plsdata[, grepl('Private.Sector', colnames(plsdata))]) 
#View(plsdata[, grepl('Energy...Mining', colnames(plsdata))])
#View(plsdata[, grepl('Infrastructure', colnames(plsdata))])
#View(plsdata[, grepl('Public.Sector', colnames(plsdata))]) 
#faltan otras categorias

