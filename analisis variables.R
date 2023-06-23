



vars = read.csv("variables.csv")
vars <- vars[!duplicated(vars$variable),] #saco dupes
vars = data.frame(lapply(vars, function(x) {gsub(",", ".", x)})) #saco comas de mas
vars[29,1] = colnames(plsdata)[28] #cambio uno rari
variables = vars[(vars$variable %in% colnames(plsdata)),] #ahora saco las que estan en nuestra base

#control
(colnames(plsdata) %in% variables$variable)
(variables$variable %in%  colnames(plsdata))

# cambio el orden para que quede igual que el de la base
variables = variables[match(colnames(plsdata), variables$variable),]

# ahora queda con _, reeemplazo los nombres de las columnas
colnames(plsdata) = paste(colnames(plsdata), variables$tipo, sep = "_")

# para hacer subsets con las variables
#estos son booleans
endsWith(colnames(plsdata), 'Other') 
grepl('Other', colnames(plsdata))


View(plsdata[, grepl('Other', colnames(plsdata))])
View(plsdata[, grepl('Agriculture...Rural.Development', colnames(plsdata))])
View(plsdata[, grepl('Private.Sector', colnames(plsdata))]) #una sola variable, la 135 [1] "Arms.imports..SIPRI.trend.indicator.values._Public.Sector"

View(plsdata[, grepl('Energy...Mining', colnames(plsdata))])
View(plsdata[, grepl('Infrastructure', colnames(plsdata))]) #una sola variable, la 90 "ICT.goods.imports....total.goods.imports._Infrastructure"

View(plsdata[, grepl('Economy...Growth', colnames(plsdata))])
View(plsdata[, grepl('Public.Sector', colnames(plsdata))])


