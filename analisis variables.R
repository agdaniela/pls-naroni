



vars = read.csv("variables.csv")
vars <- vars[!duplicated(vars$variable),] #saco dupes


vars = data.frame(lapply(vars, function(x) {gsub(",", ".", x)})) #saco comas de mas
vars = data.frame(lapply(vars, function(x) {gsub(":", ".", x)})) #saco : de mas
#vars[29,1] = colnames(plsdata)[29] #cambio uno rari
variables = vars[(vars$variable %in% colnames(plsdata)),] #ahora saco las que estan en nuestra base
variables$variable <- tolower(variables$variable)
colnames(plsdata) <- tolower(colnames(plsdata))
#control
length(colnames(plsdata)[!(colnames(plsdata) %in% variables$variable)])

length(colnames(variables)[!(variables$variable %in%  colnames(plsdata))])

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
View(plsdata[, grepl('Private.Sector', colnames(plsdata))]) 

View(plsdata[, grepl('Energy...Mining', colnames(plsdata))])
View(plsdata[, grepl('Infrastructure', colnames(plsdata))]) #una sola variable, la 90 "ICT.goods.imports....total.goods.imports._Infrastructure"

View(plsdata[, grepl('Economy...Growth', colnames(plsdata))])
View(plsdata[, grepl('Public.Sector', colnames(plsdata))]) #una sola variable, la 135 [1] "Arms.imports..SIPRI.trend.indicator.values._Public.Sector"


#tratando de ver que esta mal
hola = read.delim("bigdata.txt", header = TRUE, sep = " ", dec = ".")
names(hola) <- sub('\\...y$', '..', names(hola))
names(hola) <- sub('\\...x$', '..', names(hola))
names(hola) <- sub('\\.x$', '', names(hola))
names(hola) <- sub('\\.y$', '', names(hola))
hola = hola[!duplicated(as.list(hola))]
#aca le saque la terminacion de punto a ambos para ver si era eso y no, siguen coincidiendo 758 variables nomas

names(hola) <- sub('\\.$', '', names(hola))
vars$variable <- sub('\\.$', '', vars$variable)






