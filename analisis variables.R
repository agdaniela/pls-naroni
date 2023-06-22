vars = read.csv("variables.csv")
vars <- vars[!duplicated(vars$variable),]
variables = vars[(vars$variable %in% colnames(plsdata)),]
colnames(plsdata[!(colnames(plsdata) %in% variables$variable),])

paste(colnames(plsdata), variables$tipo, sep = "_")