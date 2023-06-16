


vars = read.csv("variables.csv")
variables = vars[(vars$variable %in% colnames(plsdata)), c(1,2)]
