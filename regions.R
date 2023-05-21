



class(regs$Country.Code)
class(regs$Region)

class(paises)






regs = read.csv("regions.csv")
regions = regs[(regs$Country.Code %in% paste((plsdata$iso))), c(1,2)] 
regions = cbind(regions, aggregate(year ~ iso, data = plsdata, FUN = length)[,2])
regions = regions[rep(1:nrow(regions), regions$`aggregate(year ~ iso, data = plsdata, FUN = length)[, 2]`),]
plsdata$region = regions$Region
plsdata = plsdata[, c("iso", "country", "region", colnames(plsdata)[3:156] )]













