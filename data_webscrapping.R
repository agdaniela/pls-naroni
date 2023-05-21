source("~/Dropbox/BiblioGirela/PLS/aux_data_webscrapping.R")

# 1 Download all published MPI 2010-2022
MPI <- MPI_data()
countries <- unique(MPI$iso)

# 2.1 Download World Bank data
WB_url = "https://api.worldbank.org/v2/en/topic/1?downloadformat=csv"
data <- WB_data(WB_url)
data <- clean_WB_data(data,countries)
data_long <- WB_data_long(data)
lags = 1
data1 <- data_join(MPI, data_long, lags)
# Database without lags
data1 <- data1[(data1$year == data1$variable),]
total <- data1
for (i in 2:21){
  WB_url = paste0("https://api.worldbank.org/v2/en/topic/",i,"?downloadformat=csv")
  data <- WB_data(WB_url)
  data <- clean_WB_data(data,countries)
  data <- data[!(data$indicator %in% colnames(data1)),]
  data_long <- WB_data_long(data)
  data2 <- data_join(MPI, data_long, 1)
  # Database without lags
  data2 <- data2[(data2$year == data2$variable),]
  
  total <- merge(data1,data2,by=c("iso","country","year","MPI","H","A","variable"))
}

save(total,file = "~/Dropbox/BiblioGirela/PLS/bigdata.rda")
