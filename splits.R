#############################################################################################
# Split random

random.split <- function(df, train_size){
  sam = sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(train_size,1-train_size))
  train = df[sam,]
  test = df[!sam,]
  return(list(data_train = train, data_test = test))
}

#############################################################################################
# Dejando la última para test
lastyear.split <- function(df){
  # Warning: if one year is observed, it is taken as the last year. Part of test set.
  df$sam <- as.logical(ave(df$year, df$iso, FUN = function(x){x == max(x)}))
  test <- df[sam,]
  train <- df[!sam,]
  return(list(data_train = train, data_test = test))
}



test.last = function(df){
  df$indicator = FALSE
  df$indicator[cumsum(rle(as.numeric(df$iso))$lengths)] = TRUE
  data = df[df$indicator,]
  data$indicator = NULL
  return(data)  
 }

train.last = function(df){
  df$indicator = FALSE
  df$indicator[cumsum(rle(as.numeric(df$iso))$lengths)] = TRUE
  data = df[!df$indicator,]
  data$indicator = NULL
  return(data)
  
}

#checks
#try = selectdfs(plsdata,2)






