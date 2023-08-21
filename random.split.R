random.split <- function(df, train_size){
  ids <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(train_size,1-train_size))
  train <- df[ids,]
  test <- df[!ids,]
  preProcData <- caret::preProcess(train, method = "center", "scale")
  train <- predict(preProcData, train)
  test <- predict(preProcData, test)
  return(list(dataTrain = train, dataTest = test))
}
