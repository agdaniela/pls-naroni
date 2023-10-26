RandomKfoldCV.Model <- function(target, which.dataframe){
  data <- selectdfs(plsdata, which.dataframe) # modify plsdata...!
  
  # split data
  
  data <- random.split(data, 0.8)
  dataTrain <- data$dataTrain
  dataTest <- data$dataTest
  
  # Retrieve only the target variable and predictors
  ytrain <- dataTrain[,target]
  ytest <- dataTest[,target]
  dataTrain <- cbind(ytrain, dataTrain[,-c(1:13)])
  dataTest <- cbind(ytest, dataTest[,-c(1:13)])
  
  
  selected.models <- KfoldCV.RandomSearch(dataTrain, nfolds = 5)
  
  ypreds <- modelPredictions(dataTest, selected.models)
  
  RMSE <- apply(as.data.frame(ypreds), MARGIN = 2, FUN = function(x) caret::RMSE(x,ytest))
  
  return(RMSE)
}
