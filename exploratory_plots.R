source("dataframes.R")
source("splits.R")
source("kfold_cv_selection.R")
source("pls.R")
library(xgboost)
library(caret)
library("glmnet")
library("ggplot2")

#########################################################
##dataframes (correr la primera vez sin comentar)
#datas = list()
#for (i in c(1,2,5,8,9,10,13,15)){
# nombre <- paste("plsdata", i, sep="_")
#datas[[nombre]] = selectdfs(plsdata,i)
#}

#Perform 10 fold cross validation
# for(i in 1:10){
#   #Segement your data by fold using the which() function 
#   testIndexes <- which(folds==i,arr.ind=TRUE)
#   testData <- yourData[testIndexes, ]
#   trainData <- yourData[-testIndexes, ]
#   #Use the test and train data partitions however you desire...
# }
#########################################################
df = datas[[2]]
df$year_trend <- as.numeric(as.character(df$year_Other))
df$year_trend <- df$year_trend - min(df$year_trend)

# Train and test datasets
data <- random.split(df, 0.8)
y <- df[, "mpi_Other"] 
X <- df[,-c(1:33)]
Time = df[, c(13:33)]
Region = df[, c(8:12)]
ytrain <- data$data_train[, "mpi_Other"] 
Xtrain <- data$data_train[,-c(1:33)]
Ttrain = data$data_train[, c(13:33)]
Rtrain = data$data_train[, c(8:12)]

ytest <- data$data_test[, target]
Xtest <- data$data_test[,-c(1:33)]
Ttest = data$data_test[, c(13:33)]
Rtest = data$data_test[, c(8:12)]


year0005<-ifelse(df$year_trend<=2005,1,0)
year0610 <-ifelse(2005<df$year_trend & df$year_trend<=2010,1,0)
year1015 <-ifelse(2010<df$year_trend & df$year_trend<=2015,1,0)
year1621 <-ifelse(20016<df$year_trend & df$year_trend<=2021,1,0)


ycat<-ifelse(y>=0.2,1,0)

fitclass<-cv.glmnet(as.matrix(X), ycat, family = "binomial", type.measure = "class")
fitclass = glmnet(as.matrix(X), as.matrix(ycat), 
             lambda=cv.glmnet(as.matrix(X), as.matrix(ycat))$lambda.1se)
selected_vars <- as.matrix(fitclass$beta)>0
fitclass2<-glmnet(X,ycat, family=c("binomial"))
newvar<-predict(fitclass, newx = as.matrix(X))

yr<-y[Rtrain[,1]==1]
plot(density(yr,xlim=c(0,1)))


par(mfrow=c(3,2))
for (i in 1:length(Region)){
yr<-y[Rtrain[,i]==1]
plot(density(yr),xlim=c(0,0.8), ylim=c(0,3))
}


A<-df[,"a_Other"]/100
par(mfrow=c(3,2))
for (i in 1:length(Region)){
  Ar<-A[Rtrain[,i]==1]
  plot(density(Ar))
}

H<-df[,"h_Other"]/100
par(mfrow=c(3,2))
for (i in 1:length(Region)){
  Hr<-H[Rtrain[,i]==1]
  plot(density(Hr))
}


library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)

year0005<-ifelse(df$year_trend<=2005,1,0)
year0610 <-ifelse(2005<df$year_trend & df$year_trend<=2010,1,0)
year1015 <-ifelse(2010<df$year_trend & df$year_trend<=2015,1,0)
year1621 <-ifelse(2016<df$year_trend & df$year_trend<=2021,1,0)


YearInterval <-cbind(year0005, year0610,year1015,year1621)
  

# Density by periods of 5 years
periodo<-ifelse(df$year_trend<=2005,1,ifelse(2005<df$year_trend & df$year_trend<=2010,2,ifelse(2010<df$year_trend & df$year_trend<=2015,3,4)))
dplot =as.data.frame(cbind(y, periodo))

#cols <- c("#F76D5E", "#FFFFBF", "#72D8FF", "green")

p1 <- ggplot(data=dplot, aes(x=y, group=as.factor(periodo), color = as.factor(periodo))) +
  geom_density(lwd = 1.5, linetype = 1)
   
# Density by regions
#Region 1
y1<-y[Rtrain[,1]==1]
periodo1<-periodo[Rtrain[,1]==1]
dplot =as.data.frame(cbind(y1, periodo1))

p11 <- ggplot(data=dplot, aes(x=y1, group=as.factor(periodo1), color = as.factor(periodo1))) +
  geom_density(lwd = 1.5, linetype = 1)




par(mfrow=c(2,2))
for (i in 1:4){
  yy<-y[YearInterval[,i]==1]
  plot(density(yy),xlim=c(0,0.8), ylim=c(0,4))
}


main_function_tcyd = function(df, target){
  results = data.frame()
  nfolds <- 5
  
  df$year_trend <- as.numeric(as.character(df$year_Other))
  df$year_trend <- df$year_trend - min(df$year_trend)
  
  # Train and test datasets
  data <- random.split(df, 0.8)
  ytrain <- data$data_train[, target] 
  Xtrain <- data$data_train[,-c(1:33)]
  Ttrain = data$data_train[, c(13:33)]
  Rtrain = data$data_train[, c(8:12)]
  
  ytest <- data$data_test[, target]
  Xtest <- data$data_test[,-c(1:33)]
  Ttest = data$data_test[, c(13:33)]
  Rtest = data$data_test[, c(8:12)]
  
  data <- as.data.frame(cbind(ytrain, Xtrain))
  
  ##########################################
  # tiempo discreto
  d = 2
  pls.projections_td_d2 <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d, scale = FALSE)
  data_td_d2 <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections_td_d2$W))
  newdata_td_d2 <- data.frame(as.matrix(Xtest) %*% pls.projections_td_d2$W)
  colnames(newdata_td_d2) <- colnames(data_td_d2)[-1]
  
  #Beta-regression 
  data_beta_td_d2 = data.frame(data_td_d2, Ttrain, Rtrain)
  formu = as.formula(paste("ytrain", paste(names(data_beta_td_d2)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_td <- tryCatch(betareg::betareg(formu , data = data_beta_td_d2), error= function(e) {return(NA)}  )
  
  # Predict over Xtest
  newdata_beta_td_d2 = data.frame(newdata_td_d2,Ttest,Rtest)
  ytest_pred.beta_td_d2 = tryCatch(predict(beta.fit_td, newdata_beta_td_d2), error= function(e) {return(NA)}  )
  
  # d = 1
  d = 1 
  pls.projections_td_d1 <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d, scale = FALSE)
  data_td_d1 <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections_td_d1$W))
  newdata_td_d1 <- data.frame(as.matrix(Xtest) %*% pls.projections_td_d1$W)
  colnames(newdata_td_d1) <- colnames(data_td_d1)[-1]
  
  #Beta-regression 
  data_beta_td_d1 = data.frame(data_td_d1, Ttrain, Rtrain)
  formu_td_d1 = as.formula(paste("ytrain", paste(names(data_beta_td_d1)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_td_d1 <- tryCatch(betareg::betareg(formu_td_d1 , data = data_beta_td_d1), error= function(e) {return(NA)}  )
  
  # Predict over Xtest
  newdata_beta_td_d1 = data.frame(newdata_td_d1,Ttest,Rtest)
  ytest_pred.beta_td_d1 = tryCatch(predict(beta.fit_td_d1, newdata_beta_td_d1), error= function(e) {return(NA)}  )
  
  #d mayor
  max.directions = 10
  hyperparam <- kfoldCV.pls(Xtrain, ytrain, nfolds, max.directions)
  d_opt = hyperparam$d.min
  pls.projections_td_dopt <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d_opt, scale = FALSE)
  data_td_dopt <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections_td_dopt$W))
  newdata_td_dopt <- data.frame(as.matrix(Xtest) %*% pls.projections_td_dopt$W)
  colnames(newdata_td_dopt) <- colnames(data_td_dopt)[-1]
  
  #Beta-regression 
  data_beta_td_dopt = data.frame(data_td_dopt, Ttrain, Rtrain)
  formu_td_dopt = as.formula(paste("ytrain", paste(names(data_beta_td_dopt)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_td_dopt <- tryCatch(betareg::betareg(formu_td_dopt , data = data_beta_td_dopt), error= function(e) {return(NA)}  )
  
  # Predict over Xtest
  newdata_beta_td_dopt = data.frame(newdata_td_dopt,Ttest,Rtest)
  ytest_pred.beta_td_dopt = tryCatch(predict(beta.fit_td_dopt, newdata_beta_td_dopt), error= function(e) {return(NA)}  )
  
  
  #elastic net 
  data_ela_td = data.frame(Xtrain,Ttrain, Rtrain)
  hyperparam <- kfoldCV.elastic(data_ela_td, ytrain, nfolds)
  newdata_ela_td <- data.frame(Xtest,Ttest,Rtest)
  elastic.fit_td <- glmnet::glmnet(x = data_ela_td, y = ytrain, family = "gaussian", alpha = hyperparam$best.alpha, lambda = hyperparam$best.lambda)
  # Predict over Xtest
  ytest_pred.elastic_td <- glmnet::predict.glmnet(elastic.fit_td, as.matrix(newdata_ela_td))
  
  # pls con las seleccionadas por elastic
  
  
  ############################
  # tiempo continuo
  #d=2
  # uso data_td_d2 y newdata_td_d2 porque son las mismas proyecciones
  #Beta-regression d=2
  data_beta_tc_d2 = data.frame(data_td_d2 , Rtrain)
  formu_tc_d2 = as.formula(paste("ytrain", paste(names(data_beta_tc_d2)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_tc_d2 <- tryCatch(betareg::betareg(formu_tc_d2, data = data_beta_tc_d2), error= function(e) {return(NA)}  )
  
  # Predict over Xtest
  newdata_beta_tc_d2 = data.frame(newdata_td_d2,Rtest)
  ytest_pred.beta_tc_d2 = tryCatch(predict(beta.fit_tc_d2, newdata_beta_tc_d2), error= function(e) {return(NA)}  )
  
  #d=1
  # uso data_td_d1 y newdata_td_d1 porque son las mismas proyecciones
  #Beta-regression d=1
  data_beta_tc_d1 = data.frame(data_td_d1 , Rtrain)
  formu_tc_d1 = as.formula(paste("ytrain", paste(names(data_beta_tc_d1)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_tc_d1 <- tryCatch(betareg::betareg(formu_tc_d1, data = data_beta_tc_d1), error= function(e) {return(NA)}  )
  
  # Predict over Xtest
  newdata_beta_tc_d1 = data.frame(newdata_td_d1,Rtest)
  ytest_pred.beta_tc_d1 = tryCatch(predict(beta.fit_tc_d1, newdata_beta_tc_d1), error= function(e) {return(NA)}  )
  
  #d mayor
  #uso data_td_dopt y newdata_td_dopt porque son las mismas proyecciones
  
  #Beta-regression d=1
  data_beta_tc_dopt = data.frame(data_td_dopt , Rtrain)
  formu_tc_dopt = as.formula(paste("ytrain", paste(names(data_beta_tc_dopt)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_tc_dopt <- tryCatch(betareg::betareg(formu_tc_dopt, data = data_beta_tc_dopt), error= function(e) {return(NA)}  )
  
  # Predict over Xtest
  newdata_beta_tc_dopt = data.frame(newdata_td_dopt,Ttest,Rtest)
  ytest_pred.beta_tc_dopt = tryCatch(predict(beta.fit_tc_dopt, newdata_beta_td_dopt), error= function(e) {return(NA)}  )
  
  
  #elastic net 
  data_ela_tc = data.frame(Xtrain, Rtrain)
  hyperparam <- kfoldCV.elastic(data_ela_td, ytrain, nfolds)
  newdata_ela_tc <- data.frame(Xtest, Rtest)
  elastic.fit_tc <- glmnet::glmnet(x = data_ela_tc, y = ytrain, family = "gaussian", alpha = hyperparam$best.alpha, lambda = hyperparam$best.lambda)
  # Predict over Xtest
  ytest_pred.elastic_tc <- glmnet::predict.glmnet(elastic.fit_tc, as.matrix(newdata_ela_tc))
  
  # pls con las seleccionadas por elastic
  
  
  # Saving results
  results = data.frame( 
    "MSE elastic_td" = mean((ytest-ytest_pred.elastic_td)^2),
    "MSE beta_td_d2" = mean((ytest-ytest_pred.beta_td_d2)^2),
    "MSE beta_td_d1" = mean((ytest-ytest_pred.beta_td_d1)^2),
    "MSE beta_tc_d2" = mean((ytest-ytest_pred.beta_tc_d2)^2),
    "MSE beta_tc_d1" = mean((ytest-ytest_pred.beta_tc_d1)^2),
    "MSE elastic_tc" = mean((ytest-ytest_pred.elastic_tc)^2),
    "MSE beta_td_dopt" = mean((ytest-ytest_pred.beta_td_dopt)^2),
    "MSE beta_td_dopt" = mean((ytest-ytest_pred.beta_tc_dopt)^2),
    "n" = nrow(df),  
    "p"= length(colnames(df)[!((colnames(df) %in% c(colnames(df)[1:33])))]),
    "d optimo" = d_opt,
    "Total de paises" = length(unique(df$iso))
    
    
  )
  
  return(results)
  
}

set.seed(20)
parts_20 = repetitions2(df,"mpi_Other",20,"main_function_tcyd2")

lapply(parts_20, function(x) mean(na.omit(x)))
