## k-fold CV for selecting LASSO and PLS hyperparameter

y <- data$data_train$MPI

X <- as.matrix(data$data_train[,-c(1:6)])

lasso.fit <- glmnet::cv.glmnet(X, y, type.measure = "mse", nfolds = 5)
lasso.fit$lambda.min

# fit sobre todo X con el lambda min.

K <- 5
d <- 10
n <- dim(X)[1]; p <- dim(X)[2] #number of observations and number of predictors
id<-sample(1:K, n, replace=TRUE, prob=rep(1/K,K))
ids <- id
# Create empty array to be filled by results:
# 1st dimension: prediction methods (3 - linear model, non-parametric, inverse regression)
# 2nd dimension: treatment and reduction methods (10)
# 3rd dimension: number of folds
Kresults <- array(NA, dim=c(K,10,3))
for(i in 1:K){
  flag <- paste0("Running the ",i,"-th fold",sep="")
  print(flag)
  ytrain <- y[id!=i]
  ytest <- y[id==i]
  Xtrain <- X[id!=i,]
  Xtest <- X[id==i,]
  
  projections <- pca.pls(ytrain,ytest,Xtrain,Xtest,col.m,col.n,A=a)
  proj_train <- c(projections$pca_projections_train,projections$pls_projections_train)
  proj_test <- c(projections$pca_projections_test,projections$pls_projections_test)
  
  # Linear Regression
  fit <- linear_model(ytrain,ytest,Pr_train=proj_train[[pr]],Pr_test=proj_test[[pr]])
  # MSE
  MSE_test <-  fit$MSE_TEST
  Kresults[i , pr , 1] <- MSE_test
  }
}
results = colMeans(Kresults, dim=1)

pls.fit$d.min

# once a is selecter fit over all X
