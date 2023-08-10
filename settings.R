source("dataframes.R")
source("splits.R")
source("kfold_cv_selection.R")

target <- "mpi_Other"
nfolds <- 5
pls.directions <- 10

# Select dataframe ####

df <- selectdfs(plsdata,2) #ahora los WBI son a partir de la col 14

# Train and Test split ####
data <- random.split(df, 0.8)

ytrain <- data$data_train[, target]; Xtrain <- scale(data$data_train[,-c(1:13)])
ytrain <- (ytrain - mean(ytrain))/sd(ytrain)
ytest <- data$data_test[, target]; Xtest <- scale(data$data_test[,-c(1:13)])
ytest <- (ytest - mean(ytest))/sd(ytest)
# Hyperparameter selection ####
hyperparam <- kfoldCV.selection(Xtrain, ytrain, nfolds, pls.directions)

# Estimate model ####
  # Linear model
    # Create data frame for prediction
data <- as.data.frame(cbind(ytrain, Xtrain))
    # Fit model
lm.fit <- lm(ytrain ~ . , data)
    # New data for prediction
newdata <- data.frame(Xtest)
    # Predict over Xtest
ytest_pred.lm <- predict(lm.fit, newdata)
  
  # XGBoost model
  # Predict over Xtest
ytest_pred.xgb <- predict(model, Xtest)

  # LASSO model
    # Fit model
lasso.fit <- glmnet::glmnet(x = Xtrain, y = ytrain, lambda = hyperparam$lambda.min)
  # Predict over Xtest
ytest_pred.lasso <- glmnet::predict.glmnet(lasso.fit, as.matrix(newdata))

  # PLS model
    # Fit linear model
pls.projections <- chemometrics::pls1_nipals(Xtrain, ytrain, a = hyperparam$d.min, scale = FALSE)
data <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections$W))
pls.fit <- lm(ytrain ~ . , data)

newdata <- data.frame(as.matrix(Xtest) %*% pls.projections$W)
colnames(newdata) <- colnames(data)[-1]
    # Predict over Xtest
ytest_pred.pls <- predict(pls.fit, newdata)

    # Fit non-parametric model

bw <- np::npregbw(ytrain ~ V2, data=data)
np_PLS <- np::npreg(bw)
    # Predict over Xtest
ytest_pred.np = predict(np_PLS, newdata=newdata, type="response")

  # PFC model
    # Fit model
PFC <- abundant::fit.pfc(Xtrain, ytrain, r = 3, d=1)
#PFC$Rmat
    # Predict over Xtest
ytest_pred.pfc <- abundant::pred.response(PFC, Xtest)


# Test results ####
mean((ytest-ytest_pred.lm)^2)
mean((ytest-ytest_pred.lasso)^2)
mean((ytest-ytest_pred.pls)^2)
mean((ytest-ytest_pred.np)^2)
mean((ytest-ytest_pred.pfc)^2)
mean((ytest-ytest_pred.xgb)^2)

cat("k-fold CV dimension reduction PLS selection: ",hyperparam$d.min,"\n\n")

cat("Variables shrunk by LASSO: ", sum(as.vector(unname(coefficients(lasso.fit))) == 0),
    "\n over a total of: ", length(as.vector(unname(coefficients(lasso.fit)))),"\n\n")
plot(as.matrix(Xtrain)%*% pls.projections$W[,1], ytrain)



