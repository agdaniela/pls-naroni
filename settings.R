source("funciones.R")
source("dataframes.R")
source("splits.R")
source("kfold_cv_selection.R")

target <- "MPI"
pls.directions <- 9
nfolds <- 5

# Select dataframe ####
df <- subset(selectdfs(plsdata,3), MPI < 0.2)

# Train and Test split ####
data <- random.split(df, 0.8)

ytrain <- data$data_train[, target]; Xtrain <- data$data_train[,-c(1:6)]

ytest <- data$data_test[, target]; Xtest <- data$data_test[,-c(1:6)]

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

  # LASSO model
    # Fit model
lasso.fit <- glmnet::glmnet(x = Xtrain, y = ytrain, lambda = hyperparam$lambda.min)
  # Predict over Xtest
ytest_pred.lasso <- glmnet::predict.glmnet(lasso.fit, as.matrix(newdata))

  # PLS model
    # Fit model
pls.projections <- chemometrics::pls1_nipals(Xtrain, ytrain, a = hyperparam$d.min, scale = FALSE)
data <- as.data.frame(cbind(ytrain, as.matrix(Xtrain- colMeans(Xtrain)) %*% pls.projections$W))
pls.fit <- lm(ytrain ~ . , data)


newdata <- data.frame(as.matrix(Xtest- colMeans(Xtest)) %*% pls.projections$W)
colnames(newdata) <- colnames(data)[-1]
    # Predict over Xtest
ytest_pred.pls <- predict(pls.fit, newdata)



bw <- np::npregbw(ytrain ~ V2, data=data)
np_PLS <- np::npreg(bw)
ytest_pred.np = predict(np_PLS, newdata=newdata, type="response")

# Test results ####
mean((ytest-ytest_pred.lm)^2)
mean((ytest-ytest_pred.lasso)^2)
mean((ytest-ytest_pred.pls)^2)
mean((ytest-ytest_pred.np)^2)

