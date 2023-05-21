source("funciones.R")
source("dataframes.R")
source("splits.R")
source("kfold_cv_selection.R")

target <- "MPI"
pls.directions <- 5

# Select dataframe ####
df <- selectdfs(plsdata,3)

# Train and Test split ####
data <- random.split(df, 0.7)

ytrain <- data$data_train[, target]; Xtrain <- data$data_train[,-c(1:6)]

ytest <- data$data_test[, target]; Xtest <- data$data_test[,-c(1:6)]

# Hyperparameter selection ####
hyperparam <- kfoldCV.selection(Xtrain, ytrain, 5, pls.directions)

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
pls.fit <- chemometrics::pls1_nipals(Xtrain, ytrain, a = hyperparam$d.min)
    # Predict over Xtest
ytest_pred.pls <- as.matrix(Xtest) %*% pls.fit$W

# Test results ####
mean((ytest-ytest_pred.lm)^2)
mean((ytest-ytest_pred.lasso)^2)
mean((ytest-ytest_pred.pls)^2)

