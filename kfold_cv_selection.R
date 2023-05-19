## k-fold CV for selecting LASSO and PLS hyperparameter

y <- data$data_train$MPI

X <- as.matrix(data$data_train[,-c(1:6)])

lasso.fit <- glmnet::cv.glmnet(X, y, type.measure = "mse", nfolds = 5)
lasso.fit$lambda.min

# fit over all X using optimal lambda .

source("pls.R")

pls.fit <- cv.pls(X, y, 5, 3)
pls.fit$d.min

dim(chemometrics::pls1_nipals(X, y, a = pls.fit$d.min, scale = FALSE)$W)
dim(X)

# once d is selected fit over all X
