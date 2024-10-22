target = "mpi_Other"
ytrain = trainData_13[,target]
Xtrain = trainData_13[,-c(1:7,13:33)]
ytrain_nacho = dataset13[,"mpi"]
Xtrain_nacho = dataset13[,-c(1:7)]

mboost::blackboost(ytrain_nacho ~ ., data = Xtrain_nacho, family = betaboost::BetaReg(),
                                   control = mboost::boost_control(mstop = 200),
                                  tree_controls = partykit::ctree_control(maxdepth =  3))
 
mboost::blackboost(ytrain ~ ., data = Xtrain[,1:120], family = betaboost::BetaReg(),
                   control = mboost::boost_control(mstop = 200),
                   tree_controls = partykit::ctree_control(maxdepth =  2))

trace(inum:::inum.data.frame, at = list(c(7, 4, 3, 4, 3, 8)), 
      tracer = quote({
        tol <- max(tol, 10 * .Machine$double.eps)
      }))
mboost::mboost(ytrain ~ ., Xtrain)

for ( col in 1:ncol(Xtrain_nacho)){
  colnames(Xtrain_nacho)[col] <-  sub("_2.*", "", colnames(Xtrain_nacho)[col])
}
sum(sapply(Xtrain, function(x) any(is.na(x))))

# 
 
df_fold_nacho  <- dataset13[sample(nrow(dataset13)),]

folds_index_nacho <- cut(seq(1,nrow(df_fold_nacho)),breaks=10,labels=FALSE) #indices

predichos_nacho = list()

for(i in 1:10){
  testIndexes_nacho <- which(folds_index_nacho == i,arr.ind=TRUE)
  testData_nacho <- df_fold_nacho[testIndexes_nacho, ]
  trainData_nacho <- df_fold_nacho[-testIndexes_nacho, ]
  nombre_nacho <- paste("yhats", i, sep="_")
  predichos_nacho[[nombre_nacho]] = main_function_pred(trainData_nacho,testData_nacho,"mpi", corte=0.2, link_phi = "log", link_mu = "logit", distancia = "hellinger")
}
results_nacho =readRDS("~/Daniela/pls-naroni/EXPERIMENT2_MPI_dataset13.Rdata")
res_nacho = do.call("rbind",results_nacho)


mean((res_nacho$ground.truth - res_nacho$betaboost)^2)

