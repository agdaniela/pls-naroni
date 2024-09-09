source("dataframes.R")
source("kfold_cv_selection.R")
boostrap <- function(in_sample, out_of_sample, ...){
  
  methods <- c("betaboost", "elasticnet", "betatree","xgboost")
  df.is <- in_sample
  df.oos <- out_of_sample
  
  # Sample data with replacement
  boot.idxs <- sample(1:nrow(df.is), size = nrow(df.is), replace = TRUE)
  
  boot.rows <- lapply(boot.idxs, function(x) df.is[x,])
  
  boot.data <- do.call("rbind", boot.rows)
  
  # BetaBoost #
  # Fit model
  #betaboost.fit <- mboost::glmboost(y ~ ., data = boot.data, center = FALSE,
  #                                  family = betaboost::BetaReg(), 
  #                                  control = mboost::boost_control(mstop = 2000))
  betaboost.fit <- mboost::blackboost(y ~ ., data = boot.data, family = betaboost::BetaReg(),
                                      control = mboost::boost_control(mstop = 2000),
                                      tree_controls = partykit::ctree_control(maxdepth = 3))

  # Predict out-of-sample
  betaboost.pred <- predict(betaboost.fit, newdata = df.oos[,-1], type = "response")
  rownames(betaboost.pred) <- rownames(df.oos)
  # ElasticNet #
  # Select model with 5-fold CV
  hyperparam_elasticnet <- kfoldCV.elastic(boot.data[,-1], boot.data[,1], 5)
  # Fit model
  elastic.fit <- glmnet::glmnet(x = boot.data[,-1], y = boot.data[,1],
                                family = "gaussian",
                                alpha = hyperparam_elasticnet$best.alpha,
                                lambda = hyperparam_elasticnet$best.lambda)
  # Predict out-of-sample
  elastic.pred <- glmnet::predict.glmnet(elastic.fit, as.matrix(df.oos[,-1]))
  # save

  # Beta-tree elastic #
  # Create new dummy variable according to the distribution of the response variable
  cut.tree <- 0.2 # 0.2 value was taken in experiments 1 and 2
  dummy_cut.tree <- ifelse(boot.data[,1] <= cut.tree, 1, 0)
  dummy_cut.tree <- as.vector(ifelse(df.oos[,1] <= cut.tree, 1, 0))
  # Retrieve selected elastic net coefficients
  elastic.coefs <- coef(elastic.fit, s = "lambda.min")
  temp <- c("y",elastic.coefs@Dimnames[[1]][elastic.coefs@i +1])
  num_chosen_ela <-  length(temp)
  betatree.train <- boot.data[,c(colnames(boot.data) %in% temp)]
  betatree.test <- df.oos[,colnames(df.oos) %in% temp]
  # Fit
  betatree.fit <- tryCatch(betareg::betareg(y ~ ., data = betatree.train, link.phi = "log", link = "logit"), error= function(e) {return(NA)}  )
  # Predict out-of-sample
  betatree.pred <- tryCatch(predict(betatree.fit, betatree.test[,-1]), error= function(e) {return(NA)}  )

  # XGBoost #
  # Select model with 5-fold CV
  hyperparam_xgb <- kfoldCV.xgboost(boot.data[,-1], boot.data[,1], 5)
  # Fitted model
  xgb.fit <- xgboost::xgboost(params = hyperparam_xgb$xgb.params, 
                                data = as.matrix(boot.data[,-1]),
                                label = boot.data[,1], 
                                nrounds = hyperparam_xgb$xgb.model, verbose = F, nthread = 5)
  # Predict out-of-sample
  xgb.pred <- as.matrix(predict(xgb.fit, as.matrix(df.oos[,-1])))
  # These preds do not have rownames
  rownames(xgb.pred) <- rownames(elastic.pred)
  
  # Stacking #
  # fitted values of each model
  fitted.betaboost <- predict(betaboost.fit, boot.data[,-1], type = "response")
  fitted.elasticnet <- predict(elastic.fit, as.matrix(boot.data[,-1]))
  fitted.xgb <- predict(xgb.fit, as.matrix(boot.data[,-1]))
  fitted <- cbind(fitted.betaboost, fitted.elasticnet, fitted.xgb)
  colnames(fitted) <- c("betaboost","elasticnet","xgboost")
  
  
  # Save results
  results <- cbind(betaboost.pred, elastic.pred, betatree.pred, xgb.pred)
  colnames(results) <- methods
  
  return(results)
}

# Data preprocessing ------------------------------------------------------
# Call dataframe of countries that have at least one MPI measure from 1990 to 2023
file <- "bigdata_all_years.txt"
df <- read.delim(file, header = TRUE, sep = " ", dec = ".")
# Convert ISO code, country names and years variables into factor
df$iso <- as.factor(df$iso)
df$country <- as.factor(df$country)
df$year <- as.numeric(df$year)
# Get all country ISO code in World Bank database
regs <- read.csv("dataframes_regions.csv")[,1:2]
# Keep only countries that have an MPI measure
regions <- regs[(regs$Country.Code %in% df$iso),]
# Change regions colnames for merging
colnames(regions) <- c("iso","region")
  # replace special character "&" and " " and "-"
regions$region <- gsub("&","and", regions$region)
regions$region <- gsub(" ","_", regions$region)
regions$region <- gsub("-","", regions$region)
# Merge df and regions to get the world region variable
df <- merge(regions, df, by = "iso")
# Clean variables names with special characters
df <- categoriavar(df)
# Delete "_Other" subfix from variables
colnames(df) <- gsub("_Other","", colnames(df))
# df$year contain the year MPI measure and df$variable the year.
# df$variable contains duplicated values. Delete duplicated values
# First we need to sort data by country code, year and MPI measure 
  #in order not to drop out the duplicated rows with the MPI measure
df <- df[with(df, order(iso, variable_NA,-mpi)),]
df <- df[!duplicated(df[,c("iso","variable_NA")]),]
# Drop year variable and replace it by variable_NA
df$year <- NULL
colnames(df)[which(colnames(df) == "variable_NA")] <- "year"
# Remove reg and regions objects
rm(regions, regs)
# Filter years from 2000 to 2021
df <- df[(df$year >= 2000)&(df$year <= 2021),] # 2000 is the minimum year in the observed MPI 
# Create year trend variable
df$year_trend <- as.numeric(as.character(df$year))
df$year_trend <- df$year_trend - min(df$year_trend)
# Include regions as dummy variables
region <- as.factor(df$region) 
R <- model.matrix(~ region); R <- R[,-1] # remove intercept

df <- cbind(df,R)
# Scale incidence and intensity
df$h <- df$h / 100
df$a <- df$a / 100

# Prepare model frame -----------------------------------------------------
# Define the target (either MPI, H, or A)
target <- "mpi"
y <- df[,target] + 1e-05 # ensure response between 0 and 1 bounds
X <- df[,c(8:ncol(df))]

model.data <- cbind(y, X)
# index rows by countries and years
rownames(model.data) <- paste(df$country,df$year, sep = ".")

# Separate the in-sample and out-of-sample observations
  # drop NAs in target variable
df.is <- model.data[!(is.na(model.data$y)),]
  # selected countries for prediction: Senegal (8), Bangladesh (4), Bolivia (2)
selected_countries <- c("Senegal","Bangladesh","Bolivia")
    # get these countries idxs
idxs_list <- lapply(selected_countries, function(x) grep(x,rownames(model.data)))
idxs <- do.call("c",idxs_list)

# Select dataframe
which.df <- "df1"
  # df1 in the paper:
df.is <- df.is[,apply(df.is, 2, function(x) !any(is.na(x)))] # this is df1
  # df2 in the paper:
# INSERT CODE
  # df13 in the paper:
# INSERT CODE 

# Get the out-of-sample dataset (the selected countries); preserve columns in df.is
df.oos <- model.data[idxs,colnames(df.is)]

# Model training and prediction -------------------------------------------
B <- nrow(df.is) # Number of repetitions


#parallel::mclapply(1:4, boostrap, in_sample = df.is, out_of_sample = df.oos, mc.cores = 4)

library(doParallel)

cores <- 26
#create the cluster
cl <- parallel::makeCluster(cores)

#check cluster definition (optional)
print(cl)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = cl)

#check if it is registered (optional)
foreach::getDoParRegistered()
foreach::getDoParWorkers()

experiment_start <- Sys.time() 
results <- foreach(i = 1:B) %dopar% {
  boostrap(in_sample = df.is, out_of_sample = df.oos)
}
experiment_end <- Sys.time()
experiment_time <- difftime(experiment_end, experiment_start, units = "mins")
experiment_time

parallel::stopCluster(cl = cl)

# Save bootstrap results
out <- lapply(results, function(X) {
  X <- as.data.frame(X)
  X$country <- as.factor(stringr::str_extract(rownames(X), "[^.]+"))
  X$year <- as.factor(stringr::str_extract(rownames(X), "\\b\\w+$"))
  rownames(X) <- NULL
  X
}) 

out <- do.call("rbind",out)
attr(out,"selected countries") <- selected_countries
attr(out,"target") <- target
attr(out, "data frame") <- which.df
attr(out, "repetitions") <- B
filename <- paste("boostrap","results",target,which.df,".Rda", sep="_")
saveRDS(out, file=filename)

out <- readRDS(filename)
stop("Stop here")

# Get ground truth values and plot -----------------------------------------
  # Search for the ground truth
true.data <- data.frame(df.oos$y); 
rownames(true.data) <- rownames(df.oos)
colnames(true.data) <- "ground_truth"
true.data$country <- as.factor(stringr::str_extract(rownames(true.data), "[^.]+"))
true.data$year <- as.factor(stringr::str_extract(rownames(true.data), "\\b\\w+$"))

  # Plot for each method
library(ggplot2)
for (method in c("betaboost","elasticnet","betatree","xgboost")){
  # Filter by the method to be plotted
  plot.formula <- as.formula(paste(method,"~ year + country"))
  plot.data <- aggregate(plot.formula, out, mean)
  plot.data <- merge(plot.data, true.data, by = c("country","year"))
  colnames(plot.data)[which(colnames(plot.data) == method)] <- "method"
  plot.data$sd <- aggregate(plot.formula, out, sd)[,3]
  plot.data$lb <- plot.data$method - 1.96 * plot.data$sd
  plot.data$ub <- plot.data$method + 1.96 * plot.data$sd
  #plot.data$lb <- aggregate(plot.formula, out, function(x) quantile(x,0.05))[,method]
  #plot.data$ub <- aggregate(plot.formula, out, function(x) quantile(x,0.95))[,method]
  
  ggplot(plot.data, aes(x = year, y = method, group = 1)) + #define x and y axis variables
    geom_line() + #add scatterplot points
    geom_point(aes(y = ground_truth),shape=4) + 
    #stat_smooth(method = lm) + #confidence bands
    geom_line(aes(y = lb), col = "coral2", linetype = "dashed") + #lwr pred interval
    geom_line(aes(y = ub), col = "coral2", linetype = "dashed") + #upr pred interval
    geom_ribbon(data=plot.data,
                aes(ymin=lb,ymax=ub), fill="gray", alpha=0.5) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
    xlab("Years") + ylab(toupper(target)) + ylim(-0.05,0.8) + facet_grid(.~country)
  
  
  plotname <- paste(paste("plot.experiment3",method,target,which.df,sep="_"),".png",sep="")
  ggsave(file=plotname, device=png, limitsize = FALSE)
}



