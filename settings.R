source("funciones.R")
source("dataframes.R")
source("splits.R")

# Select dataframe
df <- selectdfs(plsdata,3)

# Train and Test split
data <- random.split(df, 0.8)

data <- lastyear.split(df)

# Hyperparameter selection
