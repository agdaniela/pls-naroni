library(ggplot2)
library(dplyr)
library(reshape2)

results_se_df6 = readRDS("results_se_df6.Rdata")
results_se_df7 = readRDS("results_se_df7.Rdata")
results_se_df8 = readRDS("results_se_df8.Rdata")


# Boxplots
res6 = melt(as.data.frame(results_se_df6)[,1:6])

ggplot(res6 , aes(x = variable, y = value, color = variable)) + 
  geom_boxplot(show.legend = F)+
  ylim(0,0.1)+
  xlab("")+
  ylab("df6")

res7 = melt(as.data.frame(results_se_df7)[,1:6])

ggplot(res7 , aes(x = variable, y = value, color = variable)) + 
  geom_boxplot(show.legend = F)+
  ylim(0,0.1)+
  xlab("")+
  ylab("df7")

res8 = melt(as.data.frame(results_se_df8)[,1:6])

ggplot(res8 , aes(x = variable, y = value, color = variable)) + 
  geom_boxplot(show.legend = F)+
  #ylim(0,0.1)+
  xlab("")+
  ylab("df8")


# Gráfico de dispersión por grupo
ggplot(df, aes(x = x, y = y, color = grupo)) +
  geom_point()

 

