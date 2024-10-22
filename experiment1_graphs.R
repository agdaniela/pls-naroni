library(dplyr)
library(ggplot2)


#######################################################################
#EXPERIMENTO 1
#######################################################################
############################################################
# Tablas
############################################################




# df2
rep_mpi_df2 = readRDS("rep_mpi_df2.Rdata")
rep_a_df2 = readRDS("rep_a_df2.Rdata")
rep_h_df2 = readRDS("rep_h_df2.Rdata")
# df1
rep_mpi_df1 = readRDS("rep_mpi_df1.Rdata")
rep_a_df1 = readRDS("rep_a_df1.Rdata")
rep_h_df1 = readRDS("rep_h_df1.Rdata")
# df13
rep_mpi_df13 = readRDS("rep_mpi_df13.Rdata")
rep_a_df13 = readRDS("rep_a_df13.Rdata")
rep_h_df13 = readRDS("rep_h_df13.Rdata")

lapply(rep_mpi_df13, function(x) mean(na.omit(x)))
lapply(rep_mpi_df13, function(x) sd(na.omit(x)))[1]

tabla_exp1 = function(lista, df){
  
  average = lapply(lista, function(x) round(mean(na.omit(x)),4))
  deviation = lapply(lista, function(x) round(sd(na.omit(x)),4))
  
  return(deviation)
}

 

reps_mpi = data.frame(cbind(tabla_exp1(rep_mpi_df1),tabla_exp1(rep_mpi_df2),tabla_exp1(rep_mpi_df13)))
colnames(reps_mpi) = c("df1","df2","df13")

reps_a = data.frame(cbind(tabla_exp1(rep_a_df1),tabla_exp1(rep_a_df2),tabla_exp1(rep_a_df13)))
colnames(reps_a) = c("df1","df2","df13")

reps_h = data.frame(cbind(tabla_exp1(rep_h_df1),tabla_exp1(rep_h_df2),tabla_exp1(rep_h_df13)))
colnames(reps_h) = c("df1","df2","df13")


reps_mpi_sd = data.frame(cbind(tabla_exp1(rep_mpi_df1),tabla_exp1(rep_mpi_df2),tabla_exp1(rep_mpi_df13)))
colnames(reps_mpi) = c("df1","df2","df13")

reps_a_sd = data.frame(cbind(tabla_exp1(rep_a_df1),tabla_exp1(rep_a_df2),tabla_exp1(rep_a_df13)))
colnames(reps_a) = c("df1","df2","df13")

reps_h_sd = data.frame(cbind(tabla_exp1(rep_h_df1),tabla_exp1(rep_h_df2),tabla_exp1(rep_h_df13)))
colnames(reps_h) = c("df1","df2","df13")

library("openxlsx")

reps_excel <- createWorkbook()
addWorksheet(reps_excel, "mpi - mean")
addWorksheet(reps_excel, "a - mean")
addWorksheet(reps_excel, "h - mean")


addWorksheet(reps_excel, "mpi - sd")
addWorksheet(reps_excel, "a - sd")
addWorksheet(reps_excel, "h - sd")


reps_mpi$meandev_df1 = paste0(reps_mpi$df1, " (",reps_mpi_sd$X1, ")")
reps_mpi$meandev_df2 = paste0(reps_mpi$df2, " (",reps_mpi_sd$X2, ")")
reps_mpi$meandev_df13 = paste0(reps_mpi$df13, " (",reps_mpi_sd$X3, ")")

reps_a$meandev_df1 = paste0(reps_a$df1, " (",reps_a_sd$X1, ")")
reps_a$meandev_df2 = paste0(reps_a$df2, " (",reps_a_sd$X2, ")")
reps_a$meandev_df13 = paste0(reps_a$df13, " (",reps_a_sd$X3, ")")

reps_h$meandev_df1 = paste0(reps_h$df1, " (",reps_h_sd$X1, ")")
reps_h$meandev_df2 = paste0(reps_h$df2, " (",reps_h_sd$X2, ")")
reps_h$meandev_df13 = paste0(reps_h$df13, " (",reps_h_sd$X3, ")")



writeData(reps_excel, "mpi - mean", reps_mpi, startRow = 1, startCol = 1)
writeData(reps_excel, "a - mean", reps_a, startRow = 1, startCol = 1)
writeData(reps_excel, "h - mean", reps_h, startRow = 1, startCol = 1)
writeData(reps_excel, "mpi - sd", reps_mpi_sd, startRow = 1, startCol = 1)
writeData(reps_excel, "a - sd", reps_a_sd, startRow = 1, startCol = 1)
writeData(reps_excel, "h - sd", reps_h_sd, startRow = 1, startCol = 1)

saveWorkbook(reps_excel, file = "reps_excel.xlsx", overwrite = TRUE)


############################################################
# Graficos
############################################################

#df1

rep_mpi_df1 = readRDS("rep_mpi_df1.Rdata")
rep_a_df1 = readRDS("rep_a_df1.Rdata")
rep_h_df1 = readRDS("rep_h_df1.Rdata")

#df2
rep_mpi_df2 = readRDS("rep_mpi_df2.Rdata")
rep_a_df2 = readRDS("rep_a_df2.Rdata")
rep_h_df2 = readRDS("rep_h_df2.Rdata")

#df13
rep_mpi_df13 = readRDS("rep_mpi_df13.Rdata")
rep_a_df13 = readRDS("rep_a_df13.Rdata")
rep_h_df13 = readRDS("rep_h_df13.Rdata")


graph_data_errors = function(lista){
  errors_df = data.frame(matrix(unlist(lista), nrow=50, byrow=F))
  colnames(errors_df) = names(lista)
  return(errors_df)
}

library(ggplot2)

theme_set(
  theme_light(base_size = 18) +
    theme(
      axis.line.x = element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      axis.line.y = element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = 0.5),
      panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = 0.5) ,
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.text = element_text(size = 18), #
      
    )
)


#MSE
#mpi -df1
errors = graph_data_errors(rep_mpi_df1)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(name = "",labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  guides(linetype = "none")


#a - df1
errors = graph_data_errors(rep_a_df1)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  #ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#h - df1
errors = graph_data_errors(rep_h_df1)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )


#MSE
#mpi -df2
errors = graph_data_errors(rep_mpi_df2)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(name = "",labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  guides(linetype = "none")


#a - df2
errors = graph_data_errors(rep_a_df2)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.01) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#h - df1
errors = graph_data_errors(rep_h_df2)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#MSE
#mpi -df13
errors = graph_data_errors(rep_mpi_df13)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(name = "",labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  guides(linetype = "none")


#a - df13
errors = graph_data_errors(rep_a_df13)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.01) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#h - df13
errors = graph_data_errors(rep_h_df13)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.08) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )



# DISTANCE

#mpi -df1
errors = graph_data_errors(rep_mpi_df1)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 1) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(name = "",labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  guides(linetype = "none")


#a - df1
errors = graph_data_errors(rep_a_df1)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  #ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#h - df1
errors = graph_data_errors(rep_h_df1)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.75) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )


#Distance
#mpi -df2
errors = graph_data_errors(rep_mpi_df2)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 1) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(name = "",labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  guides(linetype = "none")


#a - df2
errors = graph_data_errors(rep_a_df2)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  #ylim(-0.0001, 0.01) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#h - df1
errors = graph_data_errors(rep_h_df2)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.6) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#dist
#mpi -df13
errors = graph_data_errors(rep_mpi_df13)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 1) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(name = "",labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  guides(linetype = "none")


#a - df13
errors = graph_data_errors(rep_a_df13)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  #  ylim(-0.0001, 0.01) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )

#h - df13
errors = graph_data_errors(rep_h_df13)
errors_graph = reshape2::melt(errors[,c(9:16)])
names(errors_graph) = c("Methods","Distance")

ggplot(data = errors_graph, aes(x=Methods, y=Distance)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.6) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )


#barplot surveys
ggplot(as.data.frame(table(df_1$year_Other)) , aes(x = Var1, y = Freq)) + 
  geom_bar(stat = "identity", fill = "dodgerblue4",width = 0.5, show.legend = F)+
  xlab("Years")+
  ylab("Number of Surveys")+
  geom_text(aes(label=Freq), vjust=1.6, color="white", size=2.8)+
  theme_light()+ 
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 70,  hjust=1)
  )


ggplot(df_1, aes(x=year_Other, y=len, fill=dose)) +
  geom_bar(stat="identity")+theme_minimal()

library(dplyr)
df_1 %>%
  group_by(region_Other) %>%
  count(year_Other) %>%
  ggplot(aes(x = year_Other, y = n, fill = region_Other)) + 
  geom_bar(stat = "identity",width = 0.5, show.legend = T)+
  scale_fill_brewer(palette="Blues", name="Regions")+
  xlab("Years")+
  ylab("Number of Surveys")+
  theme_light()+ 
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 70,  hjust=1),
        legend.position="bottom")



