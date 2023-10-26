library(ggplot2)
library(dplyr)
library(reshape2)

results_se_df1 = readRDS("results_se_df1.Rdata")
results_se_df3 = readRDS("results_se_df3.Rdata")
results_se_df5 = readRDS("results_se_df5.Rdata")

results_se_df2 = readRDS("results_se_df6.Rdata")


results_se_df6 = readRDS("results_se_df6.Rdata")
results_se_df7 = readRDS("results_se_df7.Rdata")
results_se_df8 = readRDS("results_se_df8.Rdata")

lapply(results_se_df5, function(x) round(mean(x),4))

# Boxplots
res1 = as.data.frame(results_se_df1)[,1:6]
colnames(res1) = c(colnames(res1)[2:5], colnames(res1)[1], colnames(res1)[6])
res1 = melt(res1)

res2 = as.data.frame(results_se_df2)[,1:6]
colnames(res2) = c(colnames(res2)[2:5], colnames(res2)[1], colnames(res2)[6])
res2 = melt(res2)

res3 = as.data.frame(results_se_df3)[,1:6]
colnames(res3) = c(colnames(res3)[2:5], colnames(res3)[1], colnames(res3)[6])
res3 = melt(res3)

res5 = as.data.frame(results_se_df5)[,1:6]
colnames(res5) = c(colnames(res5)[2:5], colnames(res5)[1], colnames(res5)[6])
res5 = melt(res5)

res6 = as.data.frame(results_se_df6)[,1:6]
colnames(res6) = c(colnames(res6)[2:5], colnames(res6)[1], colnames(res6)[6])
res6 = melt(res6)

res7 = as.data.frame(results_se_df7)[,1:6]
colnames(res7) = c(colnames(res7)[2:5], colnames(res7)[1], colnames(res7)[6])
res7 = melt(res7)

res8 = as.data.frame(results_se_df8)[,1:6]
colnames(res8) = c(colnames(res8)[2:5], colnames(res8)[1], colnames(res8)[6])
res8 = melt(res8)

res1
#df1
ggplot(res1 , aes(x = variable, y = value, color = variable)) + 
  geom_boxplot( show.legend = F)+
  ylim(0,0.06)+
  xlab("")+
  ylab("Dataframe 1")+
  scale_x_discrete(labels=c("pls.d1", "pls.opt", "pls.np.d1", "pls.np.opt","elastic net","XG boost")) +
  theme_light()+ 
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        panel.border = element_blank(),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 12)
    )

#df2
ggplot(res2 , aes(x = variable, y = value, color = variable)) + 
  geom_boxplot( show.legend = F)+
  ylim(0,0.06)+
  xlab("")+
  ylab("Dataframe 2")+
  scale_x_discrete(labels=c("pls.d1", "pls.opt", "pls.np.d1", "pls.np.opt","elastic net","XG boost")) +
  theme_light()+ 
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        panel.border = element_blank(),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 12)
  )

#df3/5
ggplot(res8 , aes(x = variable, y = value, color = variable)) + 
  geom_boxplot( show.legend = F)+
  ylim(0,0.05)+
  xlab("")+
  ylab("Dataframe 15")+
  scale_x_discrete(labels=c("pls.d1", "pls.opt", "pls.np.d1", "pls.np.opt","elastic net","XG boost")) +
  theme_light()+ 
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        panel.border = element_blank(),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 12)
  )

 

###################################################
#grafico experimento

all.equal(ytest, df_unaobs$mpi_Other)

df_unaobs_reg1 = df_unaobs[df_unaobs$region_Other == "South Asia",]
colnames(predichos_unaobs)[1] = "elastic_net"
colnames(predichos_unaobs) = c(colnames(predichos_unaobs)[2:5], colnames(predichos_unaobs)[1], colnames(predichos_unaobs)[6])



exper_unaobs = cbind(df_unaobs[, c(2,3,5)], predichos_unaobs)
 
exper_unaobs_mpi_small = exper_unaobs[(exper_unaobs$mpi_Other<0.006),]

exper_unaobs_mpi_medium = exper_unaobs[(exper_unaobs$mpi_Other>=0.006 & exper_unaobs$mpi_Other<0.1),]

exper_unaobs_mpi_big = exper_unaobs[(exper_unaobs$mpi_Other>=0.1),]


 
#barplot mpi
ggplot(exper_unaobs_mpi_small , aes(x = country_Other, y = mpi_Other)) + 
  geom_bar(stat = "identity", fill = "dodgerblue4",width = 0.5, show.legend = F)+
  xlab("")+
  ylab("MPI")+
  theme_light()+ 
  theme(panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey" ),
    panel.border = element_blank(),
    axis.text.x = element_text(angle = 70,  hjust=1)
  )

#predicteds 
exper_unaobs_mpi_small2 = melt(exper_unaobs_mpi_small[,-c(3)] )
exper_unaobs_mpi_medium2 = melt(exper_unaobs_mpi_medium[,-c(3)] )
exper_unaobs_mpi_big2 = melt(exper_unaobs_mpi_big[,-c(3)] )

ggplot(exper_unaobs_mpi_small2 , aes(x = country_Other, y =  value, color = variable )) + 
  geom_point()+
  labs(color='Methods') +
  xlab("")+
  ylab("MPI")+
  theme_light()+ 
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 70,  hjust=1)
  )

 #juntos

ggplot(data = NULL ) +
  geom_bar(data = exper_unaobs_mpi_small , aes(x = country_Other, y = mpi_Other),
           stat = "identity", fill = "dodgerblue4",width = 0.5, show.legend = F) + 
  geom_point(data = exper_unaobs_mpi_small2 , aes(x = country_Other, y =  value, color = variable,  size=1.8 )) + 
  labs(color='Methods', size ="") +
  scale_color_discrete(labels = c("pls.d1", "pls.opt", "pls.np.d1", "pls.np.opt","elastic net","XG boost"))+
  xlab("")+
  ylab("MPI")+
  coord_flip()+
  guides(size=FALSE, colour = guide_legend(override.aes = list(size=6)))+
  theme_light()+ 
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        panel.border = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)
        
  )

ggplot(data = NULL ) +
  geom_bar(data = exper_unaobs_mpi_medium , aes(x = country_Other, y = mpi_Other),
           stat = "identity", fill = "dodgerblue4",width = 0.5, show.legend = F) + 
  geom_point(data = exper_unaobs_mpi_medium2 , aes(x = country_Other, y =  value, color = variable,  size=1.8 )) + 
  labs(color='Methods', size ="") +
  scale_color_discrete(labels = c("pls.d1", "pls.opt", "pls.np.d1", "pls.np.opt","elastic net","XG boost"))+
  xlab("")+
  ylab("MPI")+
  coord_flip()+
  guides(size=FALSE, colour = guide_legend(override.aes = list(size=6)))+
  theme_light()+ 
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        panel.border = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)
        
  )


ggplot(data = NULL ) +
  geom_bar(data = exper_unaobs_mpi_big , aes(x = country_Other, y = mpi_Other),
           stat = "identity", fill = "dodgerblue4",width = 0.5, show.legend = F) + 
  geom_point(data = exper_unaobs_mpi_big2 , aes(x = country_Other, y =  value, color = variable,  size=1.8 )) + 
  labs(color='Methods', size ="") +
  scale_color_discrete(labels = c("pls.d1", "pls.opt", "pls.np.d1", "pls.np.opt","elastic net","XG boost"))+
  xlab("")+
  ylab("MPI")+
  coord_flip()+
  guides(size=FALSE, colour = guide_legend(override.aes = list(size=6)))+
  theme_light()+ 
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        panel.border = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)
        
  )



 
############################################################

# truncando predicciones
exper_unaobs_trun = exper_unaobs
for (j in 4:9) {
  for (i in 1:nrow(exper_unaobs)) {
    if (exper_unaobs_trun[i,j] < 0){
      exper_unaobs_trun[i,j] = 0
    }
  }
  
}

exper_unaobs_mpi_small_t = exper_unaobs_trun[(exper_unaobs_trun$mpi_Other<0.006),]

exper_unaobs_mpi_medium_t = exper_unaobs_trun[(exper_unaobs_trun$mpi_Other>=0.006 & exper_unaobs$mpi_Other<0.1),]

exper_unaobs_mpi_big_t = exper_unaobs_trun[(exper_unaobs_trun$mpi_Other>=0.1),]

exper_unaobs_mpi_small2_t = melt(exper_unaobs_mpi_small_t[,-c(3)] )
exper_unaobs_mpi_medium2_t = melt(exper_unaobs_mpi_medium_t[,-c(3)] )
exper_unaobs_mpi_big2_t = melt(exper_unaobs_mpi_big_t[,-c(3)] )


# graphs
ggplot(data = NULL ) +
  geom_bar(data = exper_unaobs_mpi_small_t , aes(x = country_Other, y = mpi_Other),
           stat = "identity", fill = "dodgerblue4",width = 0.5, show.legend = F) + 
  geom_point(data = exper_unaobs_mpi_small2_t , aes(x = country_Other, y =  value, color = variable,  size=1.8 )) + 
  labs(color='Methods', size ="") +
  scale_color_discrete(labels = c("pls.d1", "pls.opt", "pls.np.d1", "pls.np.opt","elastic net","XG boost"))+
  xlab("")+
  ylab("MPI")+
  coord_flip()+
  guides(size=FALSE, colour = guide_legend(override.aes = list(size=6)))+
  theme_light()+ 
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        panel.border = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)
        
  )

 
ggplot(data = NULL ) +
  geom_bar(data = exper_unaobs_mpi_medium_t , aes(x = country_Other, y = mpi_Other),
           stat = "identity", fill = "dodgerblue4",width = 0.5, show.legend = F) + 
  geom_point(data = exper_unaobs_mpi_medium2_t , aes(x = country_Other, y =  value, color = variable,  size=1.8 )) + 
  labs(color='Methods', size ="") +
  scale_color_discrete(labels = c("pls.d1", "pls.opt", "pls.np.d1", "pls.np.opt","elastic net","XG boost"))+
  xlab("")+
  ylab("MPI")+
  coord_flip()+
  guides(size=FALSE, colour = guide_legend(override.aes = list(size=6)))+
  theme_light()+ 
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        panel.border = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)
        
  )


ggplot(data = NULL ) +
  geom_bar(data = exper_unaobs_mpi_big_t , aes(x = country_Other, y = mpi_Other),
           stat = "identity", fill = "dodgerblue4",width = 0.5, show.legend = F) + 
  geom_point(data = exper_unaobs_mpi_big2_t , aes(x = country_Other, y =  value, color = variable,  size=1.8 )) + 
  labs(color='Methods', size ="") +
  scale_color_discrete(labels = c("pls.d1", "pls.opt", "pls.np.d1", "pls.np.opt","elastic net","XG boost"))+
  xlab("")+
  ylab("MPI")+
  coord_flip()+
  guides(size=FALSE, colour = guide_legend(override.aes = list(size=6)))+
  theme_light()+ 
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line( size=.1, color="grey" ),
        panel.border = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)
         
  )












