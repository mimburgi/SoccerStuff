library(dplyr)
library(ggplot2)
library(factoextra)
##fxns ####
plot_cluster=function(data, var_cluster, title="")
{
  library(RColorBrewer)
  palette<- colorRampPalette(brewer.pal(8, "Set2"))(length(levels(var_cluster)))
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
    geom_point(size=2) +
    scale_color_manual(values = palette) +
    xlab("") + ylab("") +
    ggtitle(title) +
    theme_light(base_size=20) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.box = "horizontal") 
}

## data read in and preproc ####


chain<-read.csv('ASA_PlayerxGChain_per96table.csv', stringsAsFactors = F)
A3pass<-read.csv('ASApassingtable-attackthird.csv', stringsAsFactors = F)
M3pass<-read.csv('ASApassingtable-middlethird.csv', stringsAsFactors = F)
D3pass<-read.csv('ASApassingtable-defthird.csv', stringsAsFactors = F)
shoot<-read.csv('ASAshootertable.csv', stringsAsFactors = F)
totalpass<-read.csv('ASApassingtable-total.csv', stringsAsFactors = F)
chain<-subset(chain, Minutes > 1200)
A3pass<-subset(A3pass, Min > 1200)
M3pass<-subset(M3pass, Min > 1200)
D3pass<-subset(D3pass, Min > 1200)
shoot<-subset(shoot, Min > 1200)
totalpass<-subset(totalpass, Min > 1200)

teams<-chain$Team

#trim to only per 90s
A3pass<-select(A3pass, matches(".96|Player|Pos"))
M3pass<-select(M3pass, matches(".96|Player|Pos"))
D3pass<-select(D3pass, matches(".96|Player|Pos"))
shoot<-select(shoot, matches(".96|Player|Pos"))
chain<-select(chain, matches(".96|Player|Pos|Team"))
totalpass<-select(totalpass, matches(".96|Player|Pos"))



allplayers<-c(chain$Player, A3pass$Player, M3pass$Player, D3pass$Player, shoot$Player, totalpass$Player)
allplayers<-unique(allplayers)

dat<-data.frame(Player=allplayers, 
                Pos=as.character(rep(NA, length(allplayers))),
                stringsAsFactors = F)
for (player in allplayers){
  
  #add Position and Minutes
  if(player %in% shoot$Player){
    #dat$InShooter[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-shoot$Pos[shoot$Player==player]
    dat$shots[dat$Player==player]<-shoot$Shots.96[shoot$Player==player]
    dat$KP[dat$Player==player]<-shoot$KeyP.96[shoot$Player==player]
    dat$xG[dat$Player==player]<-shoot$xG.96[shoot$Player==player]
    dat$xA[dat$Player==player]<-shoot$xA.96[shoot$Player==player]
  }
  if(player %in% A3pass$Player){
    #dat$InA3[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-A3pass$Pos[A3pass$Player==player]
    dat$A3Passes[dat$Player==player]<-A3pass$Passes.96[A3pass$Player==player]
  }
  if(player %in% M3pass$Player){
    #dat$InM3[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-M3pass$Pos[M3pass$Player==player]
    dat$M3Passes[dat$Player==player]<-M3pass$Passes.96[M3pass$Player==player]
  }
  if(player %in% D3pass$Player){
    #dat$InD3[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-D3pass$Pos[D3pass$Player==player]
    dat$D3Passes[dat$Player==player]<-D3pass$Passes.96[D3pass$Player==player]
  }
  if(player %in% chain$Player){
    #dat$InChain[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-chain$Pos[chain$Player==player]
    dat$NumChains[dat$Player==player]<-chain$NumChains.96[chain$Player==player]
    dat$xGChain[dat$Player==player]<-chain$xGChain.96[chain$Player==player]
    dat$xB[dat$Player==player]<-chain$xB.96[chain$Player==player]
    dat$Team[dat$Player==player]<-chain$Team[chain$Player==player]
    
  }
  if(player %in% totalpass$Player){
    dat$Vertical[dat$Player==player]<-totalpass$Vertical.96[totalpass$Player==player]
    dat$PassPct[dat$Player==player]<-totalpass$PassPct.96[totalpass$Player==player]
    dat$PassDistance[dat$Player==player]<-totalpass$Distance.96[totalpass$Player==player]
    dat$TouchPerc[dat$Player==player]<-totalpass$Touch..96[totalpass$Player==player]
    
  }
}

dat[is.na(dat)]<-0 #assuming missing vals mean zeros
dat<-subset(dat, Pos != "GK")

## kmeans ####
scaled<-select_if(dat, is.numeric) %>% scale() %>% as.data.frame()

fviz_nbclust(scaled, kmeans, method = 'wss')


kmm<-kmeans(scaled, centers = 7)


scaled$cluster<-kmm$cluster

groupmeans<-aggregate(scaled, by=list(cluster=kmm$cluster), mean)

testing<-c('1','3')
groupmeans[(groupmeans$cluster %in% testing),]

scaled$Pos<-dat$Pos
scaled$Player<-dat$Player
scaled$Team<-dat$Team
scaled$clustername<-as.factor(scaled$cluster)
levels(scaled$clustername)<-c("low contributing MF", 'primary creator', 'low contributing defender', 'DLP', 'support attacker', 'skilled passing defender', 'finisher')
View(scaled)

## viz ####
toplot<-select(scaled, -cluster) %>% select_if(is.numeric)
fviz_cluster(kmm, toplot)

library(tidyr)
Team_comp<-scaled %>% group_by(Team) %>% count(clustername)


scaled %>% group_by(Team)
summary(scaled$clustername)
View(scaled[scaled$Team=='LAFC',c("clustername", "Player", "Pos")])

