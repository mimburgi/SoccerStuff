library(dplyr)
library(ggplot2)
source('soccer_util_fxns.R')
library(caret)


## data read in and preproc ####


chain<-read.csv('./2019/ASA_PlayerxGChain_per96table.csv', stringsAsFactors = F)
A3pass<-read.csv('./2019/ASApassingtable-attackthird.csv', stringsAsFactors = F)
M3pass<-read.csv('./2019/ASApassingtable-middlethird.csv', stringsAsFactors = F)
D3pass<-read.csv('./2019/ASApassingtable-defthird.csv', stringsAsFactors = F)
shoot<-read.csv('./2019/ASAshootertable.csv', stringsAsFactors = F)
totalpass<-read.csv('./2019/ASApassingtable-total.csv', stringsAsFactors = F)
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
shoot<-select(shoot, matches(".96|Player|Pos|Dist"))
chain<-select(chain, matches(".96|Player|Pos|Team|xB."))
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
    dat$xPlace[dat$Player==player]<-shoot$xPlace.96[shoot$Player==player]
    dat$ShotDist[dat$Player==player]<-shoot$Dist[shoot$Player==player]
    dat$KPDist[dat$Player==player]<-shoot$Dist.key[shoot$Player==player]
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
    dat$percChain[dat$Player==player]<-chain$TeamChain.[chain$Player==player]
    dat$xGChain[dat$Player==player]<-chain$xGChain.96[chain$Player==player]
    dat$xB[dat$Player==player]<-chain$xB.96[chain$Player==player]
    dat$Team[dat$Player==player]<-chain$Team[chain$Player==player]
    dat$ShotChainPerc[dat$Player==player]<-chain$PlayerShot.[chain$Player==player]
    dat$KPChainPerc[dat$Player==player]<-chain$PlayerKP.[chain$Player==player]
    dat$xBperc[dat$Player==player]<-chain$xB.[chain$Player==player]
    
  }
  if(player %in% totalpass$Player){
    dat$Vertical[dat$Player==player]<-totalpass$Vertical.96[totalpass$Player==player]
    dat$PassPct[dat$Player==player]<-totalpass$PassPct.96[totalpass$Player==player]
    dat$PassDistance[dat$Player==player]<-totalpass$Distance.96[totalpass$Player==player]
    dat$TouchPerc[dat$Player==player]<-totalpass$Touch..96[totalpass$Player==player]
    dat$xPassPerc[dat$Player==player]<-totalpass$xPassPct.96[totalpass$Player==player]
    dat$Passes[dat$Player==player]<-totalpass$Passes.96[totalpass$Player==player]
  }
}

dat[is.na(dat)]<-0 #assuming missing vals mean zeros
dat<-subset(dat, Pos != "GK")


dat$percA3pass<-dat$A3Passes/dat$Passes
dat$percM3pass<-dat$M3Passes/dat$Passes
dat$percD3pass<-dat$D3Passes/dat$Passes


## tsne ####
initials<-c('percA3pass', 'percM3pass', 'percD3pass', 'percChain', 
'shots', 'KP', 'Passes')
# 
# scalefn<-select(dat, one_of(initials)) %>% preProcess(method=c('BoxCox', 'scale', 'center'))
# scaled<-predict(scalefn, select(dat, one_of(initials)))

scale<-select(dat, one_of(initials)) %>% scale() %>% as.matrix()
scaled[scaled < -3]<- -3

library(Rtsne)
set.seed(100)
tsne<-Rtsne(scaled, perplexity = 16) 
plot(tsne$Y)

dtsne<-as.data.frame(tsne$Y)

## dbscan ####
dbscan::kNNdistplot(dtsne, k =  4)
fit<-dbscan::dbscan(dtsne, minPts = 4, eps = 3)
clusters<-factor(fit$cluster)
plot_cluster(dtsne, clusters)

scaled<-as.data.frame(scaled)

plotclusters(scaled, clusters)

levels(clusters)<-c('tweener', 'MF distributor', 'B2B support', 'attacker', 'defender')

dat$maintype<-clusters



## break into groups ####
dists<-which(dat$maintype=='MF distributor')
supp<-which(dat$maintype=='B2B support')
att<-which(dat$maintype=='attacker')
def<-which(dat$maintype=='defender')



distscaled<-dat[dists,] %>% select(one_of(c('xG', 'xA', 'xB'))) %>% scale() %>% as.data.frame()

library(Rtsne)
set.seed(100)
tsne<-Rtsne(distscaled, perplexity = ) 
plot(tsne$Y)
dtsne<-as.data.frame(tsne$Y)

dbscan::kNNdistplot(dtsne, k =  4)
fit<-dbscan::dbscan(dtsne, minPts = 4, eps = 2.8)
clusters<-factor(fit$cluster)
plot_cluster(dtsne, clusters)

plotclusters(distscaled, clusters)

levels(clusters)<-c('tweener', 'MF distributor', 'B2B support', 'attacker', 'defender')

