library(dplyr)
library(ggplot2)
library(factoextra)
source('soccer_util_fxns.R')
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

## major styles ####
descstats<-c("shots", "KP", 'A3Passes', 'M3Passes', 'D3Passes', 'percChain', 'ShotChainPerc', 'KPChainPerc', 'Vertical', 'PassPct', 'PassDistance', 'TouchPerc', 'ShotDistance', 'KPDistance')
allscaled<-dat %>% select_if(is.numeric) %>% scale() %>% as.data.frame()
scaled<-allscaled %>% select_if(names(.) %in% descstats) 
fviz_nbclust(scaled, kmeans, method = 'wss')

set.seed(0)
kmm<-kmeans(scaled, centers = 4)

fviz_cluster(kmm, scaled)

scaled$cluster<-kmm$cluster

aggregate(scaled, by=list(cluster=kmm$cluster), median)


scaled$Pos<-dat$Pos
scaled$Player<-dat$Player
scaled$Team<-dat$Team
scaled$clustername<-as.factor(scaled$cluster)
levels(scaled$clustername)<-c("support attacker", 'midfielder', 'defender', 'primary attacker')

dat$role<-scaled$clustername


### separate roles ####

def<-dat[dat$role=='defender',] 
mf<-dat[dat$role=='midfielder',] 
finisher<-dat[dat$role=='primary attacker',] 
support<-dat[dat$role=='support attacker',] 
finishers<-which(dat$role=='primary attacker')
defs<-which(dat$role=='defender')
supports<-which(dat$role=='support attacker')
midfielders<-which(dat$role=='midfielder')


## finishers ####
select_if(finisher, is.numeric) %>% apply(2, rsd)
finisherstats<-c('xG', 'xA', 'shots', 'KP', 'ShotChainPerc', 'KPChainPerc', 'xGChain', 'ShotDist')
finisherscaled<-allscaled %>% select_if(names(.) %in% finisherstats)
finisherscaled<-finisherscaled[finishers,]

fviz_nbclust(finisherscaled, kmeans, method = 'wss')

set.seed(25)
finisherkmm<-kmeans(finisherscaled, centers = 3)

# nearestcenter(1, finisherscaled, finisherkmm, finisher$Player)
# nearestcenter(2, finisherscaled, finisherkmm, finisher$Player)
# nearestcenter(3, finisherscaled, finisherkmm, finisher$Player)


fviz_cluster(finisherkmm, finisherscaled) #clear separations except for 3 and 7, might be on Dim 3

plotclusters(finisherscaled, finisherkmm$cluster)

finisher$subtype<-as.factor(finisherkmm$cluster)
levels(finisher$subtype)<-c('Prolific Finisher', 'Hybrid Attacker', 'Secondary Finisher')
finisherscaled$cluster = as.factor(finisherkmm$cluster)
finisherscaled$Player = finisher$Player
finisherscaled$Pos = finisher$Pos

## defenders ####
select_if(def, is.numeric) %>% apply(2, rsd)
defstats<-c('xB', 'xGChain', 'percChain', 'M3Passes','D3Passes')
def_scaled<-allscaled %>% select_if(names(.) %in% defstats)
def_scaled<-def_scaled[defs,]

fviz_nbclust(def_scaled, kmeans, method = 'wss')

set.seed(25)
defkmm<-kmeans(def_scaled, centers = 3)

nearestcenter(1, def_scaled, defkmm, def$Player)
nearestcenter(2, def_scaled, defkmm, def$Player)
nearestcenter(3, def_scaled, defkmm, def$Player)

fviz_cluster(defkmm, def_scaled) 

plotclusters(def_scaled, defkmm$cluster)

def$subcluster<-defkmm$cluster
def$subtype<-as.factor(def$subcluster)
levels(def$subtype)<-c('Initiator', 'Backfield Outlet','Backfield Hub')

def_scaled$cluster = as.factor(defkmm$cluster)
def_scaled$Player = def$Player



## midfielders ####
select_if(mf, is.numeric) %>% apply(2, rsd)
mfstats<-c('Shots','KP','xG','xA','A3Passes', 'xGChain', 'xB', 'KPChainPerc', 'percChain')
mf_scaled<-allscaled %>% select_if(names(.) %in% mfstats)
mf_scaled<-mf_scaled[midfielders,]

fviz_nbclust(mf_scaled, kmeans, method = 'wss')

set.seed(25)
mfkmm<-kmeans(mf_scaled, centers = 2)

fviz_cluster(mfkmm, mf_scaled) 

plotclusters(mf_scaled, mfkmm$cluster)

aggregate(mf_scaled, by=list(cluster=mfkmm$cluster), median)

mf$subcluster<-mfkmm$cluster
mf$subtype<-as.factor(mf$subcluster)
levels(mf$subtype)<-c('DLP', 'MF Recycler')

mf_scaled$cluster = as.factor(mfkmm$cluster)
mf_scaled$Player = mf$Player
mf_scaled$Pos = mf$Pos

## support attackers ####

select_if(support, is.numeric) %>% apply(2, rsd)
supportstats<-c('xA', 'KP', 'A3Passes','percChain','xGChain', 'xB', 'KPChainPerc', 'ShotChainPerc')
support_scaled<-allscaled %>% select_if(names(.) %in% supportstats)
support_scaled<-support_scaled[supports,]


fviz_nbclust(support_scaled, kmeans, method = 'wss')

set.seed(25)
supportkmm<-kmeans(support_scaled, centers = 3)

fviz_cluster(supportkmm, support_scaled) #clear separations except for 3 and 7, might be on Dim 3
FactoMineR::PCA(support_scaled, graph = F) %>% fviz_contrib(choice = 'var', axes=1)
#  supportkmm, support_scaled) #clear separations except for 3 and 7, might be on Dim 3

plotclusters(support_scaled, supportkmm$cluster)

aggregate(support_scaled, by=list(cluster=supportkmm$cluster), median)

support$subcluster<-supportkmm$cluster
support$subtype<-as.factor(support$subcluster)
levels(support$subtype)<-c('Primary Creator', 'Upfield Recycler', 'Secondary Creator')

support_scaled$cluster = as.factor(supportkmm$cluster)
support_scaled$Player = support$Player
support_scaled$Pos = support$Pos


#### final summary ####
dat$subtype[dat$role=='primary attacker']<-as.character(finisher$subtype)
dat$subtype[dat$role=='support attacker']<-as.character(support$subtype)
dat$subtype[dat$role=='defender']<-as.character(def$subtype)
dat$subtype[dat$role=='midfielder']<-as.character(mf$subtype)

write.table(dat, 'labeled2.txt', row.names = F)


totals<-select_if(dat, is.numeric) %>% aggregate(by=list(cluster=dat$subtype), median)
View(totals)

