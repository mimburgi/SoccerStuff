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
chain<-select(chain, matches(".96|Player|Pos|Team|xB.|Minutes"))
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
    dat$Min[dat$Player==player]<-chain$Minutes[chain$Player==player]
    
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

## major styles ####
descstats<-c("shots", "KP", 'percA3pass', 'percM3pass', 'percD3pass', 'percChain', 'ShotChainPerc', 'KPChainPerc', 'ShotDistance')
allscaled<-dat %>% select_if(is.numeric) %>% scale() %>% as.data.frame()
scaled<-allscaled %>% select_if(names(.) %in% descstats) 
fviz_nbclust(scaled, kmeans, method = 'wss')

set.seed(0)
kmm<-kmeans(scaled, centers = 5)

fviz_cluster(kmm, scaled)

scaled$cluster<-kmm$cluster

aggregate(scaled, by=list(cluster=kmm$cluster), median)


scaled$Pos<-dat$Pos
scaled$Player<-dat$Player
scaled$Team<-dat$Team
scaled$clustername<-as.factor(scaled$cluster)
levels(scaled$clustername)<-c("midfielder", 'defender', 'attacker')

dat$role<-scaled$clustername


### separate roles ####

def<-dat[dat$role=='defender',] 
mf<-dat[dat$role=='midfielder',] 
attacker<-dat[dat$role=='attacker',] 
attackers<-which(dat$role=='attacker')
defs<-which(dat$role=='defender')
midfielders<-which(dat$role=='midfielder')


## attackers ####
#select_if(attacker, is.numeric) %>% apply(2, rsd)
attackerstats<-c('xG', 'xA', 'shots', 'KP', 'ShotChainPerc', 'KPChainPerc', 'xGChain', 'ShotDist')
attackerscaled<-allscaled %>% select_if(names(.) %in% attackerstats)
attackerscaled<-attackerscaled[attackers,]

fviz_nbclust(attackerscaled, kmeans, method = 'wss')

set.seed(25)
attackerkmm<-kmeans(attackerscaled, centers = 3)

# nearestcenter(1, attackerscaled, attackerkmm, attacker$Player)
# nearestcenter(2, attackerscaled, attackerkmm, attacker$Player)
# nearestcenter(3, attackerscaled, attackerkmm, attacker$Player)


fviz_cluster(attackerkmm, attackerscaled) #clear separations except for 3 and 7, might be on Dim 3

plotclusters(attackerscaled, attackerkmm$cluster)

attacker$subtype<-as.factor(attackerkmm$cluster)
levels(attacker$subtype)<-c('Complete Attacker', 'Prolific Scorer', 'Secondary scorer')
attackerscaled$cluster = as.factor(attackerkmm$cluster)
attackerscaled$Player = attacker$Player
attackerscaled$Pos = attacker$Pos

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
levels(def$subtype)<-c('Deep Lying MF', 'Defensive Outlet','Primary Defensive Distributor')

def_scaled$cluster = as.factor(defkmm$cluster)
def_scaled$Player = def$Player



## midfielders ####
select_if(mf, is.numeric) %>% apply(2, rsd)
mfstats<-c('Shots','KP','xA','A3Passes', 'KPChainPerc')
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
levels(mf$subtype)<-c('Creator', 'MF Link')

mf_scaled$cluster = as.factor(mfkmm$cluster)
mf_scaled$Player = mf$Player
mf_scaled$Pos = mf$Pos


#### final summary ####
dat$subtype[dat$role=='attacker']<-as.character(attacker$subtype)
dat$subtype[dat$role=='defender']<-as.character(def$subtype)
dat$subtype[dat$role=='midfielder']<-as.character(mf$subtype)



write.table(dat, 'labeled3.txt', row.names = F)



totals<-select_if(dat, is.numeric) %>% aggregate(by=list(cluster=dat$subtype), median)
View(totals)


res.hca<-dist(scaled, method = "euclidean") %>%
  hclust(method = 'ward.D2' )
plot(fviz_dend(res.hca, cex = 0.5, k = 6, color_labels_by_k = TRUE))
labels<-cutree(res.hca, 6)
plotclusters(scaled, labels)
