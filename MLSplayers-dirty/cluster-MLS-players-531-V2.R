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
totalpass<-select(totalpass, matches(".96|Player|Pos|Score"))



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
    dat$PassScore[dat$Player==player]<-totalpass$Score.96[totalpass$Player==player]
  }
}

dat[is.na(dat)]<-0 #assuming missing vals mean zeros
dat<-subset(dat, Pos != "GK")

dat$A3perc<-dat$A3Passes/dat$Passes
dat$M3perc<-dat$M3Passes/dat$Passes
dat$D3perc<-dat$D3Passes/dat$Passes

## major styles ####
descstats<-c("shots", "KP", 'A3perc', 'M3perc', 'D3perc', 
             'percChain', 'ShotChainPerc', 'KPChainPerc', 
             'xGchain', 'xBperc', 'xB')
allscaled<-dat %>% select_if(is.numeric) %>% scale() %>% as.data.frame()
scaled<-allscaled %>% select_if(names(.) %in% descstats) 
scaled[scaled < -3]<- -3
scaled[scaled > 3]<- 3


library(FactoMineR)
pca<-PCA(scaled)
fviz_screeplot(pca)
pcadecomp<-preProcess(scaled, method='pca', pcaComp = 3) %>% predict(scaled)


set.seed(100)
kmm<-kmeans(scaled, centers = 3)
library(NbClust)
kmeansnb<-NbClust(scaled, max.nc = 7, method = c('kmeans'))
fviz_nbclust(kmeansnb)

plotclusters(scaled, kmm$cluster)
clusters<-as.factor(kmm$cluster)
levels(clusters)<-c('M3', 'D3', 'A3')

dat$role<-clusters
### separate roles ####

mf<-dat[dat$role=='M3',] 
att<-dat[dat$role=='A3',] 
def<-dat[dat$role=='D3',] 

mfs<-which(dat$role=='M3')
defs<-which(dat$role=='D3')
atts<-which(dat$role=='A3')

## attack ####
select_if(att,is.numeric) %>% apply(2, rsd)
candidate_finvars<-c('xG', 'xA', 'shots', 'KP', 'percChain', 'KPChainPerc',
                     'ShotChainPerc', 'xGChain')
finscaled<-select(dat[atts,], one_of(candidate_finvars)) %>% scale() %>% as.data.frame()
pcafin<-PCA(finscaled)
fviz_screeplot(pcafin)
for (i in 1:3){
  plot(fviz_contrib(pcafin, choice = 'var', axes=i))
}
#3 = freq of involvement
#2 = shooting
#1 = general chance creation
findecomp<-preProcess(finscaled, method = 'pca', pcaComp = 3) %>% predict(finscaled)
findecomp2<-preProcess(finscaled, method = 'pca', pcaComp = 2) %>% predict(finscaled)

attnb<-NbClust(findecomp2, max.nc = 7, method = c('kmeans'))

fviz_pca_var(pcafin)

attnb<-NbClust(findecomp2, max.nc = 7, method = c('kmeans'))
set.seed(50)
attfuzzy<-fanny(findecomp2, 3)
plotclusters(findecomp2, attfuzzy$clustering)
plotclusters(finscaled, attfuzzy$clustering)

attclusts<-as.factor(attfuzzy$clustering)
levels(attclusts)<-c('creative forward', 'finisher', 'support forward')
  
att$subrole<-attclusts
att$Pos<-as.factor(att$Pos)

summary(att$Pos)
summary(att$Pos[att$subrole=='support forward'])

## defenders ####
select_if(def, is.numeric) %>% apply(2, rsd)
defstats<-c('xBperc', 'xGChain', 'percChain', 'PassScore', 'Vertical', 'PassDistance')
defscaled<-select(dat[defs,], one_of(defstats)) %>% scale() %>% as.data.frame()

pcadef<-PCA(defscaled)
fviz_screeplot(pcadef)
for (i in 1:3){
  plot(fviz_contrib(pcadef, choice = 'var', axes=i))
}
#3 = goal buildup influence
#2 = passing directness
#1 = overall passing involvement
defdecomp<-preProcess(defscaled, method = 'pca', pcaComp = 3) %>% predict(defscaled)
defdecomp2<-preProcess(defscaled, method = 'pca', pcaComp = 2) %>% predict(defscaled)
defdecomp3<-preProcess(defscaled, method = 'pca', pcaComp = 1) %>% predict(defscaled)

defnb<-NbClust(defdecomp, max.nc = 7, method = c('ward.D2'))
fviz_nbclust(defnb)
defnb<-NbClust(defdecomp2, max.nc = 7, method = c('kmeans'))

set.seed(50)
deffuzzy<-fanny(defdecomp3, 2)
plotclusters(defdecomp3, deffuzzy$clustering)
plotclusters(defscaled, deffuzzy$clustering)

defclusters<-as.factor(deffuzzy$clustering)
levels(defclusters)<-c('Back Third General', 'Back Third Outlet')
def$subrole<-defclusters

def$Pos<-as.factor(def$Pos)
summary(def$Pos)
summary(def$Pos[def$subrole=='Back Third General'])
summary(def$Pos[def$subrole=='Back Third Outlet'])



## midfielders ####
select_if(mf, is.numeric) %>% apply(2, rsd)
mfstats<-c('Shots','KP','xG','xA', 'xGChain', 'xB', 'KPChainPerc', 'percChain', 'xBperc')
mfscaled<-allscaled %>% select_if(names(.) %in% mfstats)
mfscaled<-mfscaled[mfs,]

pcamf<-PCA(mfscaled)
fviz_screeplot(pcamf)
for (i in 1:3){
  plot(fviz_contrib(pcamf, choice = 'var', axes=i))
}
#3 = goals
#2 = indirect creation
#1 = direct creation
mfdecomp<-preProcess(mfscaled, method = 'pca', pcaComp = 3) %>% predict(mfscaled)
mfdecomp2<-preProcess(mfscaled, method = 'pca', pcaComp = 2) %>% predict(mfscaled)
mfdecomp3<-preProcess(mfscaled, method = 'pca', pcaComp = 1) %>% predict(mfscaled)

mfnb<-NbClust(mfdecomp, max.nc = 7, method = c('kmeans'))
fviz_nbclust(mfnb)

set.seed(50)
mffuzzy<-fanny(mfdecomp2, 2)
plotclusters(mfdecomp2, mffuzzy$clustering)
plotclusters(mfscaled, mffuzzy$clustering)

mfclusters<-as.factor(mffuzzy$clustering)
levels(mfclusters)<-c('Direct MF Creator', 'Indirect MF Creator')
mf$subrole<-mfclusters

mf$Pos<-as.factor(mf$Pos)
summary(mf$Pos)
summary(mf$Pos[mf$subrole=='Direct MF Creator'])
summary(mf$Pos[mf$subrole=='Indirect MF Creator'])


#### final summary ####
dat$subtype[dat$role=='A3']<-as.character(att$subrole)
dat$subtype[dat$role=='M3']<-as.character(mf$subrole)
dat$subtype[dat$role=='D3']<-as.character(def$subrole)

write.table(dat, 'labeled4.txt', row.names = F)


totals<-select_if(dat, is.numeric) %>% aggregate(by=list(cluster=dat$subtype), median)
View(totals)

