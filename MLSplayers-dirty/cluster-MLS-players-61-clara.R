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

dat$xGper<-dat$xG/dat$shots
dat$xAper<-dat$xA/dat$KP

dat[is.na(dat)]<-0 #assuming missing vals mean zeros

## major styles ####
stats<-c('xGchain', 'xBperc', 'xB', 'xG', 'xA',
         'PassScore', 'xPassPct', 'percChain')
allscaled<-dat %>% select_if(is.numeric) %>% scale() %>% as.data.frame()
scaled<-allscaled %>% select_if(names(.) %in% stats) 
scaled[scaled < -3]<- -3
scaled[scaled > 3]<- 3




library(FactoMineR)
pca<-PCA(scaled)
fviz_screeplot(pca)
pcadecomp<-preProcess(scaled, method='pca', pcaComp = 3) %>% predict(scaled)


library(clValid)

set.seed(20)
tsne<-Rtsne(as.matrix(allscaled), perplexity=17)
plot(tsne$Y)
dtsne<-tsne$Y
comp1<-clValid(dtsne, 
              nClust=2:12, 
              clMethods = c('hierarchical', 'pam','clara', 
                            'diana', 'som','agnes'),
              validation=c('internal'),
              method = 'complete')
summary(comp1)

comp1<-clValid(dtsne, 
               nClust=2:12, 
               clMethods = c('hierarchical', 'pam','clara', 
                             'diana', 'som','agnes'),
               validation=c('internal'),
               method = 'ward')
summary(comp1)





get_clust_tendency(dtsne, 4)

clara<

res.hc <- eclust(dtsne, "hclust", k = 5,
                 method = "ward.D2", graph = FALSE) 
grp <- res.hc$cluster



clus.centers <- aggregate(dtsne, list(grp), mean)
clus.centers <- clus.centers[, -1]
km.res2 <- eclust(dtsne, "kmeans", k = clus.centers, graph = FALSE)
fviz_silhouette(km.res2)

fviz_dend(res.hc, k = 4, 
          label_cols =  km.res2$cluster[res.hc$order], cex = 0.6)



hc<-hclust(dist(dtsne), method = 'average')
cutfive<-cutree(hc, 5)

plotclusters(scaled, km.res2$cluster)
kmnames<-c('finisher', 'facilitator', 'support')

fviz_dend(hc, k=5)

clusters<-as.factor(kmm$cluster)
levels(clusters)<-kmnames

dat$role<-clusters



### separate roles ####

fac<-dat[dat$role=='facilitator',] 
fin<-dat[dat$role=='finisher',] 
supp<-dat[dat$role=='support',] 

facs<-which(dat$role=='facilitator')
fins<-which(dat$role=='finisher')
supps<-which(dat$role=='support')

## try fuzzy clustering of roles
library(ppclust)
set.seed(12)
fuzzyrole<-fcm(scaled, 3)

plotclusters(scaled, fuzzyrole$cluster)
fuzzynames<-c('support', 'finisher', 'facilitator')

fuzzyprobs<-fuzzyrole$u
colnames(fuzzyprobs)<-fuzzynames

fuzzyrolenamed<-as.factor(fuzzyrole$cluster)
levels(fuzzyrolenamed)<-fuzzynames
kmrolenamed<-as.factor(kmm$cluster)
levels(kmrolenamed)<-kmnames


sum(fuzzyrolenamed==kmrolenamed)/length(kmrolenamed)
#94 percent agreement
disagree<-which(fuzzyrolenamed!=kmrolenamed)

fuzzyprobs<-as.data.frame(fuzzyprobs)
secondprobs<-apply((fuzzyprobs),1,secondmax) 

fuzzyprobsUN<-mutate(fuzzyprobs, maxprob=apply(fuzzyprobs,1,max)) 
secondfins<-which(!is.na(match(fuzzyprobs[,2],secondprobs)))
secondsupps<-which(!is.na(match(fuzzyprobs[,1],secondprobs)))
secondfacs<-which(!is.na(match(fuzzyprobs[,3],secondprobs)))
fuzzyprobsUN$secondrole[secondfacs]<-'facilitator'
fuzzyprobsUN$secondrole[secondfins]<-'finisher'
fuzzyprobsUN$secondrole[secondsupps]<-'support'

fuzzyprobsUN$secondrole[fuzzyprobsUN$maxprob>.7]<-NA
fuzzyprobsUN$uncertain<-ifelse(fuzzyprobsUN$maxprob < .7, T, F)  

sum(fuzzyprobsUN$uncertain[disagree])/length(disagree)
#this threshold captures 100% of disagreement across methods

datUN<-dat
datUN$Pos<-as.factor(datUN$Pos)
datUN$primaryrole<-fuzzyrolenamed
datUN$secondaryrole<-fuzzyprobsUN$secondrole


datUN$hybridrole[is.na(datUN$secondaryrole)]<-as.character(datUN$primaryrole[is.na(datUN$secondaryrole)])
datUN$hybridrole[(datUN$primaryrole=='finisher' | datUN$primaryrole=='facilitator') &
             (datUN$secondaryrole=='finisher' | datUN$secondaryrole=='facilitator')]<-"creative finisher"
datUN$hybridrole[(datUN$primaryrole=='support' | datUN$primaryrole=='facilitator') &
                   (datUN$secondaryrole=='support' | datUN$secondaryrole=='facilitator')]<-"support facilitator"

datUN$hybridrole<-as.factor(datUN$hybridrole)

colnames(fuzzyprobs)<-paste0(colnames(fuzzyprobs), 'prob')


summary(datUN$hybridrole)


#write.table(datUN, 'labeled5.txt', row.names = F)

## separate groups ####
fin<-which(datUN$hybridrole=='finisher')
cf<-which(datUN$hybridrole=='creative finisher')
fac<-which(datUN$hybridrole=='facilitator')
sf<-which(datUN$hybridrole=='support facilitator')
s<-which(datUN$hybridrole=='support')

## goal scoring rating ####
scorers<-which(datUN$hybridrole=='finisher' |
                 datUN$hybridrole=='creative finisher')

goalscores<-(scale(datUN$xG[scorers])*.8)*2 + 5.5

## direct creation rating ####
passers<-which(datUN$hybridrole=='finisher' |
                 datUN$hybridrole=='creative finisher' | 
                 datUN$hybridrole == 'facilitator',)

assistscores<-(scale(datUN$xA[passers])*.7)*2 + 5.5

## indirect creation rating ####
builders<-which(datUN$hybridrole=='creative finisher' | 
                 datUN$hybridrole == 'facilitator' | 
                 datUN$hybridrole == 'support facilitator' | 
                 datUN$hybridrole == 'support',)
indirectscores<-(scale(datUN$xB[builders])*.6)*2 + 5.5
  


## add ratings ####
datUN$goalrating<-NA
datUN$goalrating[scorers]<-goalscores
datUN$directcreationrating<-NA
datUN$directcreationrating[passers]<-assistscores
datUN$indirectcreationrating<-NA
datUN$indirectcreationrating[builders]<-indirectscores

View(datUN[,c('Player', 'Team', 'hybridrole', 'goalrating', 'directcreationrating', 'indirectcreationrating')])
