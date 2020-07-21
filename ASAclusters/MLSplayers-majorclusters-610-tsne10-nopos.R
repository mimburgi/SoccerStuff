## package requirements ####

library(dplyr)
library(ggplot2)

#network graph packages
library(network)
library(igraph)
library(bipartite)

#radar graph packages
library(fmsb)
library(RColorBrewer)
library(scales)

#cluster packages
library(factoextra)
library(cluster)
library(FactoMineR)

#for my plotclusters fxn
source('soccer_util_fxns.R')
## concat data across both years ####
curr<-read.table('2019summary.txt', header = T)
currscaled<-curr %>% select_if(is.numeric) %>% scale() %>% as.data.frame()
# currscaled<-select(currscaled, -c(xGteamperc, shotteamperc, A3Passes, M3Passes, D3Passes)) %>%
#   select(-c(teamA3pass, teamM3pass, teamD3pass, teampass, teamxG, teamshots, xPlace, ShotDist, KPDist,
#             xGper, xAper, passteamperc))
currscaled[currscaled < -3]<- -3

prev<-read.table('2018summary.txt', header = T)
prevscaled<-prev %>% select_if(is.numeric) %>% scale() %>% as.data.frame()
# prevscaled<-select(prevscaled, -c(xGteamperc, shotteamperc, A3Passes, M3Passes, D3Passes)) %>%
#   select(-c(teamA3pass, teamM3pass, teamD3pass, teampass, teamxG, teamshots, xPlace, ShotDist, KPDist,
#             xGper, xAper, passteamperc))

#to separate out later
currrows<-c(1:nrow(currscaled))

bothscaledall<-rbind(currscaled, prevscaled)

## trim and explore ####


usedvars<-c("shots", "npxG", 'ShotChainPerc', "KP", 
            'xA',  "KPChainPerc", "xGChain", 
            "xB", "xPassPerc", "PassPct", 'passprg',
            'CarryPrgDis','AttDrib','Crs','penCrs','Vertical', 
            'targeted', 'PressPass') 




bothscaled<-select(bothscaledall, all_of(usedvars))

bothscaled[bothscaled > 3] <- 3
bothscaled[bothscaled < -3] <- -3

#make a graph of hopkins stats
hopkinsdf<-data.frame(clusters=as.numeric(1), stat=as.numeric(NA))
for (i in 2:18){
  hopstat<-get_clust_tendency(bothscaled, i, graph = F)$hopkins_stat
  hopkinsdf[nrow(hopkinsdf) + 1,]<-c(i, hopstat)
}
hopkinsdf<-hopkinsdf[-1,]

ggplot(hopkinsdf, aes(x=clusters, y=stat)) + geom_point() + geom_line() +
  ggtitle('hopkins statistic, scaled')


# #try pca
# #decreases the hopkins stat across the board
# fviz_screeplot(PCA(bothscaled, graph = F))
# 
# pcascaled<-caret::preProcess(bothscaled, method='pca', pcaComp=5) %>% predict(bothscaled)
# hopkinsdfpca<-data.frame(clusters=as.numeric(1), stat=as.numeric(NA))
# for (i in 2:18){
#   hopstat<-get_clust_tendency(pcascaled, i, graph = F)$hopkins_stat
#   hopkinsdfpca[nrow(hopkinsdfpca) + 1,]<-c(i, hopstat)
# }
# hopkinsdfpca<-hopkinsdfpca[-1,]
# 
# ggplot(hopkinsdfpca, aes(x=clusters, y=stat)) + geom_point() + geom_line() +
#   ggtitle('hopkins statistic, pca')


#try tsne
#huge increase

set.seed(20)
tsne<-Rtsne::Rtsne(bothscaled, perplexity=37)$Y #17
plot(tsne)
hopkinsdftsne<-data.frame(clusters=as.numeric(1), stat=as.numeric(NA))
for (i in 2:15){
  hopstat<-get_clust_tendency(tsne, i, graph = F)$hopkins_stat
  hopkinsdftsne[nrow(hopkinsdftsne) + 1,]<-c(i, hopstat)
}
hopkinsdftsne<-hopkinsdftsne[-1,]

ggplot(hopkinsdftsne, aes(x=clusters, y=stat)) + geom_point() + geom_line() +
  ggtitle('hopkins statistic, tsne')

## set pars ####
k=10
input=tsne


## cluster ####

set.seed(20)
clara<-clara(input, k)
set.seed(20)
km<-kmeans(input, k, nstart = k)
set.seed(20)
pam<-kmeans(input, k)
set.seed(20)
hk<-hkmeans(input, k)
set.seed(20)
hk2<-hkmeans(input, k, hc.method = 'complete')
set.seed(20)
hc<-hclust(dist(input), method = 'ward.D2')%>% cutree(k)
set.seed(20)
hc2<-hclust(dist(input), method = 'complete')%>% cutree(k)




## define final clusters to be used ####

# compclusts<-as.factor(km$cluster)
# compclusts<-as.factor(clara$clustering)
# compclusts<-as.factor(pam$cluster)
# compclusts<-as.factor(hk$cluster)
compclusts<-as.factor(hk2$cluster) #75
# compclusts<-as.factor(hc) 
compclusts<-as.factor(hc2) #73

## name cluster levels ####
  levels(compclusts)<-c('Pivot','Crossing Specialist',
                        'Playmaker','Wide Attacker',
                        'Support','BPD',
                        'Shuttler','Recycler',
                        'Hybrid Scorer','Outlet',
                        'Pure Scorer')
## compare cluster assignments across years ####
curr$cluster<-compclusts[currrows]
prev$cluster<-compclusts[-currrows]
both<-rbind(curr, prev)

commonplayers<-intersect(prev$Player, curr$Player)
prevtest<-subset(prev, Player %in% commonplayers) %>% arrange(Player)
currtest<-subset(curr, Player %in% commonplayers) %>% arrange(Player)

sum(prevtest$cluster == currtest$cluster)/length(prevtest$cluster)
#sum(prevtest$Player == currtest$Player)/length(prevtest$Player)

## confusion matrix ####
caret::confusionMatrix(currtest$cluster, prevtest$cluster)



## plot vars for each group across clusters ####
compnums<-c('Shuttler', 'Support')

comps<-which(both$cluster %in% compnums)

plotclusters(bothscaled[comps,c(1:3)], compclusts[comps])
plotclusters(bothscaled[comps,c(4:6)], compclusts[comps])
plotclusters(bothscaled[comps,c(7:9)], compclusts[comps])
plotclusters(bothscaled[comps,c(10:12)], compclusts[comps])
plotclusters(bothscaled[comps,c(13:15)], compclusts[comps])
plotclusters(bothscaled[comps,c(16:18)], compclusts[comps])
plotclusters(bothscaled[comps,c(18:19)], compclusts[comps])


plotclusters(bothscaled[,c(1:3)], compclusts)
plotclusters(bothscaled[,c(4:6)], compclusts)
plotclusters(bothscaled[,c(7:9)], compclusts)
plotclusters(bothscaled[,c(10:12)], compclusts)
plotclusters(bothscaled[,c(13:15)], compclusts)
plotclusters(bothscaled[,c(16:18)], compclusts)
plotclusters(bothscaled[,c(18:19)], compclusts)


## Position breakdown for each cluster ####
both$Pos<-as.factor(both$Pos)
ftable<- count(both, cluster, Pos, .drop = F) %>%
  group_by(Pos) %>%
  mutate(freq = n / sum(n))

ggplot(ftable, aes(x=cluster, y=Pos, fill=freq)) + geom_tile() +
  scale_fill_gradient(low="lightyellow", high="darkred") + 
  labs(fill='% of\nPosition') +
  ggtitle('Positional Breakdown of Each Role') + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

## Network graph of movement between clusters ####

edgelist<-data.frame(prevrole=prevtest$cluster, currole=currtest$cluster)
# edgelist$prevrole<-paste('2018', edgelist$prevrole)
# edgelist$currole<-paste('2019', edgelist$currole)


netdat<-edgelist %>% as.matrix %>% graph.edgelist %>% as_adjacency_matrix(sparse = F)

linecol<-'slategray2'
boxcol<-'coral3'

bigmove<-netdat
bigmove[bigmove < 3]=0

plotweb(bigmove, bor.col.interaction = 'white', 
        col.interaction = linecol, 
        col.high = boxcol, col.low = boxcol,
        bor.col.high = boxcol, bor.col.low = boxcol,
        text.rot = 30)
title('Major Year-to-Year Movement Between Roles')





## cluster centers ####
centers<-tsne %>% as.data.frame %>% 
  mutate(cluster=compclusts) %>% group_by(cluster) %>%
  summarise_all(median)
findcenters <- tsne %>% as.data.frame
for (clustnum in 1:nrow(centers)){
  colname<-paste0('dist', clustnum)
  clustV1<-centers$V1[clustnum]
  clustV2<-centers$V2[clustnum]
  findcenters[[colname]]<-
    ((findcenters$V1-clustV1)^2) + ((findcenters$V2-clustV2)^2)
}#end for cluster

both[which(findcenters$dist1==min(findcenters$dist1)),c('Player', 'cluster')]
both[which(findcenters$dist2==min(findcenters$dist2)),c('Player', 'cluster')]
both[which(findcenters$dist3==min(findcenters$dist3)),c('Player', 'cluster')]
both[which(findcenters$dist4==min(findcenters$dist4)),c('Player', 'cluster')]
both[which(findcenters$dist5==min(findcenters$dist5)),c('Player', 'cluster')]
both[which(findcenters$dist6==min(findcenters$dist6)),c('Player', 'cluster')]
both[which(findcenters$dist7==min(findcenters$dist7)),c('Player', 'cluster')]
both[which(findcenters$dist8==min(findcenters$dist8)),c('Player', 'cluster')]
both[which(findcenters$dist9==min(findcenters$dist9)),c('Player', 'cluster')]
both[which(findcenters$dist10==min(findcenters$dist10)),c('Player', 'cluster')]

## def radar chart  ####

defvars<-c('targeted', 'passprg', 'xB', 'xPassPerc',
           'shortpasspct', 'longpasspct', 'miscontrol', 'Vertical')
BPDs<-bothscaled [both$cluster=='BPD',] 
Outlets<-bothscaled [both$cluster=='Outlet',]

BPDaggs<-select(BPDs, all_of(defvars)) %>% summarise_all(mean)
Outletaggs<-select(Outlets, all_of(defvars)) %>% summarise_all(mean)
means<-rep(0, length(defvars))
names(means)<-defvars

defRadarDat<- rbind(BPDaggs, Outletaggs, means)
rownames(defRadarDat) <- c("BPD", 'Outlet', 'League Average')

defRadarDat_withscale <- rbind(rep(2,length(defvars)) , 
                     rep(-2, length(defvars)) , 
                     defRadarDat)

col <- c('darksalmon', 'cornflowerblue')
colors_border <- c(col, 'black')
colors_in <- c(alpha(col,0.3), alpha('white', 0))

radarchart(defRadarDat_withscale,
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
           #custom labels
           vlcex=0.8,
           vlabels = c('Times Targeted\nby Pass', 'Progressive\nPass Distance',
                       'xBuildup','xPass%','% Short Passes', '% Long Passes',
                       'Miscontrolled\nPasses', 'Vertical Pass\nDistance'),
           centerzero = F)

## dmf radar chart  ####

mfvars<-c('targeted', 'passprg', 'xB', 'xA',
           'shortpasspct', 'longpasspct', 'xPassPerc', 'Vertical')
Pivots<-bothscaled [both$cluster=='Pivot',] 
Recyclers<-bothscaled [both$cluster=='Recycler',]

Pivotaggs<-select(Pivots, all_of(mfvars)) %>% summarise_all(mean)
Recycleraggs<-select(Recyclers, all_of(mfvars)) %>% summarise_all(mean)

means<-rep(0, length(mfvars))
names(means)<-mfvars

mfRadarDat<- rbind(means, Pivotaggs, Recycleraggs)
rownames(mfRadarDat) <- c('Average',"Pivot", 'Recycler')

mfRadarDat_withscale <- rbind(rep(2,length(mfvars)) , 
                               rep(-2, length(mfvars)) , 
                               mfRadarDat)


col <- c('darksalmon', 'cornflowerblue')
colors_border <- c('black', col)
colors_in <- c(alpha('white', 0), alpha(col,0.3))

radarchart(mfRadarDat_withscale,
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
           #custom labels
           vlcex=0.8,
           vlabels = c('Times Targeted\nby Pass', 'Progressive\nPass Distance',
                       'xBuildup','xA','% Short Passes', '% Long Passes',
                       'xPass%', 'Vertical Pass\nDistance'),
           centerzero = F)
legend(x=0.7, y=1.25, legend = rownames(mfRadarDat_withscale[-c(1,2),]), 
      bty = "n", pch=20 , col=colors_border , text.col = "black", 
      cex=.9, pt.cex=3)


## shuttler radar chart  ####

shuttvars<-c('npxG', 'passprg', 'xB', 'xA',
          'shortpasspct', 'longpasspct', 'xPassPerc', 'Crs')
Shuttlers<-bothscaled [both$cluster=='Shuttler',] 

Shuttleraggs<-select(Shuttlers, all_of(shuttvars)) %>% summarise_all(mean)

means<-rep(0, length(shuttvars))
names(means)<-shuttvars

shuttRadarDat<- rbind(means, Shuttleraggs)
rownames(shuttRadarDat) <- c('Average','Shuttler')

shuttRadarDat_withscale <- rbind(rep(2,length(mfvars)) , 
                              rep(-2, length(mfvars)) , 
                              shuttRadarDat)


col <- c('mediumaquamarine')
colors_border <- c('black', col)
colors_in <- c(alpha('white', 0), alpha(col,0.3))

radarchart(shuttRadarDat_withscale,
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
           #custom labels
           vlcex=0.8,
           vlabels = c('npxG', 'Progressive\nPass Distance',
                       'xBuildup','xA','% Short Passes', '% Long Passes',
                       'xPass%', 'Crosses'),
           centerzero = F)
legend(x=0.7, y=1.25, legend = rownames(shuttRadarDat_withscale[-c(1,2),]), 
       bty = "n", pch=20 , col=colors_border , text.col = "black", 
       cex=.9, pt.cex=3)



## fb radar chart  ####

fbvars<-c('Crs', 'penCrs', 'npxG', 'xA','xPassPerc')
Crossers<-bothscaled [both$cluster=='Crossing Specialist',] 
Supporters<-bothscaled [both$cluster=='Support',]


Crosseraggs<-select(Crossers, all_of(fbvars)) %>% summarise_all(mean)
Supporteraggs<-select(Supporters, all_of(fbvars)) %>% summarise_all(mean)

means<-rep(0, length(fbvars))
names(means)<-fbvars

fbRadarDat<- rbind(means, Crosseraggs, Supporteraggs)
rownames(fbRadarDat) <- c('Average',"Crossing Specialist", 'Support')

fbRadarDat_withscale <- rbind(rep(2,length(fbvars)) , 
                              rep(-2, length(fbvars)) , 
                              fbRadarDat)


col <- c('darksalmon', 'cornflowerblue')
colors_border <- c('black', col)
colors_in <- c(alpha('white', 0), alpha(col,0.3))

radarchart(fbRadarDat_withscale,
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
           #custom labels
           vlcex=0.8,
           vlabels = c('Crosses', 'Crosses into\nPenalty\nArea',
                       'npxG','xA','xPass%'))
legend(x=0.7, y=1.4, legend = c('Average', 'Crossing\nSpecialist', 'Support'), 
       bty = "n", pch=20 , col=colors_border , text.col = "black", 
       cex=.9, pt.cex=3)

## att radar chart  ####

attvars<-c('Crs', 'xA', 'npxG', 'targeted','passprg', 'SuccDrib')
Playmakers<-bothscaled [both$cluster=='Playmaker',] 
Wides<-bothscaled [both$cluster=='Wide Attacker',]
Hybrids<-bothscaled [both$cluster=='Hybrid Scorer',]
Scorers<-bothscaled [both$cluster=='Pure Scorer',]


Playmakeraggs<-select(Playmakers, all_of(attvars)) %>% summarise_all(mean)
Wideaggs<-select(Wides, all_of(attvars)) %>% summarise_all(mean)
Hybridaggs<-select(Hybrids, all_of(attvars)) %>% summarise_all(mean)
Scoreaggs<-select(Scorers, all_of(attvars)) %>% summarise_all(mean)

means<-rep(0, length(attvars))
names(means)<-attvars

attRadarDat<- rbind(means, Playmakeraggs, Wideaggs, Hybridaggs, Scoreaggs)
rownames(attRadarDat) <- c('Average',"Playmaker", 'Wide Attacker','Hybrid Scorer', 'Pure Scorer')

attRadarDat_withscale <- rbind(rep(2,length(attvars)) , 
                              rep(-2, length(attvars)) , 
                              attRadarDat)


col <- c('darksalmon', 'cornflowerblue', 'mediumseagreen', 'mediumpurple')
colors_border <- c('black', col)
colors_in <- c(alpha('white', 0), alpha(col,0.3))

radarchart(attRadarDat_withscale,
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
           #custom labels
           vlcex=0.8)
legend(x=0.7, y=1.4, legend = c('Average', 'Playmaker', 'Wide Attacker', 'Hybrid Scorer', 'Pure Scorer'), 
       bty = "n", pch=20 , col=colors_border , text.col = "black", 
       cex=.8, pt.cex=2)


## def density chart ####
