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
            'Crs','penCrs','Vertical', 
            'SuccDrib', 'miscontrol', 'targeted',
            'shortpasspct', 'longpasspct') 




bothscaled<-select(bothscaledall, all_of(usedvars))

bothscaled[bothscaled > 3] <- 3
bothscaled[bothscaled < -3] <- -3

#make a graph of hopkins stats
hopkinsdf<-data.frame(clusters=as.numeric(1), stat=as.numeric(NA))
for (i in 3:15){
  hopstat<-get_clust_tendency(bothscaled, i, graph = F)$hopkins_stat
  hopkinsdf[nrow(hopkinsdf) + 1,]<-c(i, hopstat)
}
hopkinsdf<-hopkinsdf[-1,]

ggplot(hopkinsdf, aes(x=clusters, y=stat)) + geom_point() + geom_line() +
  ggtitle('hopkins statistic, scaled')


#try pca
#decreases the hopkins stat across the board
fviz_screeplot(PCA(bothscaled, graph = F))

pcascaled<-caret::preProcess(bothscaled, method='pca', pcaComp=5) %>% predict(bothscaled)
hopkinsdfpca<-data.frame(clusters=as.numeric(1), stat=as.numeric(NA))
for (i in 3:15){
  hopstat<-get_clust_tendency(pcascaled, i, graph = F)$hopkins_stat
  hopkinsdfpca[nrow(hopkinsdfpca) + 1,]<-c(i, hopstat)
}
hopkinsdfpca<-hopkinsdfpca[-1,]

ggplot(hopkinsdfpca, aes(x=clusters, y=stat)) + geom_point() + geom_line() +
  ggtitle('hopkins statistic, pca')


#try tsne
#huge increase

set.seed(20)
tsne<-Rtsne::Rtsne(bothscaled, perplexity=20)$Y #20
plot(tsne)
hopkinsdftsne<-data.frame(clusters=as.numeric(1), stat=as.numeric(NA))
for (i in 3:15){
  hopstat<-get_clust_tendency(tsne, i, graph = F)$hopkins_stat
  hopkinsdftsne[nrow(hopkinsdftsne) + 1,]<-c(i, hopstat)
}
hopkinsdftsne<-hopkinsdftsne[-1,]

ggplot(hopkinsdftsne, aes(x=clusters, y=stat)) + geom_point() + geom_line() +
  ggtitle('hopkins statistic, tsne')

allhopkins<-rbind(hopkinsdf %>% mutate(method='None'),
                  hopkinsdfpca %>% mutate(method='PCA'),
                  hopkinsdftsne %>% mutate(method = 'tSNE'))


dev.off()

tiff("reduction.tiff", units="in", width=5, height=3, res=300)
ggplot(allhopkins, aes(x=clusters, y=stat, color=method)) + geom_point() + geom_line() +
  ggtitle('Effect of Dimension Reduction on Clusterability') + 
  labs(x='\nNumber of Clusters',
       y='Hopkins Statistic (larger is better)\n',
       color='Method') + 
  scale_color_manual(values=c('mediumseagreen', 
                              'midnightblue', 
                              'coral3')) + 
  theme(legend.position = c(0.5, 0.1), legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill="transparent", colour=NA),
        legend.key        = element_rect(fill="transparent", colour=NA),
        panel.background = element_rect(fill = "lightyellow", colour = NA), 
        panel.border = element_rect(fill = NA, colour = "grey50"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black")) 
dev.off()  
  
  
  

## set pars ####
k=11
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
compclusts<-as.factor(hk2$cluster) #74
# compclusts<-as.factor(hc)
# compclusts<-as.factor(hc2)

## name cluster levels ####
levels(compclusts)<-c('Pivot','Crossing Specialist',
                        'Playmaker','Wide Attacker',
                        'Wide Support','Ball-Playing Def.',
                        'Shuttler','Recycler',
                        'Hybrid Scorer','Backfield Outlet',
                        'Pure Scorer')

#reorder to go from back to front to make heatmap look nicer
compclusts<-factor(compclusts, levels=c('Ball-Playing Def.','Backfield Outlet', 'Crossing Specialist',
                                        'Wide Support', 'Pivot', 'Recycler',
                                        'Shuttler',
                                        'Playmaker', 'Wide Attacker',
                                        'Hybrid Scorer', 'Pure Scorer'))
## compare cluster assignments across years ####
curr$cluster<-compclusts[currrows]
prev$cluster<-compclusts[-currrows]
both<-rbind(curr, prev)

commonplayers<-intersect(prev$Player, curr$Player)
prevtest<-subset(prev, Player %in% commonplayers) %>% arrange(Player)
currtest<-subset(curr, Player %in% commonplayers) %>% arrange(Player)

sum(prevtest$cluster == currtest$cluster)/length(prevtest$cluster)
#sum(prevtest$Player == currtest$Player)/length(prevtest$Player)

# ## confusion matrix ####
# caret::confusionMatrix(currtest$cluster, prevtest$cluster)
# 
# 
# 
# ## plot vars for each group across clusters ####
compnums<-c('Shuttler', 'Playmaker', 'Pivot')

comps<-which(both$cluster %in% compnums)

plotclusters(bothscaled[comps,c(1:3)], compclusts[comps])
plotclusters(bothscaled[comps,c(4:6)], compclusts[comps])
plotclusters(bothscaled[comps,c(7:9)], compclusts[comps])
plotclusters(bothscaled[comps,c(10:12)], compclusts[comps])
plotclusters(bothscaled[comps,c(13:15)], compclusts[comps])
plotclusters(bothscaled[comps,c(16:18)], compclusts[comps])
plotclusters(bothscaled[comps,c(18:19)], compclusts[comps])
# 
# 
# plotclusters(bothscaled[,c(1:3)], compclusts)
# plotclusters(bothscaled[,c(4:6)], compclusts)
# plotclusters(bothscaled[,c(7:9)], compclusts)
# plotclusters(bothscaled[,c(10:12)], compclusts)
# plotclusters(bothscaled[,c(13:15)], compclusts)
# plotclusters(bothscaled[,c(16:18)], compclusts)
# plotclusters(bothscaled[,c(18:19)], compclusts)
# 

## Position breakdown for each cluster ####
both$Pos<-as.factor(both$Pos)
ftable<- count(both, cluster, Pos, .drop = F) %>%
  group_by(Pos) %>%
  mutate(freq = n / sum(n))

dev.off()
tiff("heatmap.tiff", units="in", width=5, height=3, res=300)

ggplot(ftable, aes(x=cluster, y=Pos, fill=freq)) + geom_tile() +
  scale_fill_gradient(low="lightyellow", high="darkred") + 
  labs(fill='% of\nPosition') +
  ggtitle('Breakdown of Positions by Attacking Role') + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(hjust = 0.5)) + 
  labs(y='Position', x='Attacking Role')
dev.off()
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

defvars<-c('targeted', 'passprg', 'xB', 'xA','xPassPerc',
           'shortpasspct', 'longpasspct', 'miscontrol', 'Vertical')
BPDs<-bothscaled [both$cluster=='Ball-Playing Def.',] 
Outlets<-bothscaled [both$cluster=='Backfield Outlet',]

BPDaggs<-select(BPDs, all_of(defvars)) %>% summarise_all(median)
Outletaggs<-select(Outlets, all_of(defvars)) %>% summarise_all(median)
means<-rep(0, length(defvars))
names(means)<-defvars

defRadarDat<- rbind(means, BPDaggs, Outletaggs)
rownames(defRadarDat) <- c("Average", "BPD", 'Outlet')

defRadarDat_withscale <- rbind(rep(2,length(defvars)) , 
                     rep(-2, length(defvars)) , 
                     defRadarDat)

col <- c('#3f2199', '#b41e51')
colors_border <- c('black', col)
colors_in <- c(alpha('white', 0), alpha(col,0.3))


dev.off()
tiff("defradar.tiff", units="in", width=6, height=6, res=300)

radarchart(defRadarDat_withscale,
           pty=32,
           pcol=colors_border , pfcol=colors_in , plwd=c(2,4,4) , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=1.5, 
           #vlcex=0,
           vlabels = rep("", ncol(defRadarDat_withscale)),
           centerzero = T)


dev.off()

## dmf radar chart  ####

mfvars<-c('targeted', 'passprg', 'xB', 'xA','xPassPerc',
          'shortpasspct', 'longpasspct', 'miscontrol', 'Vertical')
Pivots<-bothscaled [both$cluster=='Pivot',] 
Recyclers<-bothscaled [both$cluster=='Recycler',]

Pivotaggs<-select(Pivots, all_of(mfvars)) %>% summarise_all(median)
Recycleraggs<-select(Recyclers, all_of(mfvars)) %>% summarise_all(median)

means<-rep(0, length(mfvars))
names(means)<-mfvars

mfRadarDat<- rbind(means, Pivotaggs, Recycleraggs)
rownames(mfRadarDat) <- c('Average',"Pivot", 'Recycler')

mfRadarDat_withscale <- rbind(rep(2,length(mfvars)) , 
                               rep(-2, length(mfvars)) , 
                               mfRadarDat)


col <- c('#088490', '#b84529')
colors_border <- c('black', col)
colors_in <- c(alpha('white', 0), alpha(col,0.3))

dev.off()
tiff("mfradar.tiff", units="in", width=6, height=6, res=300)

radarchart(mfRadarDat_withscale,
           pty = 32,
           pcol=colors_border , pfcol=colors_in , plwd=c(2,4,4) , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=1.5,  
           vlabels = rep("", ncol(defRadarDat_withscale)),
           centerzero = T)

dev.off()

## fb radar chart  ####

fbvars<-c('targeted','npxG', 'xA', 'xB', 'Crs',
          'xPassPerc', 'longpasspct', 'miscontrol', 'Vertical')
Crossers<-bothscaled [both$cluster=='Crossing Specialist',] 
Supporters<-bothscaled [both$cluster=='Wide Support',]


Crosseraggs<-select(Crossers, all_of(fbvars)) %>% summarise_all(mean)
Supporteraggs<-select(Supporters, all_of(fbvars)) %>% summarise_all(mean)

means<-rep(0, length(fbvars))
names(means)<-fbvars

fbRadarDat<- rbind(means, Crosseraggs, Supporteraggs)
rownames(fbRadarDat) <- c('Average',"Crossing Specialist", 'Support')


fbRadarDat_withscale <- rbind(rep(2,length(fbvars)) , 
                              rep(-2, length(fbvars)) , 
                              fbRadarDat)


col <- c('#588065', '#957206')
colors_border <- c('black', col)
colors_in <- c(alpha('white', 0), alpha(col,0.3))

dev.off()
tiff("fbradar.tiff", units="in", width=6, height=6, res=300)

radarchart(fbRadarDat_withscale,
           pty = 32,
           pcol=colors_border , pfcol=colors_in , plwd=c(2,4,4) , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=1.5,  
           vlabels = rep("", ncol(fbRadarDat_withscale)),
           centerzero = T)

dev.off()

## att radar charts  ####

attvars<-c('targeted','npxG', 'xA', 'Crs', 'SuccDrib',
           'xPassPerc', 'shortpasspct', 'miscontrol', 'Vertical')
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

attRadar1Dat<- rbind(means, Hybridaggs, Scoreaggs)
rownames(attRadar1Dat) <- c('Average','Hybrid Scorer', 'Pure Scorer')

attRadar1Dat_withscale <- rbind(rep(2,length(attvars)) , 
                              rep(-2, length(attvars)) , 
                              attRadar1Dat)


col <- c('#895ae7' ,'#af5b89')
colors_border <- c('black', col)
colors_in <- c(alpha('white', 0), alpha(col,0.3))

dev.off()
tiff("attradar1.tiff", units="in", width=6, height=6, res=300)

radarchart(attRadar1Dat_withscale,
           pty = 32,
           pcol=colors_border , pfcol=colors_in , plwd=c(2,4,4,4) , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=1.5,  
           vlabels = rep("", ncol(attRadar1Dat_withscale)),
           centerzero = T)

dev.off()

attRadar2Dat<- rbind(means, Playmakeraggs, Wideaggs)
rownames(attRadar2Dat) <- c('Average','Playmaker', 'Wide Attacker')

attRadar2Dat_withscale <- rbind(rep(2,length(attvars)) , 
                                rep(-2, length(attvars)) , 
                                attRadar2Dat)

col <- c('#d63092' ,'#7173a1')
colors_border <- c('black', col)
colors_in <- c(alpha('white', 0), alpha(col,0.3))

dev.off()
tiff("attradar2.tiff", units="in", width=6, height=6, res=300)

radarchart(attRadar2Dat_withscale,
           pty = 32,
           pcol=colors_border , pfcol=colors_in , plwd=c(2,4,4,4) , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=1.5,  
           vlabels = rep("", ncol(attRadar1Dat_withscale)),
           centerzero = T)

dev.off()


## shuttler radar chart ####
shuttvars<-c('targeted','xB','passprg','npxG', 'xA', 'Crs', 'xPassPerc',
            'shortpasspct', 'miscontrol')
Shuttlers<-bothscaled [both$cluster=='Shuttler',] 

Pivotaggs2<-select(Pivots, all_of(shuttvars)) %>% summarise_all(mean)
Shuttleraggs<-select(Shuttlers, all_of(shuttvars)) %>% summarise_all(mean)
Playmakeraggs2<-select(Playmakers, all_of(shuttvars)) %>% summarise_all(mean)


means<-rep(0, length(shuttvars))
names(means)<-shuttvars

shuttRadarDat<- rbind(means, Pivotaggs2, Playmakeraggs2, Shuttleraggs)
rownames(shuttRadarDat) <- c('Average',"Pivot", 'Playmaker', 'Shuttler')

shuttRadarDat_withscale <- rbind(rep(2,length(shuttvars)) , 
                              rep(-2, length(shuttvars)) , 
                              shuttRadarDat)


col <- c('#088490', '#d63092', '#368741')
colors_border <- c('black', col)
colors_in <- c(alpha('white', 0), alpha(col,0.3))

dev.off()
tiff("shuttradar.tiff", units="in", width=6, height=6, res=300)

radarchart(shuttRadarDat_withscale,
           pty = 32,
           pcol=colors_border , pfcol=colors_in , plwd=c(2,4,4,4) , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=1.5,  
           vlabels = rep("", ncol(shuttRadarDat_withscale)),
           centerzero = T)

dev.off()

