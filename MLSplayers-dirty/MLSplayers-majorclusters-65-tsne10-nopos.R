## package requirements ####

library(dplyr)
library(ggplot2)

#network graph packages
library(network)
library(igraph)
library(bipartite)


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
for (i in 2:18){
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
for (i in 2:18){
  hopstat<-get_clust_tendency(pcascaled, i, graph = F)$hopkins_stat
  hopkinsdfpca[nrow(hopkinsdfpca) + 1,]<-c(i, hopstat)
}
hopkinsdfpca<-hopkinsdfpca[-1,]

ggplot(hopkinsdfpca, aes(x=clusters, y=stat)) + geom_point() + geom_line() +
  ggtitle('hopkins statistic, pca')


#try tsne
#huge increase

set.seed(20)
tsne<-Rtsne::Rtsne(bothscaled, perplexity=20)$Y #17
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

# compclusts<-as.factor(km$cluster) #75.8
# compclusts<-as.factor(clara$clustering) 
# compclusts<-as.factor(pam$cluster)
# compclusts<-as.factor(hk$cluster) 
# compclusts<-as.factor(hk2$cluster) 
compclusts<-as.factor(hc) #75.8
# compclusts<-as.factor(hc2)

## name cluster levels ####
levels(compclusts)<-c('Ball Progressor','attacking FB',
                      'Playmaker','Shuttler',
                      'BPD','Support Attacker',
                      'Recycler','Hybrid Attacker',
                      'Outlet','Finisher')
## compare cluster assignments across years ####
curr$cluster<-compclusts[currrows]
prev$cluster<-compclusts[-currrows]
commonplayers<-intersect(prev$Player, curr$Player)
prevtest<-subset(prev, Player %in% commonplayers) %>% arrange(Player)
currtest<-subset(curr, Player %in% commonplayers) %>% arrange(Player)

sum(prevtest$cluster == currtest$cluster)/length(prevtest$cluster)
#sum(prevtest$Player == currtest$Player)/length(prevtest$Player)

## confusion matrix ####
caret::confusionMatrix(currtest$cluster, prevtest$cluster)



## plot vars for each group across clusters ####

plotclusters(bothscaled[,c(1:3)], compclusts)
plotclusters(bothscaled[,c(4:6)], compclusts)
plotclusters(bothscaled[,c(7:9)], compclusts)
plotclusters(bothscaled[,c(10:12)], compclusts)
plotclusters(bothscaled[,c(13:15)], compclusts)
plotclusters(bothscaled[,c(16:18)], compclusts)
plotclusters(bothscaled[,c(18:19)], compclusts)


## Position breakdown for each cluster ####
both<-rbind(curr, prev)
both$Pos<-as.factor(both$Pos)
ftable<- count(both, cluster, Pos, .drop = F) %>%
  group_by(Pos) %>%
  mutate(freq = n / sum(n))

ggplot(ftable, aes(x=cluster, y=Pos, fill=freq)) + geom_tile() +
  scale_fill_gradient(low="lightyellow", high="darkred")

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
title('Major Movement Between Clusters')





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
