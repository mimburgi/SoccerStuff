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


usedvars<-c("shots", "KP", "xG", "xA", 
            "passteamperc", "xGChain", 'ShotChainPerc', "KPChainPerc", 
            "xBperc", "xPassPerc", "PassPct", "A3teamperc", 
            "M3teamperc","D3teamperc", 'Vertical') 

shootvars<-c(1,3,7)
asvars<-c(2,4,8)
areavars<-c(12:14)
indirectvars<-c(6,9)
passvars<-c(10,11, 15)



bothscaled<-select(bothscaledall, usedvars)

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

pcascaled<-caret::preProcess(bothscaled, method='pca', pcaComp=4) %>% predict(bothscaled)
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

set.seed(250)
tsne<-Rtsne::Rtsne(bothscaled, perplexity=28)$Y
plot(tsne)
hopkinsdftsne<-data.frame(clusters=as.numeric(1), stat=as.numeric(NA))
for (i in 2:18){
  hopstat<-get_clust_tendency(tsne, i, graph = F)$hopkins_stat
  hopkinsdftsne[nrow(hopkinsdftsne) + 1,]<-c(i, hopstat)
}
hopkinsdftsne<-hopkinsdftsne[-1,]

ggplot(hopkinsdftsne, aes(x=clusters, y=stat)) + geom_point() + geom_line() +
  ggtitle('hopkins statistic, tsne')
## cluster ####

#6
clara6<-clara(tsne, 6)
set.seed(20)
km6<-kmeans(tsne, 6, nstart = 6)
set.seed(20)
pam6<-kmeans(tsne, 6)
set.seed(20)
hk6<-hkmeans(tsne, 6)
set.seed(20)
hk62<-hkmeans(tsne, 6, hc.method = 'complete')

#5
clara5<-clara(tsne, 5)
set.seed(20)
km5<-kmeans(tsne, 5, nstart = 5)
pam5<-kmeans(tsne, 5)
set.seed(15)
hk5<-hkmeans(tsne, 5)
hk52<-hkmeans(tsne, 5, hc.method = 'complete')

#7
set.seed(20)
clara7<-clara(tsne, 7)
set.seed(20)
km7<-kmeans(tsne, 7, nstart = 7)
set.seed(20)
pam7<-kmeans(tsne, 7)
set.seed(17)
hk7<-hkmeans(tsne, 7)
set.seed(20)
hk72<-hkmeans(tsne, 7, hc.method = 'complete')


# #10
# set.seed(20)
# clara12<-clara(tsne, 12)
# set.seed(20)
# km12<-kmeans(tsne, 12, nstart = 10)
# set.seed(20)
# pam12<-kmeans(tsne, 12)
# set.seed(20)
# hk12<-hkmeans(tsne, 12)
# set.seed(20)
# hk122<-hkmeans(tsne, 12, hc.method = 'complete')


## define final clusters to be used ####
# compclusts<-as.factor(km4$cluster)
# compclusts<-as.factor(clara4$cluster)
# compclusts<-as.factor(pam4$cluster)
# compclusts<-as.factor(hk4$cluster)
# 
# compclusts<-as.factor(km5$cluster)
# compclusts<-as.factor(clara5$cluster)
# compclusts<-as.factor(pam5$cluster)
# compclusts<-as.factor(hk5$cluster)
# 
# compclusts<-as.factor(km6$cluster)
# compclusts<-as.factor(clara6$cluster)
# compclusts<-as.factor(pam6$cluster)
# compclusts<-as.factor(hk6$cluster)
# compclusts<-as.factor(hk62$cluster)
# 
# compclusts<-as.factor(km7$cluster)
# compclusts<-as.factor(clara7$cluster)
# compclusts<-as.factor(pam7$cluster)
# compclusts<-as.factor(hk7$cluster)
# compclusts<-as.factor(hk72$cluster)

# compclusts<-as.factor(km8$cluster)
# compclusts<-as.factor(clara8$cluster)
# compclusts<-as.factor(pam8$cluster)
# compclusts<-as.factor(hk8$cluster)
# compclusts<-as.factor(hk82$cluster)


# compclusts<-as.factor(km10$cluster) #72.6
# compclusts<-as.factor(clara10$cluster) #69.8
# compclusts<-as.factor(pam10$cluster) #70.95
# compclusts<-as.factor(hk10$cluster) #70.95
# compclusts<-as.factor(hk102$cluster) #66


# compclusts<-as.factor(km5$cluster) 
# compclusts<-as.factor(clara5$cluster) #84
# compclusts<-as.factor(pam5$cluster) #79.9
# compclusts<-as.factor(hk5$cluster) #81
# compclusts<-as.factor(hk52$cluster)#79.9

compclusts<-as.factor(km7$cluster) #78.2
# compclusts<-as.factor(clara7$cluster) 
# compclusts<-as.factor(pam7$cluster)
# compclusts<-as.factor(hk7$cluster)
# compclusts<-as.factor(hk72$cluster)#78.2


compclusts<-as.factor(km6$cluster) #84
compclusts<-as.factor(clara6$cluster)
compclusts<-as.factor(pam6$cluster) #83
compclusts<-as.factor(hk6$cluster)
compclusts<-as.factor(hk62$cluster)#83
compclusts<-hclust(dist(tsne), method = 'ward.D2') %>% cutree(6) #85


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
#shootvars, asvars, areavars, 
#passnumvars, indirectvars, passtylevars 
plotclusters(bothscaled[,shootvars], compclusts)
plotclusters(bothscaled[,asvars], compclusts)
plotclusters(bothscaled[,indirectvars], compclusts)
plotclusters(bothscaled[,passvars], compclusts)
plotclusters(bothscaled[,areavars], compclusts)


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
bigmove[bigmove < 2]=0

plotweb(bigmove, bor.col.interaction = 'white', 
        col.interaction = linecol, 
        col.high = boxcol, col.low = boxcol,
        bor.col.high = boxcol, bor.col.low = boxcol,
        text.rot = 30)
title('Major Movement Between Clusters')



## name cluster levels ####
levels(compclusts)<-c('MF Possessor', 'AM Creator', 'Hybrid Attacker',
                      'Possessing Shuttler', 'Defender', 'Direct Shuttler', 'Target')
