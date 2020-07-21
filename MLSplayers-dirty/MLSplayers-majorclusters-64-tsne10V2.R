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
            "M3teamperc","D3teamperc", 'Crs','AttDrib', 
            'Vertical') 

shootvars<-c(1,3,7)
asvars<-c(2,4,8)
areavars<-c(12:14)
indirectvars<-c(6,9)
passsuccessvars<-c(10,11)
passtypevars<-c(15,17)
dribblevars<-c(18)



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
tsne<-Rtsne::Rtsne(bothscaled, perplexity=27)$Y #27 good for 10
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

#10
set.seed(20)
clara10<-clara(tsne, 10)
set.seed(20)
km10<-kmeans(tsne, 10, nstart = 10)
set.seed(20)
pam10<-kmeans(tsne, 10)
set.seed(20)
hk10<-hkmeans(tsne, 10)
set.seed(20)
hk102<-hkmeans(tsne, 10, hc.method = 'complete')
set.seed(20)
hc10<-hclust(dist(tsne), method = 'ward.D2')%>% cutree(10)
set.seed(20)
hc102<-hclust(dist(tsne), method = 'complete')%>% cutree(10)




## define final clusters to be used ####
# compclusts<-as.factor(km6$cluster) #82.7
# compclusts<-as.factor(clara6$cluster) #81.9
# compclusts<-as.factor(pam6$cluster) #81.3
# compclusts<-as.factor(hk6$cluster) #81.3
# compclusts<-as.factor(hk62$cluster) #82.9

compclusts<-as.factor(km10$cluster) 
compclusts<-as.factor(clara10$cluster) #72.5
compclusts<-as.factor(pam10$cluster) #73.1
compclusts<-as.factor(hk10$cluster) #73.1
compclusts<-as.factor(hk102$cluster) 
compclusts<-as.factor(hc10) 
compclusts<-as.factor(hc102) #73.6


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
plotclusters(bothscaled[,passtypevars], compclusts)
plotclusters(bothscaled[,passsuccessvars], compclusts)
plotclusters(bothscaled[,areavars], compclusts)
plotclusters(bothscaled[,16], compclusts)


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



## name cluster levels ####
levels(compclusts)<-c('MF Possessor', 'AM Creator', 'Hybrid Attacker',
                      'Possessing Shuttler', 'Defender', 'Direct Shuttler', 'Target')
