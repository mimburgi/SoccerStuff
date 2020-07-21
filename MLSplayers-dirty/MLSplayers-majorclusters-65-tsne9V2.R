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


usedvars<-c("shots", "KP", "npxG", 
            "xGChain", 'ShotChainPerc', "KPChainPerc", 
            "xBperc", "xPassPerc", "PassPct", "A3teamperc", 
            "M3teamperc","D3teamperc", 'Crs','Vertical', 
            'SuccDrib') 




bothscaled<-select(bothscaledall, usedvars)

bothscaled[bothscaled > 3] <- 3
bothscaled[bothscaled < -3] <- -3
# 
# #make a graph of hopkins stats
# hopkinsdf<-data.frame(clusters=as.numeric(1), stat=as.numeric(NA))
# for (i in 2:18){
#   hopstat<-get_clust_tendency(bothscaled, i, graph = F)$hopkins_stat
#   hopkinsdf[nrow(hopkinsdf) + 1,]<-c(i, hopstat)
# }
# hopkinsdf<-hopkinsdf[-1,]
# 
# ggplot(hopkinsdf, aes(x=clusters, y=stat)) + geom_point() + geom_line() +
#   ggtitle('hopkins statistic, scaled')
# 
# 
# #try pca
# #decreases the hopkins stat across the board
# fviz_screeplot(PCA(bothscaled, graph = F))
# 
# pcascaled<-caret::preProcess(bothscaled, method='pca', pcaComp=4) %>% predict(bothscaled)
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

set.seed(25)
tsne<-Rtsne::Rtsne(bothscaled, perplexity=18)$Y 
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
k=5
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

compclusts<-as.factor(km$cluster)
compclusts<-as.factor(clara$clustering) #72
# compclusts<-as.factor(pam$cluster)
# compclusts<-as.factor(hk$cluster)
# compclusts<-as.factor(hk2$cluster)
# compclusts<-as.factor(hc)
# compclusts<-as.factor(hc2) #73, but yields one cluster that doesn't really exist in one year


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

plotclusters(bothscaled[,c(1:4)], compclusts)
plotclusters(bothscaled[,c(5:8)], compclusts)
plotclusters(bothscaled[,c(9:11)], compclusts)
plotclusters(bothscaled[,c(12:15)], compclusts)


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
levels(compclusts)<-c('MF Orchestrator', 'MF Ball Protector', 'B2B & Cross',
                      'Attacking Hub','B2B Buildup', 'Defender',
                      'Hybrid Attacker', 'Finisher', 'Creator')

## cluster centers ####

