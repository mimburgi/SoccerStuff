library(dplyr)
library(ggplot2)
library(factoextra)
library(Rtsne)
library(clValid)
library(dbscan)
source('soccer_util_fxns.R')
## across both years ####
curr<-read.table('2019summary.txt', header = T)
currscaled<-curr %>% select_if(is.numeric) %>% scale() %>% as.data.frame()
currscaled<-select(currscaled, -c(xGteamperc, shotteamperc, A3Passes, M3Passes, D3Passes)) %>%
  select(-c(teamA3pass, teamM3pass, teamD3pass, teampass, teamxG, teamshots, xPlace, ShotDist, KPDist,
            xGper, xAper))
currscaled[currscaled < -3]<- -3

prev<-read.table('2018summary.txt', header = T)
prevscaled<-prev %>% select_if(is.numeric) %>% scale() %>% as.data.frame()
prevscaled<-select(prevscaled, -c(xGteamperc, shotteamperc, A3Passes, M3Passes, D3Passes)) %>%
  select(-c(teamA3pass, teamM3pass, teamD3pass, teampass, teamxG, teamshots, xPlace, ShotDist, KPDist,
            xGper, xAper))

#to separate out later
currrows<-c(1:nrow(currscaled))

bothscaled<-rbind(currscaled, prevscaled)

get_clust_tendency(bothscaled, 3)

set.seed(30)
tsne<-Rtsne(as.matrix(bothscaled), perplexity=23)
plot(tsne$Y)
Bdtsne<-tsne$Y

kNNdistplot(Bdtsne, k=10)
db<-dbscan(Bdtsne, minPts = 10, eps = 3.5)
hullplot(Bdtsne, db)

plotclusters(bothscaled[13:23], db$cluster)

remove<-c('percChain', 'xB', 'Passes', 'M3perc',
          'M3teamperc', 'passteamperc', 'xPassPerc',
          'xBperc', 'PassScore', 'xGChain', 'Vertical', 
          'PassDistance', 'A3teamperc')

bothtrimmed<-select(bothscaled, -remove)

bothtrimmed[bothtrimmed > 3]<- 3


get_clust_tendency(bothtrimmed, 3)

set.seed(215)
tsne<-Rtsne(as.matrix(bothtrimmed), perplexity=14)
plot(tsne$Y)
Bdtsne<-tsne$Y

clara<-clara(as.data.frame(Bdtsne), 5)
fviz_cluster(clara, as.data.frame(Bdtsne))



kNNdistplot(Bdtsne, k=5)
db<-dbscan(Bdtsne, minPts = 4, eps = 2)
hullplot(Bdtsne, db)

plotclusters(bothtrimmed[13:17], db$cluster)

res.fanny<-fanny(as.data.frame(Bdtsne), 3)
fviz_cluster(res.fanny)

plotclusters(bothtrimmed, res.fanny$clustering)

plot(res.fanny)

## 2019 ####


set.seed(20)
tsne<-Rtsne(as.matrix(currscaled), perplexity=9)
plot(tsne$Y)
Cdtsne<-tsne$Y

get_clust_tendency(Cdtsne, 6)

Ccomp<-clValid(Cdtsne, nClust = 2:6, validation = c('internal', 'stability'), method = 'ward',
        clMethods = c('hierarchical', 'diana', 'som', 'clara'))
summary(Ccomp)

Chc<-hclust(dist(Cdtsne), method = 'ward')
Chccut<-cutree(Chc, 4)
fviz_dend(Chc, k=4)

set.seed(0)
Cclara<-clara(as.data.frame(Cdtsne), 4)
fviz_cluster(Cclara)

plotclusters(currscaled[,c('A3perc', 'A3teamperc', 'M3perc', 
                           'M3teamperc', 'D3perc', 'D3teamperc')], 
             Cclara$clustering)


currclusts<-as.factor(Cclara$clustering)
levels(currclusts)<-c('playmaker', 'B2B', 'mf', 'def', 'finisher')

curr<-curr %>% mutate(cluster=currclusts)



## 2018 ####

set.seed(50)
Ptsne<-Rtsne(as.matrix(prevscaled), perplexity=9)
plot(Ptsne$Y)
Pdtsne<-Ptsne$Y

get_clust_tendency(Pdtsne, 6)

Pcomp<-clValid(Pdtsne, nClust = 2:6, validation = c('internal', 'stability'), method = 'ward',
               clMethods = c('hierarchical', 'diana', 'som', 'clara'))
summary(Pcomp)


set.seed(0)
Pclara<-clara(as.data.frame(Pdtsne), 4)
fviz_cluster(Pclara)


plotclusters(prevscaled[,c('A3perc', 'A3teamperc', 'M3perc', 'M3teamperc', 'D3perc', 'D3teamperc')], Pclara$clustering)

prevclusts<-as.factor(Pclara$clustering)
levels(prevclusts)<-c('mf', 'B2B', 'attack', 'def',)

prev<-prev %>% mutate(cluster=prevclusts)



## compare cluster assignments across years ####
commonplayers<-intersect(prev$Player, curr$Player)

prevtest<-subset(prev, Player %in% commonplayers) %>% arrange(Player)
currtest<-subset(curr, Player %in% commonplayers) %>% arrange(Player)

prevtest$Player<-droplevels(prevtest$Player)
currtest$Player<-droplevels(currtest$Player)

#sum(prevtest$Player == currtest$Player)/length(prevtest$Player)
sum(prevtest$cluster == currtest$cluster)/length(prevtest$cluster)

caret::confusionMatrix(prevtest$cluster, currtest$cluster)


curr<-select(curr, -role)
write.table(curr, '2019summary-majorclass.txt', row.names = F)
write.table(prev, '2018summary-majorclass.txt', row.names = F)
