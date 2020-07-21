library(dplyr)
library(ggplot2)
library(factoextra)
source('soccer_util_fxns.R')
## 2019 ####
dat<-read.table('2019summary.txt', header = T)
allscaled<-dat %>% select_if(is.numeric) %>% scale() %>% as.data.frame()

set.seed(20)
tsne<-Rtsne(as.matrix(allscaled), perplexity=15)
plot(tsne$Y)
dtsne<-tsne$Y

library(fclust)
set.seed(30)
res.fkm<-FKM(dtsne, 4, RS=5)
plot(res.fkm,umin=0.5)

plotclusters(allscaled[,c('A3perc', 'M3perc', 'D3perc')], res.fkm$clus[,1])

currclusts<-as.factor(res.fkm$clus[,1])
levels(currclusts)<-c('B2B', 'mf', 'attack', 'def')

current<-dat %>% mutate(cluster=currclusts)


## 2018 ####
dat<-read.table('2018summary.txt', header = T)
stats<-c('xGchain', 'xBperc', 'xB', 'xGChain', 'xG', 'xA',
         'PassScore', 'xPassPct', 'percChain')
allscaled<-dat %>% select_if(is.numeric) %>% scale() %>% as.data.frame()

set.seed(500)
tsne<-Rtsne(as.matrix(allscaled), perplexity=12)
plot(tsne$Y)
dtsne<-tsne$Y

#fuzzy kmeans
library(fclust)
set.seed(30)
res.fkm<-FKM(dtsne, 4, RS=5)
plot(res.fkm,umin=0.5)


plotclusters(allscaled[,c('A3perc', 'M3perc', 'D3perc')], res.fkm$clus[,1])

prevclusts<-as.factor(res.fkm$clus[,1])
levels(prevclusts)<-c('def', 'attack', 'B2B', 'mf')

prev<-dat %>% mutate(cluster=prevclusts)

## compare cluster assignments across years ####
commonplayers<-intersect(prev$Player, current$Player)

prevtest<-subset(prev, Player %in% commonplayers) %>% arrange(Player)
currtest<-subset(current, Player %in% commonplayers) %>% arrange(Player)

prevtest$Player<-droplevels(prevtest$Player)
currtest$Player<-droplevels(currtest$Player)

sum(prevtest$cluster == currtest$cluster)/length(prevtest$Player)


current<-select(current, -role)
write.table(current, '2019summary-majorclass.txt', row.names = F)
write.table(prev, '2018summary-majorclass.txt', row.names = F)
