library(dplyr)
library(ggplot2)
library(factoextra)
source('soccer_util_fxns.R')
## 2019 ####
dat<-read.table('2019summary.txt', header = T)
stats<-c('xGchain', 'xBperc', 'xB', 'xGChain', 'xG', 'xA',
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
tsne<-Rtsne(as.matrix(allscaled), perplexity=15)
plot(tsne$Y)
dtsne<-tsne$Y
comp1<-clValid(dtsne, 
              nClust=2:12, 
              clMethods = c('hierarchical', 'pam','clara', 
                            'diana', 'som','agnes'),
              validation=c('internal'),
              method = 'complete')
summary(comp1)

comp2<-clValid(dtsne, 
               nClust=2:12, 
               clMethods = c('hierarchical', 'pam','clara', 
                             'diana', 'som','agnes'),
               validation=c('internal'),
               method = 'ward')
summary(comp2)

res.clara<-clara(dtsne, 4)
plotclusters(scaled, res.clara$clustering)

#dbscan
library(dbscan)
kNNdistplot(dtsne, 5)
db<-dbscan(dtsne, minPts = 5, eps = 3.5)
plot(db, dtsne)
colnames(dtsne)<-c('one', 'two')

#fanny
res.fanny<-fanny(dtsne, 4)
fviz_cluster(res.fanny)
fviz_silhouette(res.fanny)

#fuzzy kmeans
library(fclust)
res.fkm<-FKM(dtsne, 4, RS=5)
plot(res.fkm,umin=0.5)


comp1<-clValid(pcadecomp, 
               nClust=2:12, 
               clMethods = c('hierarchical', 'pam','clara', 
                             'diana', 'som','agnes'),
               validation=c('internal'),
               method = 'complete')
summary(comp1)

comp2<-clValid(pcadecomp, 
               nClust=2:12, 
               clMethods = c('hierarchical', 'pam','clara', 
                             'diana', 'som','agnes'),
               validation=c('internal'),
               method = 'ward')
summary(comp2)

plotclusters(allscaled[,c('A3perc', 'M3perc', 'D3perc')], res.fkm$clus[,1])

currclusts<-as.factor(res.fkm$clus[,1])
levels(currclusts)<-c('attack', 'defend', 'B2B', 'mf')

current<-dat %>% mutate(cluster=currclusts)


## 2018 ####
dat<-read.table('2018summary.txt', header = T)
stats<-c('xGchain', 'xBperc', 'xB', 'xGChain', 'xG', 'xA',
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

set.seed(500)
tsne<-Rtsne(as.matrix(allscaled), perplexity=12)
plot(tsne$Y)
dtsne<-tsne$Y
comp1<-clValid(dtsne, 
               nClust=2:12, 
               clMethods = c('hierarchical', 'pam','clara', 
                             'diana', 'som','agnes'),
               validation=c('internal'),
               method = 'complete')
summary(comp1)

comp2<-clValid(dtsne, 
               nClust=2:12, 
               clMethods = c('hierarchical', 'pam','clara', 
                             'diana', 'som','agnes'),
               validation=c('internal'),
               method = 'ward')
summary(comp2)

res.clara<-clara(dtsne, 4)
plotclusters(scaled, res.clara$clustering)

#dbscan
library(dbscan)
kNNdistplot(dtsne, 4)
db<-dbscan(dtsne, minPts = 4, eps = 2.5)
plot(db, dtsne)
colnames(dtsne)<-c('one', 'two')

#fanny
res.fanny<-fanny(dtsne, 4)
fviz_cluster(res.fanny)
fviz_silhouette(res.fanny)

#fuzzy kmeans
library(fclust)
res.fkm<-FKM(dtsne, 4, RS=5)
plot(res.fkm,umin=0.5)


plotclusters(allscaled[,c('A3perc', 'M3perc', 'D3perc')], res.fkm$clus[,1])

prevclusts<-as.factor(res.fkm$clus[,1])
levels(prevclusts)<-c('mf', 'B2B', 'attack', 'defend')

prev<-dat %>% mutate(cluster=prevclusts)

## compare cluster assignments across years ####
commonplayers<-intersect(prev$Player, current$Player)

prevtest<-subset(prev, Player %in% commonplayers) %>% arrange(Player)
currtest<-subset(current, Player %in% commonplayers) %>% arrange(Player)
length(prevtest$cluster == currtest$cluster)/length(prevtest$Player)
