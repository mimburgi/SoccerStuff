library(dplyr)
library(ggplot2)
library(factoextra)
library(Rtsne)
library(caret)
source('soccer_util_fxns.R')
current<-read.table('2019summary-majorclass.txt', header = T)
prev<-read.table('2018summary-majorclass.txt', header = T)





## attack ####
currscaled<-current %>% select_if(is.numeric) %>% scale %>% as.data.frame()
curratt<-currscaled[current$cluster=='attack',] %>% select(c(-TouchPerc, -xPlace))


prevscaled<-prev %>% select_if(is.numeric) %>% scale %>% as.data.frame()
prevatt<-prevscaled[prev$cluster=='attack',] %>% select(-xPlace)

CApca<-PCA(curratt, scale.unit = F)
PApca<-PCA(prevatt, scale.unit = F)

fviz_screeplot(PApca)

fviz_contrib(CApca, choice = 'var', axes = 1)
fviz_contrib(PApca, choice = 'var', axes = 1)

fviz_contrib(CApca, choice = 'var', axes = 2)
fviz_contrib(PApca, choice = 'var', axes = 2)

allA<-rbind(curratt, prevatt)
get_clust_tendency(allA, 3) # sweet spot

set.seed(12)
Akm<-kmeans(allA, 3, nstart = 10)
fviz_cluster(Akm, allA)

attvats<-c('xG', 'xA', 'Passes', 'xB', 'percChain', 'xGChain',
           'ShotChainPerc', 'shots')
allA2<-allA %>% select(one_of(attvats))
get_clust_tendency(allA2, 3) # sweet spot


set.seed(201)
Atsne<-Rtsne(allA2, perplexity=16)
plot(Atsne$Y)

Afkm<-FKM(allA2, 2)
plotclusters(allA2, Afkm$clus[,1])

attvats2<-c('Passes', 'xB', 'percChain')
allA3<-allA %>% select(one_of(attvats2))
get_clust_tendency(allA3, 2) # sweet spot


set.seed(20)
Atsne<-Rtsne(allA3, perplexity=10)
plot(Atsne$Y)

set.seed(10)
Akm3<-kmeans(allA3, 2, nstart = 10)
fviz_cluster(Akm3, allA3)

Aclara3<-clara(allA3, 2)
fviz_cluster(Aclara3, allA3)

plotclusters(allA3, Aclara3$cluster)

Afkm<-FKM(allA3, 2)
plot(density(Afkm$U[2,]))

comp<-clValid(allA3, nClust = 2, clMethods=c('hierarchical', 'diana', 'kmeans',
                                             'clara', 'som'),
              validation = c('internal', 'stability'))

Adiana<-diana(allA3)
Adianacut<-cutree(Adiana, 2)
fviz_dend(Adiana, k=2)

plotclusters(allA3, Adianacut)

curratt2<-select(curratt, one_of(attvats2))
prevatt2<-select(prevatt, one_of(attvats2))

get_clust_tendency(curratt2, 2)
get_clust_tendency(prevatt2, 2)

CAdiana<-diana(curratt2)
CAdianacut<-cutree(CAdiana, 2)
plotclusters(curratt2, CAdianacut)

currlabsA<-CAdianacut

PAdiana<-diana(prevatt2)
PAdianacut<-cutree(PAdiana, 2)
plotclusters(prevatt2, PAdianacut)

prevlabsA<-PAdianacut

## comparisons across years ####
commonplayers<-intersect(current$Player, prev$Player)
cattTest<-subset(current, cluster=='attack') %>% mutate(lab=currlabsA) %>%
  subset(Player %in% commonplayers) %>% arrange(Player)
pattTest<-subset(prev, cluster=='attack') %>% mutate(lab=prevlabsA) %>%
  subset(Player %in% commonplayers) %>% arrange(Player)
cattTest$Player=as.character(cattTest$Player)
pattTest$Player=as.character(pattTest$Player)
sum(cattTest$Player==pattTest$Player)/length(cattTest$Player)

currattplayers<-current$Player[current$cluster=='attack']
prevattplayers<-prev$Player[prev$cluster=='attack']

commonCA<-currattplayers[currattplayers %in% commonplayers]
commonPA<-prevattplayers[prevattplayers %in% commonplayers]


