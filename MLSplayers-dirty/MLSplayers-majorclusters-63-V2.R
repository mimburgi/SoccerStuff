library(dplyr)
library(ggplot2)

#network graph packages
library(ggnet) #devtools::install_github("briatte/ggnet")
library(network) #to make input to ggnet
library(igraph)
library(ggbipart) #need to install from git
library(ggnet)

#cluster packages
library(factoextra)
library(cluster)

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


usedvars<-c("shots", "KP", "xG", "xA", "percChain", "xGChain", "xB", 'ShotChainPerc',
            "KPChainPerc", "xBperc", "Vertical", "PassPct", 
            "PassDistance", "xPassPerc", "Passes", "PassScore",
            "A3perc", "M3perc", "D3perc", "A3teamperc", "M3teamperc","D3teamperc") 

shootvars<-c(1,3,8)
asvars<-c(2,4,9)
areavars<-c(17:22)
passnumvars<-c(5, 15)
indirectvars<-c(6,7,10)
passstylevars<-c(11, 12, 13, 14, 16)



bothscaled<-select(bothscaledall, usedvars)

#make a graph of hopkins stats
hopkinsdf<-data.frame(clusters=as.numeric(1), stat=as.numeric(NA))
for (i in 2:18){
  hopstat<-get_clust_tendency(bothscaled, i, graph = F)$hopkins_stat
  hopkinsdf[nrow(hopkinsdf) + 1,]<-c(i, hopstat)
}
hopkinsdf<-hopkinsdf[-1,]

ggplot(hopkinsdf, aes(x=clusters, y=stat)) + geom_point() + geom_line()



test<-get_clust_tendency(bothscaled, 4, graph = F)
test$hopkins_stat


## cluster ####
#4
clara4<-clara(bothscaled, 4)
km4<-kmeans(bothscaled, 4)
pam4<-kmeans(bothscaled, 4)
hk4<-hkmeans(bothscaled, 4)


#5
clara5<-clara(bothscaled, 5)
km5<-kmeans(bothscaled, 5)
pam5<-kmeans(bothscaled, 5)
hk5<-hkmeans(bothscaled, 5)

#6
clara6<-clara(bothscaled, 6)
km6<-kmeans(bothscaled, 6)
pam6<-kmeans(bothscaled, 6)
set.seed(0)
hk6<-hkmeans(bothscaled, 6)
hk62<-hkmeans(bothscaled, 6, hc.method = 'complete')

#7
clara7<-clara(bothscaled, 7)
km7<-kmeans(bothscaled, 7)
pam7<-kmeans(bothscaled, 7)
hk7<-hkmeans(bothscaled, 7)
hk72<-hkmeans(bothscaled, 7, hc.method = 'complete')

#8
clara8<-clara(bothscaled, 8)
km8<-kmeans(bothscaled, 8)
pam8<-kmeans(bothscaled, 8)
hk8<-hkmeans(bothscaled, 8)
hk82<-hkmeans(bothscaled, 8, hc.method = 'complete')

#11
clara11<-clara(bothscaled, 11)
km11<-kmeans(bothscaled, 11)
pam11<-kmeans(bothscaled, 11)
hk11<-hkmeans(bothscaled, 11)
hk112<-hkmeans(bothscaled, 11, hc.method = 'complete')

# 
# fviz_screeplot(PCA(bothscaled, scale.unit = F))
# decomp<-preProcess(bothscaled, method='pca', pcaComp=3) %>% predict(bothscaled)
# 
# fviz_nbclust(decomp, kmeans, method = 'gap_stat')
# 
# km6<-kmeans(decomp, 6, nstart = 10)
# 
# plotclusters(bothscaled[,c(1:8)], km6$cluster)

## define clusters to be used for year-to-year comps ####
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
compclusts<-as.factor(hk6$cluster)
# compclusts<-as.factor(hk62$cluster)
# 
# compclusts<-as.factor(km7$cluster)
# compclusts<-as.factor(clara7$cluster)
# compclusts<-as.factor(pam7$cluster)
# compclusts<-as.factor(hk7$cluster)
# compclusts<-as.factor(hk72$cluster)

compclusts<-as.factor(km8$cluster)
compclusts<-as.factor(clara8$cluster)
compclusts<-as.factor(pam8$cluster)
compclusts<-as.factor(hk8$cluster)
compclusts<-as.factor(hk82$cluster)

compclusts<-as.factor(km11$cluster)
compclusts<-as.factor(clara11$cluster)
compclusts<-as.factor(pam11$cluster)
compclusts<-as.factor(hk11$cluster)
compclusts<-as.factor(hk112$cluster)

## name cluster levels ####
levels(compclusts)<-c('MF Recycler', 'MF creator','Hybrid Attacker',
                      'Defender', 'B2B support', 'Attacker')
## compare cluster assignments across years ####
curr$cluster<-compclusts[currrows]
prev$cluster<-compclusts[-currrows]
commonplayers<-intersect(prev$Player, curr$Player)
prevtest<-subset(prev, Player %in% commonplayers) %>% arrange(Player)
currtest<-subset(curr, Player %in% commonplayers) %>% arrange(Player)
prevtest$Player<-droplevels(prevtest$Player)
currtest$Player<-droplevels(currtest$Player)
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
plotclusters(bothscaled[,passnumvars], compclusts)
plotclusters(bothscaled[,passstylevars], compclusts)
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

plotweb(netdat, bor.col.interaction = 'white', 
        col.interaction = linecol, 
        col.high = boxcol, col.low = boxcol,
        bor.col.high = boxcol, bor.col.low = boxcol)

