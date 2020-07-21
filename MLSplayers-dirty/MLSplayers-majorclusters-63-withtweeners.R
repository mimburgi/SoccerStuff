library(dplyr)
library(ggplot2)
library(factoextra)
library(tidyr)
library(ppclust)

source('soccer_util_fxns.R') #for plotclusters fxn
## concat data across both years ####
curr<-read.table('2019summary.txt', header = T) %>% arrange(Player)
currscaled<-curr %>% select_if(is.numeric) %>% scale() %>% as.data.frame()
# currscaled<-select(currscaled, -c(xGteamperc, shotteamperc, A3Passes, M3Passes, D3Passes)) %>%
#   select(-c(teamA3pass, teamM3pass, teamD3pass, teampass, teamxG, teamshots, xPlace, ShotDist, KPDist,
#             xGper, xAper, passteamperc))
currscaled[currscaled < -3]<- -3

prev<-read.table('2018summary.txt', header = T)  %>% arrange(Player)
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
# #4
# clara4<-clara(bothscaled, 4)
# km4<-kmeans(bothscaled, 4)
# pam4<-kmeans(bothscaled, 4)
# hk4<-hkmeans(bothscaled, 4)
# 
# 
# #5
# clara5<-clara(bothscaled, 5)
# km5<-kmeans(bothscaled, 5)
# pam5<-kmeans(bothscaled, 5)
# hk5<-hkmeans(bothscaled, 5)
# 
# #6
# clara6<-clara(bothscaled, 6)
# km6<-kmeans(bothscaled, 6)
# pam6<-kmeans(bothscaled, 6)
set.seed(0)
hk6<-hkmeans(bothscaled, 6)
set.seed(10)
fuzzy<-fcm(bothscaled, centers = hk6$centers)
# fuzzy2<-fcm(bothscaled, centers = hk6$centers,
#             fixcent = T, nstart = 5)

# hk62<-hkmeans(bothscaled, 6, hc.method = 'complete')
# 
# 
# #7
# clara7<-clara(bothscaled, 7)
# km7<-kmeans(bothscaled, 7)
# pam7<-kmeans(bothscaled, 7)
# hk7<-hkmeans(bothscaled, 7)
# hk72<-hkmeans(bothscaled, 7, hc.method = 'complete')

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
# compclusts<-as.factor(hk6$cluster)
# compclusts<-as.factor(hk62$cluster)
# 
# compclusts<-as.factor(km7$cluster)
# compclusts<-as.factor(clara7$cluster)
# compclusts<-as.factor(pam7$cluster)
# compclusts<-as.factor(hk7$cluster)
# compclusts<-as.factor(hk72$cluster)

compclusts<-as.factor(fuzzy$cluster)


##name cluster levels ####
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
caret::confusionMatrix(prevtest$cluster, currtest$cluster)



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
  group_by(cluster) %>%
  mutate(freq = n / sum(n))

ggplot(ftable, aes(x=cluster, y=Pos, fill=freq)) + geom_tile() +
  scale_fill_gradient(low="darkgrey", high="darkred")

## set a tweener threshold from fuzzy cluster probs ####
misclassedplayers<-prevtest$Player[prevtest$cluster!=currtest$cluster]

threshold<-.1

fuzzyprobs<-as.data.frame(fuzzy$u)
fuzzyprobsprev<-fuzzyprobs[-currrows,]
fuzzyprobstest<-fuzzyprobsprev[which(prevtest$Player %in% misclassedplayers),]

colnames(fuzzyprobstest)<-levels(compclusts)
fuzzyprobstest$secondprobs<-apply((fuzzyprobstest),1,secondmax) 
fuzzyprobstest$firstprobs<-apply(fuzzyprobstest,1,max)
fuzzyprobstest$tweener<-0
fuzzyprobstest$tweener[(fuzzyprobstest$firstprobs-fuzzyprobstest$secondprobs) < threshold]<-1
sum(fuzzyprobstest$tweener)
