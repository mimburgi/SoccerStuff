library(PCAmixdata)
library(FactoMineR)
library(factoextra)

# testdf<-select(bothscaledall, !one_of(c('teamA3pass', 'teamM3pass', 'teamD3pass', 'teampass', 'teamxG',
#                                         'xG','PassScore','xBperc')))

usedvars<-c("npxG", 'xA', "xB", "PassPct", 'passprg', 
            'Crs', 'SuccDrib','targeted',
            'shortpasspct', 'longpasspct') 
testdf<-select(bothscaledall, one_of(usedvars))

## PCA varimax ####
res.pca <- PCA(testdf, graph=FALSE, ncp=7)

fviz_screeplot(res.pca)

res.pca$var$cor %>% # table with factor loadings
  varimax # but ask for a varimax rotation to improve interpretability
fviz_pca_var(res.pca, repel = TRUE) # the repel = TRUE argument makes sure the text is displayed nicely on the graph

## PCA promax ####
library(psych) #for fa
library(GPArotation) #for rotation within fa
fit <- principal(bothscaled, nfactors=7, rotate="promax")
print(fit)
simple<-fit$scores %>% as.data.frame()
colnames(simple)<-c('Scoring', 'Verticality','Direct Creation','Buildup Involvement', 
                    'Crossing', 'Ball Retention', 'Dribbling')
simple$group<-compclusts
plotclusters(simple[,c(1:3)], compclusts)
plotclusters(simple[,c(3:7)], compclusts)




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
both[which(findcenters$dist11==min(findcenters$dist11)),c('Player', 'cluster')]

## def radar chart  ####

BPDs<-simple [both$cluster=='Ball-Playing Def.',] 
Outlets<-simple [both$cluster=='Backfield Outlet',]

BPDaggs<-select_if(BPDs, is.numeric) %>% summarise_all(median)
Outletaggs<-select_if(Outlets, is.numeric) %>% summarise_all(median)
means<-rep(0, length(BPDaggs))
names(means)<-colnames(BPDaggs)

defRadarDat<- rbind(means, BPDaggs, Outletaggs)

rownames(defRadarDat) <- c("Average", "BPD", 'Outlet')

defRadarDat_withscale <- rbind(rep(2,length(BPDaggs)) , 
                               rep(-2, length(BPDaggs)) , 
                               defRadarDat)

col <- c('#3f2199', '#b41e51')
colors_border <- c('black', col)
colors_in <- c(scales::scales::alpha('white', 0), scales::scales::alpha(col,0.3))



radarchart(defRadarDat_withscale,
           pty=32,
           pcol=colors_border , pfcol=colors_in , plwd=c(2,4,4) , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=1.5, 
           #vlcex=0,
           centerzero = T)



## dmf radar chart  ####

Pivots<-simple [both$cluster=='Pivot',] 
Recyclers<-simple [both$cluster=='Recycler',]

Pivotaggs<-select_if(Pivots, is.numeric) %>% summarise_all(median)
Recycleraggs<-select_if(Recyclers, is.numeric) %>% summarise_all(median)
means<-rep(0, length(Pivotaggs))
names(means)<-colnames(Pivotaggs)

mfRadarDat<- rbind(means, Pivotaggs, Recycleraggs)
rownames(mfRadarDat) <- c("Average", "Pivot", 'Recycler')

mfRadarDat_withscale <- rbind(rep(2,length(Pivotaggs)) , 
                               rep(-2, length(Pivotaggs)) , 
                               mfRadarDat)

col <- c('#3f2199', '#b41e51')
colors_border <- c('black', col)
colors_in <- c(scales::alpha('white', 0), scales::alpha(col,0.3))



radarchart(mfRadarDat_withscale,
           pty=32,
           pcol=colors_border , pfcol=colors_in , plwd=c(2,4,4) , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=1.5, 
           #vlcex=0,
           centerzero = T)

## fb radar chart  ####

Crossers<-simple [both$cluster=='Crossing Specialist',] 
Supports<-simple [both$cluster=='Wide Support',]

Crosseraggs<-select_if(Crossers, is.numeric) %>% summarise_all(median)
Supportaggs<-select_if(Supports, is.numeric) %>% summarise_all(median)
means<-rep(0, length(Crosseraggs))
names(means)<-colnames(Crosseraggs)

mfRadarDat<- rbind(means, Crosseraggs, Supportaggs)
rownames(mfRadarDat) <- c("Average", "Crosser", 'Support')

mfRadarDat_withscale <- rbind(rep(2,length(Crosseraggs)) , 
                              rep(-2, length(Crosseraggs)) , 
                              mfRadarDat)

col <- c('#3f2199', '#b41e51')
colors_border <- c('black', col)
colors_in <- c(scales::alpha('white', 0), scales::alpha(col,0.3))



radarchart(mfRadarDat_withscale,
           pty=32,
           pcol=colors_border , pfcol=colors_in , plwd=c(2,4,4) , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=1.5, 
           #vlcex=0,
           centerzero = T)

## att radar charts  ####

Scorers<-simple [both$cluster=='Pure Scorer',] 
Hybrids<-simple [both$cluster=='Hybrid Scorer',]

Scoreraggs<-select_if(Scorers, is.numeric) %>% summarise_all(median)
Hybridaggs<-select_if(Hybrids, is.numeric) %>% summarise_all(median)
means<-rep(0, length(Scoreraggs))
names(means)<-colnames(Scoreraggs)

mfRadarDat<- rbind(means, Scoreraggs, Hybridaggs)
rownames(mfRadarDat) <- c("Average", "Scorer", 'Hybrid')

mfRadarDat_withscale <- rbind(rep(2,length(Scoreraggs)) , 
                              rep(-2, length(Scoreraggs)) , 
                              mfRadarDat)

col <- c('#3f2199', '#b41e51')
colors_border <- c('black', col)
colors_in <- c(scales::alpha('white', 0), scales::alpha(col,0.3))



radarchart(mfRadarDat_withscale,
           pty=32,
           pcol=colors_border , pfcol=colors_in , plwd=c(2,4,4) , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=1.5, 
           #vlcex=0,
           centerzero = T)

##passers radar####
Wides<-simple [both$cluster=='Wide Attacker',] 
Playmakers<-simple [both$cluster=='Playmaker',]

Wideaggs<-select_if(Wides, is.numeric) %>% summarise_all(median)
Playmakeraggs<-select_if(Playmakers, is.numeric) %>% summarise_all(median)
means<-rep(0, length(Wideaggs))
names(means)<-colnames(Wideaggs)

mfRadarDat<- rbind(means, Wideaggs, Playmakeraggs)
rownames(mfRadarDat) <- c("Average", "Wide", 'Playmaker')

mfRadarDat_withscale <- rbind(rep(2,length(Wideaggs)) , 
                              rep(-2, length(Wideaggs)) , 
                              mfRadarDat)

col <- c('#3f2199', '#b41e51')
colors_border <- c('black', col)
colors_in <- c(scales::alpha('white', 0), scales::alpha(col,0.3))



radarchart(mfRadarDat_withscale,
           pty=32,
           pcol=colors_border , pfcol=colors_in , plwd=c(2,4,4) , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=1.5, 
           #vlcex=0,
           centerzero = T)

##shuttler radar####
Shuttlers<-simple [both$cluster=='Shuttler',] 
Playmakers<-simple [both$cluster=='Playmaker',]

Shuttleraggs<-select_if(Shuttlers, is.numeric) %>% summarise_all(median)
Playmakeraggs<-select_if(Playmakers, is.numeric) %>% summarise_all(median)
means<-rep(0, length(Shuttleraggs))
names(means)<-colnames(Shuttleraggs)

mfRadarDat<- rbind(means, Shuttleraggs, Pivotaggs)
rownames(mfRadarDat) <- c("Average", "Shuttler", 'Playmaker')

mfRadarDat_withscale <- rbind(rep(2,length(Shuttleraggs)) , 
                              rep(-2, length(Shuttleraggs)) , 
                              mfRadarDat)

col <- c('#3f2199', '#b41e51')
colors_border <- c('black', col)
colors_in <- c(scales::alpha('white', 0), scales::alpha(col,0.3))



radarchart(mfRadarDat_withscale,
           pty=32,
           pcol=colors_border , pfcol=colors_in , plwd=c(2,4,4) , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=1.5, 
           #vlcex=0,
           centerzero = T)

