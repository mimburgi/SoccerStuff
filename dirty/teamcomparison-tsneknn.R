library(kknn)
library(tidyr)
library(ggplot2)
plot_cluster=function(data, var_cluster, title="")
{
  library(RColorBrewer)
  palette<- colorRampPalette(brewer.pal(8, "Set2"))(length(levels(var_cluster)))
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
    geom_point(size=2) +
    scale_color_manual(values = palette) +
    xlab("") + ylab("") +
    ggtitle(title) +
    theme_light(base_size=20) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.box = "horizontal") 
}

knnset<-select(raw_extra, c(Team, League,
                            #percTBKP, 
                            #attackMiddle,
                            #percOppThird,
                            ThroughballKP,
                            CrossKP,
                            CounterShots,
                            #percLongKeyPasses,
                            LongKeyPasses,
                            ShortKeyPasses,
                            #percLongKP,
                            #OtherKP,
                            #percOutofBoxShots
                            OutOfBoxShots,
                            SixYardBoxShots
                            #PenaltyAreaShots,
                            #TotalShots
                            #xGper,
                            #xGshot,
                            #xGdiffper,
                            #xGAper,
                            #xGAdiffper,
                            #SetPieceShots
                            #FreekickKP,
                            #CornerKP
                            #percSetPieceShots,
                            #TotalPasses,
                            #percLB,
                            #OpenPlayShots,
                            #TotalDribbles,
                            #TotalTackles,
                            #Interceptions,
                            #Clearances,
                            #TotalAerial
))

library(Rtsne)
tsneinput<-select_if(knnset, is.numeric) %>% as.data.frame() %>% as.matrix()
# for (parval in seq(5,33,3)){
#   set.seed(42)
#   tsne_out <- Rtsne(tsneinput,pca=FALSE,perplexity=parval) # Run TSNE
#   # Show the objects in the 2D tsne representation
#   plot(tsne_out$Y,asp=1)
# }

set.seed(42)
tsne_out <- Rtsne(tsneinput,pca=T,perplexity=4, dims=2, initial_dims = 4) # Run TSNE
plot(tsne_out$Y,asp=1)
#text(tsne_out$Y,label=knnset$Team, cex=.5)
dtsne<-as.data.frame(tsne_out$Y)
# 
# #make data frame of best eps for each k
# 
# dbscan::kNNdistplot(dtsne, k =  2)
# fit<-dbscan::dbscan(dtsne, minPts = 2, eps = 3)
# clusters<-factor(fit$cluster)
# 
# library(RColorBrewer)
# p<-plot_cluster(dtsne,clusters)
# plot(p)
# 
# #fit<-kmeans(dtsne, centers=4, nstart = 50)
# 
# 
# clusterset<-knnset %>% mutate(cluster=fit$cluster)
# clusterset$Team[clusterset$cluster==2]
# View(clusters[clusterset$cluster==3,])
# #plot_cluster(dtsne, 'clusters', 'Set3')

#now find nearest neighbor from tsne comp
knnin<-dtsne %>% mutate(Team=knnset$Team)

mlsteams<-which(knnset$League=="MLS")
euroteams<-which(knnset$League!="MLS")
fit<-kknn(Team ~ ., knnin[euroteams,], knnin[mlsteams,],
          k = 3, kernel='rectangular', scale = F)

mlscomp<-subset(knnset, League=='MLS') %>%
  mutate(eurocomp=as.character(fit$fitted.values)) %>%
  mutate(team=subset(raw, League=='MLS')[['Team']])

length(unique(mlscomp$eurocomp))