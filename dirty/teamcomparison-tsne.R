library(kknn)
library(tidyr)
library(ggplot2)
plot_cluster=function(data, var_cluster, palette, title="")
{
  ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
    geom_point(size=2) +
    guides(colour=guide_legend(override.aes=list(size=6))) +
    xlab("") + ylab("") +
    ggtitle(title) +
    theme_light(base_size=20) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.box = "horizontal") + 
    scale_colour_brewer(palette = palette) 
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
tsne_out <- Rtsne(tsneinput,pca=T,perplexity=2, dims=2, initial_dims = 4) # Run TSNE
plot(tsne_out$Y,asp=1)
#text(tsne_out$Y,label=knnset$Team, cex=.5)
dtsne<-as.data.frame(tsne_out$Y)

#make data frame of best eps for each k

dbscan::kNNdistplot(dtsne, k =  1)
fit<-dbscan::dbscan(dtsne, minPts = 1, eps = 1.1)
clusters<-factor(fit$cluster)
p<-plot_cluster(dtsne,'clusters','Set3')
plot(p)

#fit<-kmeans(dtsne, centers=4, nstart = 50)


clusters<-knnset %>% mutate(cluster=fit$cluster)
clusters$Team[clusters$cluster==3]
View(clusters[clusters$cluster==3,])
#plot_cluster(dtsne, 'clusters', 'Set3')

#breakdown burther

clust1<-subset(clusters, cluster==1) %>% select_if(is.numeric) %>% as.data.frame() %>% as.matrix()
set.seed(42)
tsne_out <- Rtsne(clust1,pca=T,perplexity=3, dims=2, initial_dims = 4) # Run TSNE
plot(tsne_out$Y,asp=1)
dtsne1<-tsne_out$Y

fit<-kmeans(dtsne1, centers=8, nstart = 50)


clusters1<-subset(clusters, cluster==1) %>% mutate(cluster=fit$cluster)
clusters1$Team[clusters1$cluster==1]
