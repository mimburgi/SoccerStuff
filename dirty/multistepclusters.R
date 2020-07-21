library(tidyr)
source('soccer_util_fxns.R')
##initial feature trimming, keep only what might be valuable ####

raw<-read.table('rawdata_full.txt', header = T)

trim<-select(raw, c(Team, League,
                            #percTBKP, 
                            attackMiddle,
                            percOppThird,
                            ThroughballKP,
                            CrossKP, 
                      CounterShots,
                            #FastSF,
                            #percLongKeyPasses,
                            LongKeyPasses,
                            ShortKeyPasses,
                            #percLongKP,
                            #OtherKP,
                            TotalKeyPasses,
                            #percOutofBoxShots,
                            OutOfBoxShots,
                            SixYardBoxShots,
                            #PenaltyAreaShots,
                            #TotalShots,
                            xGper,
                            xGshot,
                            #xGdiffper,
                            xGAper,
                            #xGAdiffper,
                            SetPieceShots,
                            #percSetPieceShots,
                            TotalPasses,
                            #percLB,
                            OpenPlayShots,
                            TotalDribbles,
                            pAdjTackles,
                            pAdjInts,
                            pAdjClearances,
                            TotalAerial
))


#leagueavs<-trim %>% group_by(League) %>% summarise_if(is.numeric, mean)
#View(leagueavs)


### try tsne on full dataset ####

library(Rtsne)
tsneinput<-select_if(trim, is.numeric) %>% as.data.frame() %>% as.matrix()

#examine pca comp
library(factoextra)
tsneinput %>% PCA(graph=F) %>% fviz_screeplot()

set.seed(42)
tsne_out <- Rtsne(tsneinput,pca=T,perplexity=10, dims=2, initial_dims = 3) # Run TSNE
plot(tsne_out$Y,asp=1) #two large clusters here
dtsne1<-as.data.frame(tsne_out$Y)


### split data by initial clustering ####

#try labeling clusters with knn, looks clear enough that that will work
fit1<-kmeans(dtsne1, centers=2, nstart = 50)
clusters1<-as.factor(fit1$cluster)

clusterset<-trim %>% mutate(cluster1=clusters1)
plot_cluster(dtsne1, clusters1) #looks right

full<-mutate(trim, cluster1 = clusters1)

#examine differences across groups- are they meaningful?
cluster1a_aggs<-subset(full, clusters1 == "1") %>%
  select_if(is.numeric) %>% colMeans()
cluster1b_aggs<-subset(full, clusters1 == "2") %>%
  select_if(is.numeric) %>% colMeans()
cluster1aggs<-rbind(cluster1a_aggs, cluster1b_aggs) %>% View()

#does look meaningful, one looks to be better teams in general

#### try tsne on each cluster individually ####
tsneinput2a<-tsneinput[clusters1=="1",]
tsneinput2a %>% PCA(graph=F) %>% fviz_screeplot()

tsneinput2b<-tsneinput[clusters1=="2",]
tsneinput2b %>% PCA(graph=F) %>% fviz_screeplot()


set.seed(7)
tsne_out2a <- Rtsne(tsneinput2a,pca=T,perplexity=5, dims=2, initial_dims = 3) # Run TSNE
plot(tsne_out2a$Y,asp=1) #four clusters here
dtsne2a<-as.data.frame(tsne_out2a$Y)


set.seed(100)
tsne_out2b <- Rtsne(tsneinput2b,pca=T,perplexity=6, dims=2, initial_dims = 1) # Run TSNE
plot(tsne_out2b$Y,asp=1) #four clusters here
dtsne2b<-as.data.frame(tsne_out2b$Y)

### split data by second clustering ####

fit2a<-kmeans(dtsne2a, centers=4, nstart = 50)
clusters2a<-as.factor(fit2a$cluster)

plot_cluster(dtsne2a, clusters2a) #looks right


fit2b<-kmeans(dtsne2b, centers=4, nstart = 50)
clusters2b<-as.factor(fit2b$cluster)

plot_cluster(dtsne2b, clusters2b) #not right, try dbscan


dbscan::kNNdistplot(dtsne2b, k =  6)
fit2b<-dbscan::dbscan(dtsne2b, minPts = 6, eps = 8)
clusters2b<-factor(fit2b$cluster)
plot_cluster(dtsne2b, clusters2b) #there we go
levels(clusters2b)<-c('5', '6', '7', '8')



full$cluster2[full$cluster1=='2']<-as.character(clusters2b)
full$cluster2[full$cluster1=='1']<-as.character(clusters2a)
full$cluster2<-as.factor(full$cluster2)


# #### tsne on each of the eight ####
# tsneinput3a<-tsneinput[full$cluster2=="1",]
# tsneinput3a %>% PCA(graph=F) %>% fviz_screeplot()
# 
# tsneinput3b<-tsneinput[full$cluster2=="2",]
# tsneinput3b %>% PCA(graph=F) %>% fviz_screeplot()
# 
# tsneinput3c<-tsneinput[full$cluster2=="3",]
# tsneinput3c %>% PCA(graph=F) %>% fviz_screeplot()
# 
# tsneinput3d<-tsneinput[full$cluster2=="4",]
# tsneinput3d %>% PCA(graph=F) %>% fviz_screeplot()
# 
# tsneinput3e<-tsneinput[full$cluster2=="5",]
# tsneinput3e %>% PCA(graph=F) %>% fviz_screeplot()
# 
# 
# #5 looks about reasonable for all of them
# 
# set.seed(23)
# tsne_out3a <- Rtsne(tsneinput3a,pca=T,perplexity=2, dims=2, initial_dims = 5) # Run TSNE
# plot(tsne_out3a$Y,asp=1) #three clusters here
# dtsne3a<-as.data.frame(tsne_out3a$Y)
# 
# 
# set.seed(23)
# tsne_out3b <- Rtsne(tsneinput3b,pca=T,perplexity=2, dims=2, initial_dims = 5) # Run TSNE
# plot(tsne_out3b$Y,asp=1) #two clusters here
# dtsne3b<-as.data.frame(tsne_out3b$Y)
# 
# set.seed(23)
# tsne_out3c <- Rtsne(tsneinput3c,pca=T,perplexity=2, dims=2, initial_dims = 5) # Run TSNE
# plot(tsne_out3c$Y,asp=1) #four clusters here
# dtsne3c<-as.data.frame(tsne_out3c$Y)
# 
# 
# set.seed(23)
# tsne_out3d <- Rtsne(tsneinput3d,pca=T,perplexity=3, dims=2, initial_dims = 5) # Run TSNE
# plot(tsne_out3d$Y,asp=1) #two clusters here
# dtsne3d<-as.data.frame(tsne_out3d$Y)
# 
# 
# #this will need a different approach, tsne can't separate these teams
# #they're likely the 'average' teams with no clear identity in general
# set.seed(23)
# tsne_out3e <- Rtsne(tsneinput3e,pca=T,perplexity=4, dims=2, initial_dims = 5) # Run TSNE
# plot(tsne_out3e$Y,asp=1) #two clusters here
# 
# ### split data by third clustering ####
# 
# 
# fit3a<-kmeans(dtsne3a, centers=3, nstart = 50)
# clusters3a<-as.factor(fit3a$cluster)
# 
# plot_cluster(dtsne3a, clusters3a) #looks right
# 
# fit3b<-kmeans(dtsne3b, centers=2, nstart = 50)
# clusters3b<-as.factor(fit3b$cluster)
# 
# plot_cluster(dtsne3b, clusters3b) #looks right
# 
# fit3c<-kmeans(dtsne3c, centers=4, nstart = 50)
# clusters3c<-as.factor(fit3c$cluster)
# 
# plot_cluster(dtsne3c, clusters3c) #looks right
# 
# fit3d<-kmeans(dtsne3d, centers=3, nstart = 50)
# clusters3d<-as.factor(fit3d$cluster)
# 
# plot_cluster(dtsne3d, clusters3d) #looks right
# 
# fit3e<-kmeans(dtsne3e, centers=3, nstart = 50)
# clusters3e<-as.factor(fit3e$cluster)
# 
# plot_cluster(dtsne3e, clusters3e) #looks right
# 
# 
# levels(clusters3b)<-c('4','5')
# levels(clusters3c)<-c('6','7','8', '9')
# levels(clusters3d)<-c('10','11','12')
# 
# 
# full$cluster3[full$cluster2=='1']<-as.character(clusters3a)
# full$cluster3[full$cluster2=='2']<-as.character(clusters3b)
# full$cluster3[full$cluster2=='3']<-as.character(clusters3c)
# full$cluster3[full$cluster2=='4']<-as.character(clusters3d)
# full$cluster3[full$cluster2=='5']<-'13'
# full$cluster3<-as.factor(full$cluster3)
# 
# 
# full$Team[full$cluster3=='1']
# 
# full$Team[full$cluster3=='2']
# 
# full$Team[full$cluster3=='3']
# 
# full$Team[full$cluster3=='4']
# 
# full$Team[full$cluster3=='5']
# 
# full$Team[full$cluster3=='6']
# 
# full$Team[full$cluster3=='7']
# 
# full$Team[full$cluster3=='8']
# 
# full$Team[full$cluster3=='9']
# 
# full$Team[full$cluster3=='10']
# 
# full$Team[full$cluster3=='11']
# 
# full$Team[full$cluster3=='12']
# 
# full$Team[full$cluster3=='13']
