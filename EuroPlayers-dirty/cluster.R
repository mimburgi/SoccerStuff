source('../MLSplayers-dirty/soccer_util_fxns.R')
library(dplyr)
df<-read.csv('../top5players.csv')

df$PrgSpeed<-(df$PassPrgDist+df$CarryPrgDist)/(df$Touches)

df$Crs<-df$Crs-df$PassCK

df$ShperTouch<-df$Sh/df$Touches
df$KPperTouch<-df$KP/df$Touches



## attacking pca dim reduction ####
attvars<-c('CrsPA',
           'PassLive', 'Carries', 
            'xA', 'KP', 'KPperTouch',
           'npxG', 'Sh', 'ShperTouch',
           'PassCmp.',
           'DribAtt', 'DribSucc',
           'CarryPrgDist', 'PassPrgDist')
df<-df %>% #remove players that didn't play enough and GKs
  subset(Player != 'Player') %>%
  subset(Min > 1000) %>% subset(Pos != 'GK') 
attscaled<-df %>% 
  mutate(LeagueNum=as.numeric(as.factor(League)))%>% #add a leaguenum so we can group by league
  select(all_of(c(attvars, 'LeagueNum'))) %>% group_by(LeagueNum) %>% #group
  scale() %>% as.data.frame %>% select(-LeagueNum) #scale within groups to control for league differences

library(FactoMineR)
res.pca <- PCA(attscaled, graph=FALSE, ncp=6)

res.pca$var$cor %>% 
  promax 

library(psych)
library(GPArotation)
fit <- principal(attscaled, nfactors=6, rotate="promax")
print(fit)
simple<-fit$scores %>% as.data.frame()
colnames(simple)<-c('Finishing', 'Creation', 'BallProg',
                    'Dribbling','Crossing', 'BallRet')

##cluster ####

set.seed(50)
dtsne<-Rtsne::Rtsne(cordist, perplexity=18, is_distance=T)
plot(dtsne$Y)
restsne<-as.data.frame(dtsne$Y)
plot_clust_tendency(2:15, as.data.frame(dtsne$Y))

basic_clusts(restsne, 9)
plot_cluster(restsne, pam9$cluster)

library(dbscan)

reshdb<-hdbscan(restsne, 25)
plot_cluster(restsne, reshdb$cluster)


kNNdistplot(restsne, k=25)
resdb<-dbscan(restsne, minPts=20, eps=3.042)
plot_cluster(restsne, resdb$cluster)

plot_radars(resdb$cluster, scaled)

df$Player[resdb$cluster==8]

hcward<-hclust(cordist, method='ward.D2')
hcavg<-hclust(cordist, method='average')

fviz_dend(hcward, 10)
plot_radars(cutree(hcward, 10), attscaled)

## centers of each cluster ####
df[which(find_dists$dist1==min(find_dists$dist1)),c('Player')]
df[which(find_dists$dist2==min(find_dists$dist2)),c('Player')]
df[which(find_dists$dist3==min(find_dists$dist3)),c('Player')]
df[which(find_dists$dist4==min(find_dists$dist4)),c('Player')]
df[which(find_dists$dist5==min(find_dists$dist5)),c('Player')]
df[which(find_dists$dist6==min(find_dists$dist6)),c('Player')]
