source('../MLSplayers-dirty/soccer_util_fxns.R')
library(dplyr)
library(ggplot2)
df<-read.csv('../top5teams.csv')


df$ProgDist<-df$PassPrgDist+df$CarryPrgDist
df$CrsPerc<-df$CrsPA/df$PPA 
df$CrsPerc<-tidyr::replace_na(df$CrsPerc, 0)

df$percProg<-df$ProgPasses/df$PassLive
df$percPressure<-df$PassPress/df$PassLive
df$percMiscon<-df$Miscon/df$Targ
df$ProgPassSpeed<-df$PassPrgDist/df$Touches
df$ProgCarrySpeed<-df$CarryPrgDist/df$Touches
df$OneTouchPass<-df$PassLive/df$Touches
df$CarryPassPrgRatio<-df$CarryPrgDist/df$PassPrgDist
df$TotalProg<-(df$CarryPrgDist+df$PassPrgDist)/df$Touches

df %>% arrange(desc(OneTouchPass)) %>% select(Squad, OneTouchPass) %>% View()
df %>% arrange(desc(ProgCarrySpeed)) %>% select(Squad, ProgCarrySpeed) %>% View()
df %>% arrange(desc(ProgPassSpeed)) %>% select(Squad, ProgPassSpeed) %>% View()
df %>% arrange(desc(TotalProg)) %>% select(Squad, TotalProg) %>% View()


# per touch adjustments ####
toadj<-c('Sh', 'KP', 
         'ProgPasses', 'DribAtt', 'PPA','CrsPA',
         'CarryPrgDist', 'PassPrgDist')

for (var in toadj){
  df[[paste0(var, 'Adj')]]<-df[[var]]/df$TouchesLive
}


adjdf<-df %>% select(contains('Adj')) %>%
#  mutate(PressPass=df$PassPress/df$PassLive) %>%
  mutate(PassComp=df$PassCmp.) 


## attacking pca dim reduction ####
scaled<-adjdf %>% scale() %>% as.data.frame 

library(FactoMineR)
res.pca <- PCA(scaled, graph=FALSE, ncp=8)

res.pca$var$cor %>% 
  promax 

library(psych)
library(GPArotation)
fit <- principal(scaled, nfactors=6, rotate="promax")
print(fit)
simple<-fit$scores %>% as.data.frame()
colnames(simple)<-c('Creator', 'Shooter', 'BallRetainer',
                    'Dribbler','Progressor', 'Crosser')

##cluster ####

library(factoextra)
cordist<-get_dist(scaled, method='pearson')

set.seed(50)
dtsne<-Rtsne::Rtsne(cordist, perplexity=15, is_distance=T)
plot(dtsne$Y)
restsne<-dtsne$Y %>% as.data.frame()

plot_clust_tendency(2:15, as.data.frame(dtsne$Y), 'tsne')

basic_clusts(restsne, 7)

plot_cluster(restsne, km7$cluster)

plot_radars(km7$cluster, scaled)

library(cluster)
pam7<-pam(restsne, 7)
plot_cluster(restsne, as.factor(pam7$clustering))

library(dbscan)
reshdb<-hdbscan(restsne, 24)
plot_cluster(restsne, as.factor(reshdb$cluster))

resdb<-dbscan(restsne, minPts=50, eps=4.5)
plot_cluster(restsne, as.factor(resdb$cluster))

plot_radars(pam7$clustering, simple)
plot_cluster(restsne, as.factor(pam7$clustering))


df$Player[km7$cluster=='6']


## testing ####
plot_clust_tendency(2:20, scaled)

basic_clusts(scaled, 6)

plot_radars(km6$cluster, scaled)

cordist<-get_dist(scaled, method = 'pearson')
hc<-hclust(cordist)
fviz_dend(hc, k = 18)

clusts<-cutree(hc, 18)
df$Player[clusts==14]

## cluster 2 ####

clust2vars<-c('Sh', 'npxG', 'TouchesAttPen',
              'KP', 'xA', 'TouchesAtt3rd', 'PPA',
              'CrsPA', 'DribAtt', 'DribSucc',
              'Carries', 'CarryPrgDist', 'ProgPasses', 'PassPrgDist',
              'TouchesMid3rd',
              'SCADrib', 'SCAPassLive', 'SCASh', 
              'PassHead', 'AerWon')

library(tidyr)
clust2scaled<-df %>% replace(is.na(.),0) %>%
  mutate(LeagueNum=as.numeric(as.factor(League)))%>% #add a leaguenum so we can group by league
  select(all_of(c(clust2vars, 'LeagueNum'))) %>% group_by(LeagueNum) %>% #group
  scale() %>% as.data.frame %>% select(-LeagueNum) %>% #scale within groups to control for league differences
  mutate(clust1=df$clust1)

grp1<-subset(clust2scaled[clusts=="1",])

clusts21<-hkmeans(grp1, 7)$cluster %>% as.factor()
plot_radars(clusts21, grp1)

df %>% filter(clusts1=="1") %>% filter(!!clusts21=="4") %>% select('Player')
