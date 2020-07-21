source('../MLSplayers-dirty/soccer_util_fxns.R')
library(dplyr)
df<-read.csv('../top5players.csv')

df$PrgSpeed<-(df$PassPrgDist+df$CarryPrgDist)/(df$Touches)

df$Crs<-df$Crs-df$PassCK

df$ShperTouch<-df$Sh/df$Touches
df$KPperTouch<-df$KP/df$Touches

df$percPressure<-df$PassPress/df$PassLive
df$percMiscon<-df$Miscon/df$Targ
df$percProg<-df$ProgPasses/df$PassLive
df$HighPress<-(df$PressAtt3rd+df$PressMid3rd)/df$Press


## attacking pca dim reduction ####
attvars<-c('PassLive',
            'KP', 'KPperTouch',
           'Sh', 'ShperTouch',
           'PassCmp.',
           #'DribAtt',
           'TouchesAtt3rd', 'TouchesMid3rd', 'TouchesDef3rd',
           'ProgPasses', 'PassPrgDist',
           'TouchesAttPen', 'PassF3')
df<-df %>% #remove players that didn't play enough and GKs
  subset(Player != 'Player') %>%
  subset(Min > 1000) %>% subset(Pos != 'GK') 
att<-df %>% select(all_of(c(attvars))) %>% scale() %>% as.data.frame()

cordist<-factoextra::get_dist(att, method='spearman')

set.seed(530)
dtsne<-Rtsne::Rtsne(cordist, perplexity=35, is_distance=T)
plot(dtsne$Y)
restsne<-as.data.frame(dtsne$Y)

library(dbscan)

reshdb<-hdbscan(restsne, minPts=30)
plot_cluster(restsne, reshdb$cluster)
plot_radars(reshdb$cluster, att)

resdb<-dbscan(restsne, minPts=50, eps=4)
plot_cluster(restsne, resdb$cluster)

plot_radars(resdb$cluster, att)

clusts<-resdb$cluster %>% as.factor


df$Player[clusts==2]


levels(clusts)<-c('Noise', 'Defenders', 'Midfielders','Attackers')
df$majorclust<-clusts

library(NbClust)
nbres<-NbClust(att, diss = cordist, distance=NULL, method='kmeans')
summary(nbres)

km10

library(clValid)
clres<-clValid(att, 2:10, metric='correlation',
               clMethods=c('hierarchical', 'kmeans',
                           'pam'),
               validation='internal',
               method = 'complete')

hcclusts<-hclust(cordist) %>% cutree(8)

plot_radars(hcclusts, att)

## break down central attackers ####


attvars<-c('ShperTouch',
                'KPperTouch',
                'DribAtt',
                'CrsPA',
                'ProgPasses', 'PassCmp.',
           'Touches')
catt<-df %>% filter(!!clusts=='Attackers') %>%
  select(all_of(c(attvars))) %>% scale() %>% as.data.frame()



cattcordist<-get_dist(catt, method='spearman')
set.seed(400)
dtsne<-Rtsne::Rtsne(cattcordist, perplexity=27, is_dist=T)
plot(dtsne$Y)
restsne<-dtsne$Y %>% as.data.frame()


resdb<-dbscan(restsne, minPts = 20, eps=2.5)
plot_cluster(restsne, resdb$cluster)


plot_radars(resdb$cluster, catt)

cattclusts<-resdb$cluster 

df %>% filter(!!clusts=='Attackers') %>% filter(cattclusts==3) %>% select(Player)

shootervars<-c('Sh','npxG', 'ShperTouch',
               'DribAtt',
               'KP','xA', 'KPperTouch',
               'AerWon', 'PassHead')

shooters<-df %>% filter(majorclust=='Central Attackers') %>% filter(cattclusts==1) %>%
  select(all_of(shootervars)) %>% scale %>% as.data.frame


res.pca <- PCA(shooters, graph=FALSE, ncp=5)

res.pca$var$cor %>% 
  promax 




shootercordist<-get_dist(shooters, method='spearman')
set.seed(420)
dtsne<-Rtsne::Rtsne(shootercordist, perplexity=7, is_dist=T)
plot(dtsne$Y)
restsne<-dtsne$Y %>% as.data.frame()

reshdb<-hdbscan(restsne, 9)
plot_cluster(restsne, reshdb$cluster)

plot_radars(reshdb$cluster, shooters)

resdb<-dbscan(restsne, minPts = 15, eps=7)
plot_cluster(restsne, resdb$cluster)

plot_radars(resdb$cluster, shooters)

df %>% filter(majorclust=='Central Attackers') %>% filter(cattclusts==1) %>% 
  filter(resdb$cluster==4) %>% select(Player)


plot_radars(resdb$cluster, shooters)
