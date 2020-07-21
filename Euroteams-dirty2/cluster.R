source('./MLSplayers-dirty/soccer_util_fxns.R')



df<-read.csv('top5teams.csv')

df$PrgSpeed<-(df$PassPrgDist+df$CarryPrgDist)/(df$Touches)

df$Crs<-df$Crs-df$CK


## attacking pca dim reduction ####
attvars<-c('CrsPA', 'Crs','PassHead', 'PassHigh',
           'Poss',
            'PrgSpeed', 
           'Sh', 'SoT')
attscaled<-df %>% mutate(LeagueNum=as.numeric(as.factor(League)))%>%
  select(all_of(c(attvars, 'LeagueNum'))) %>% group_by(LeagueNum) %>%
  scale() %>% as.data.frame %>% select(-LeagueNum)

library(FactoMineR)
res.pca <- PCA(attscaled, graph=FALSE, ncp=5)

res.pca$var$cor %>% 
  promax 

library(psych)
library(GPArotation)
fit <- principal(attscaled, nfactors=5, rotate="promax")
print(fit)
simple<-fit$scores %>% as.data.frame()
colnames(simple)<-c('ShotVol', 'Aerial', 'Crossing',
                    'ProgSpeed', 'Possession')
att<-simple

## def pca dim reduction ####
toAdj=c('Int', 'Blocks',
        'PressDef3rd', 'PressMid3rd', 'PressAtt3rd',
        'TklDef3rd', 'TklMid3rd', 'TklAtt3rd', 'Clr', 'TklW')
for (stat in toAdj){
  df[[paste0('pAdj', stat)]]<-
    df[[stat]]*2/(1+exp(1)^(-.1*(df$Poss-50)))
}

defvars<-c('pAdjInt', 'PressMid3rd','PressAtt3rd',
           'pAdjTklW', 'pAdjBlocks', 'pAdjClr')
defscaled<-select(df, all_of(defvars)) %>% scale() %>% as.data.frame()

res.pca <- PCA(defscaled, graph=FALSE, ncp=3)

res.pca$var$cor %>% 
  promax 

fit <- principal(defscaled, nfactors=2, rotate="promax")
print(fit)
simple<-fit$scores %>% as.data.frame()
colnames(simple)<-c('DeepDef', 'HighPress')
def<-simple


## join and cluster ####
simple<-cbind(att, def)


library(clValid)
clv<-clValid(simple, 2:15, clMethods=c('hierarchical',
                                     'kmeans', 'diana',
                                     'clara'),
             method = 'complete',
             validation = 'internal')
summary(clv)

km8<-kmeans(simple, 8, nstart = 10)
clusts<-km8$cluster %>% as.factor

set.seed(50)
dtsne<-Rtsne::Rtsne(simple, perplexity=8)
plot(dtsne$Y)

km2<-kmeans(dtsne, 2,)


##radars####

#makegroups
for (i in levels(clusts)){
  assign(paste0('group', i), simple[clusts==i,])
}

#makeaggs
for (i in levels(clusts)){
  assign(paste0('groupaggs', i),
         get(paste0('group', i)) %>% summarise_all(median))
}


#graph parameters
means<-rep(0, length(simple))
names(means)<-colnames(simple)
col <- c('#3f2199')
colors_border <- c('black', col)
colors_in <- c(scales::alpha('white', 0), scales::alpha(col,0.3))

#make dfs for radars
for (i in levels(clusts)){
  assign(paste0('radardf', i),
         rbind(rep(3, length(simple)),
               rep(-3, length(simple)),
               means,
               get(paste0('groupaggs',i))))
}

#make radars
library(fmsb)

for (i in levels(clusts)){
  radarchart(get(paste0('radardf', i)),
             pty=32,
             pcol=colors_border, pfcol=colors_in,plwd=c(2,4),plty=1,
             cglcol='grey', cglty=1, axislabcol = 'black', cglwd=1.5,
             centerzero = T,
             title=paste('Group', i)
  )
}



##explore ####
df[clusts==8,c('Squad')]


plotclusters()