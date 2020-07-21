source('../MLSplayers-dirty/soccer_util_fxns.R')
library(tidyverse)
library(ggplot2)
library(psych)
library(GPArotation)
library(FactoMineR)
library(factoextra)

players<-read.csv('../top5players.csv')
teams<-read.csv('../top5teams.csv')

## team adjustments ####
players$TotalProg<-players$PassPrgDist + players$CarryPrgDist
teams$TotalProg<-teams$PassPrgDist + teams$CarryPrgDist

players$nsSCA<-players$SCADrib+players$SCAPassLive+players$SCAFld
teams$nsSCA<-teams$SCADrib+teams$SCAPassLive+teams$SCAFld

players<-filter(players, Min > 1000)%>%filter(Pos !='GK')

players$Crs<-players$Crs-players$PassCK
teams$Crs<-teams$Crs-teams$PassCK
players$BallsWon<-players$Int+players$TklW
teams$BallsWon<-teams$Int+teams$TklW
players$KPperTouch<-players$KP/players$Touches
players$ShperTouch<-players$Sh/players$Touches

players$TouchperPass<-players$TouchesLive/players$PassLive
players$TouchperAction<-players$TouchesLive/(players$PassLive+players$Sh+players$Dispos + players$Fld)
players$percPassPress<-players$PassPress/players$PassLive

players$percLongPass<-players$LPassAtt/players$PassAtt
players$percShortPass<-players$SPassAtt/players$PassAtt
players$percTB<-players$PassTB/players$PassLive

players$RiskyTouch<-players$Dispos/players$TouchesLive

dontadj<-c(
  names(players)[which(!(names(players)%in%names(teams)))], #stuff like 'Player' and 'Nation' which is player specific
  names(players)[which(grepl(pattern = '\\.', names(players)))], #columns with a period, which is a percentage or xStat-Stat
  names(which(sapply(players, is.character))) #any remaining character columns like Squad
  )
toadj<-names(players)[!(names(players) %in% dontadj)] #the rest, which will be team adjusted

tadjdf<-players

for (player in 1:nrow(tadjdf)){
  for (var in toadj){
    playerteam<-tadjdf$Squad[player]
    teamvar<-teams[[var]][teams$Squad==playerteam]
    tadjdf[[paste0('team', var)]][player]<-teamvar
    tadjdf[[paste0('tAdj', var)]][player]<-
      tadjdf[[var]][player]/tadjdf[[paste0('team', var)]][player]
  }
}

# #remove the team- and original values, rename the tAdj values to original names
# tadjdf<-tadjdf %>%select(which(grepl(pattern = 'Player|Squad|tAdj', names(tadjdf))))
# colnames(tadjdf)<-str_replace(colnames(tadjdf), "tAdj", "")
# # add the unadjusted variables back in
# for (var in dontadj){ 
#   tadjdf<-tadjdf %>% mutate(!!var:= !!players[[var]])
# } 

#remove OG, red cards and ShBlkSav which converted incorrectly because some teams have zero
tadjdf<-tadjdf %>% select(-c(tAdjOG, tAdjCrdR, tAdjBlockShSv))

tadjscaled<-tadjdf %>% select_if(is.numeric) %>% scale() %>% as.data.frame()
scaled<-players %>% select_if(is.numeric) %>% scale %>% as.data.frame()


## initial clustering of major roles ####

initvars<-c(
            'Sh', 'npxG',
            'ShperTouch',
            'KP', 'xA',
            'KPperTouch',
            'PPA', 'PassF3',
            'CrsPA',
            'tAdjProgPasses',
            'tAdjPassPrgDist',
            'TouchperAction',
            'tAdjCarryPrgDist',
            'DribAtt', 'DribSucc',
            'tAdjTouchesAttPen',
            'tAdjTouchesAtt3rd',
            'tAdjTouchesMid3rd',
            'tAdjTouchesDef3rd',
            #'tAdjPassLive',
            'PassCmp.',
            #'SCADrib',
            #'SCAFld',
            'SCAPassLive',
            #'SCASh',
            'tAdjBallsWon',
            'tAdjPressMid3rd',
            'tAdjPressDef3rd',
            'percLongPass',
            'percShortPass',
            'RiskyTouch',
            'percTB')


cordisttadj<-tadjscaled %>% select(all_of(initvars)) %>% get_dist(method='spearman')
#cordist<-scaled %>% select(all_of(initvars)) %>% get_dist(method='spearman')


set.seed(200)
dtsne<-Rtsne::Rtsne(cordisttadj, perplexity=30, is_dist=T)
plot(dtsne$Y)
restsne<-dtsne$Y %>% as.data.frame()

plot_clust_tendency(4:15, restsne, "tSNE perp 30")

library(ppclust)
resfcm<-fcm(restsne, centers=10, m=2.5)
plot_cluster(restsne, resfcm$cluster)
#plot_radars(resfcm$cluster, select(scaled, all_of(initvars)))

library(FactoMineR)
respca<-PCA(select(tadjscaled, all_of(initvars)), scale.unit = F, ncp = 13, graph=F)
respca$var$cor %>% 
  promax 
library(psych)
library(GPArotation)
fit <- principal(select(tadjscaled, all_of(initvars)), nfactors=11, rotate="promax")
print(fit)
simple<-fit$scores %>% as.data.frame()
colnames(simple)<-c('FinalBall', 'Shooting', 'Ballwinning',
                    'MFProg','LongPass','HighDef',
                    'RiskyPoss', 'Dribbling', 'Crossing', 
                    'TBs', 'CarryProg')

pcacordist<-get_dist(simple, method='spearman')
set.seed(200)
dtsne<-Rtsne::Rtsne(pcacordist, perplexity=50, is_dist=T)
plot(dtsne$Y)
restsne<-dtsne$Y %>% as.data.frame()

plot_clust_tendency(4:13, restsne)



resfcm<-fcm(restsne, 10, m=2)
plot_cluster(restsne, resfcm$cluster)
#plot_radars(resfcm$cluster, simple)

compare=c(1,8)
simple %>% mutate(clustnum=resfcm$cluster) %>%
  group_by(clustnum) %>% summarise_all(median) %>%
  filter(clustnum%in% compare) %>% View


clustprobs<-resfcm$u %>% as.data.frame %>%
  setNames(c('WideDef', 'Finisher', 'ProgDef',
             'DefMF','RoundedFinisher','ProgMF', 
             'ClumsyDef' ,'WideAttack','DribblyGuy',
             'Playmaker'
             )) %>%
  round(3) %>%
  mutate(Player=!!players$Player)
