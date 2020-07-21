source('../MLSplayers-dirty/soccer_util_fxns.R')
library(tidyverse)
library(ggplot2)
library(psych)
library(GPArotation)
library(FactoMineR)

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
players$BallsWon<-players$Int+players$TklW+players$PressSucc
teams$BallsWon<-teams$Int+teams$TklW+teams$PressSucc
players$KPperTouch<-players$KP/players$Touches
players$ShperTouch<-players$Sh/players$Touches

players$TouchperPass<-players$TouchesLive/players$PassLive
players$TouchperAction<-players$TouchesLive/(players$PassLive+players$Sh+players$Dispos)
players$percPassPress<-players$PassPress/players$PassLive

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

#remove the team- and original values, rename the tAdj values to original names
tadjdf<-tadjdf %>%select(which(grepl(pattern = 'Player|Squad|tAdj', names(tadjdf))))
colnames(tadjdf)<-str_replace(colnames(tadjdf), "tAdj", "")
# add the unadjusted variables back in
for (var in dontadj){ 
  tadjdf<-tadjdf %>% mutate(!!var:= !!players[[var]])
} 

#remove OG, red cards and ShBlkSav which converted incorrectly because some teams have zero
tadjdf<-tadjdf %>% select(-c(OG, CrdR, BlockShSv))

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
            'ProgPasses',
            'PassPrgDist',
            #'TouchperAction',
            'CarryPrgDist',
            'DribAtt', 'DribSucc',
            'TouchesAttPen',
            'TouchesAtt3rd',
            'TouchesMid3rd',
            'TouchesDef3rd',
            'PassCmp.')

library(factoextra)
cordisttadj<-tadjscaled %>% select(all_of(initvars)) %>% get_dist(method='spearman')
cordist<-scaled %>% select(all_of(initvars)) %>% get_dist(method='spearman')


set.seed(200)
dtsne<-Rtsne::Rtsne(cordist, perplexity=15, is_dist=T)
plot(dtsne$Y)
restsne<-dtsne$Y %>% as.data.frame()


library(ppclust)
resfcm<-fcm(restsne, centers=8, nstart=5, m=2.5)
plot_cluster(restsne, resfcm$cluster)
#plot_radars(resfcm$cluster, select(scaled, all_of(initvars)))

library(FactoMineR)
respca<-PCA(select(scaled, all_of(initvars)), scale.unit = F, ncp = 7, graph=F)
respca$var$cor %>% 
  promax 
library(psych)
library(GPArotation)
fit <- principal(select(scaled, all_of(initvars)), nfactors=7, rotate="promax")
print(fit)
simple<-fit$scores %>% as.data.frame()
colnames(simple)<-c('Finishing', 'FinalPass', 'Dribbling',
                    'MFProg','Crossing','D3Prog',
                    'BallRet')
plot_radars(resfcm$cluster, simple)

clustprobs<-resfcm$u %>% as.data.frame %>%
  setNames(c('BackfieldHold', 'MobileFinisher', 'PossFB',
             'MFProg','Finisher','DefProg',
             'Crosser', 'Creator'
             )) %>%
  round(3) %>%
  mutate(Player=!!players$Player)
