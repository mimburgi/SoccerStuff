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

scaled<-tadjdf %>% select_if(is.numeric) %>% scale() %>% as.data.frame()

cordist<-factoextra::get_dist(scaled, method='spearman')

## initial clustering of major roles ####

initvars<-c(
            'Sh',
            'ShperTouch',
            'KP',
            'KPperTouch',
            'CrsPA',
            'ProgPasses',
            'TouchesAttPen',
            'TouchesAtt3rd',
            'TouchesMid3rd',
            'TouchesDef3rd',
            'PassCmp.')

scaled<-tadjdf %>% select(all_of(initvars)) %>% scale %>% as.data.frame


set.seed(200)
dtsne<-Rtsne::Rtsne(scaled, perplexity=100)
plot(dtsne$Y)
restsne<-dtsne$Y %>% as.data.frame()

library(dbscan)
resdb<-dbscan::dbscan(restsne, minPts = 120, eps=3.27)
plot_cluster(restsne, as.factor(resdb$cluster))
plot_radars(resdb$cluster, scaled)

majorclusts<-resdb$cluster %>% as.factor %>%
  recode("0"='Tweener', "1" = 'Backfield', "2" = 'Attack', "3" = 'MF', "4" = 'Wide')
players$majorclust<-majorclusts

## attackers ####
attplayers<-filter(players, majorclusts=='Attack')

attvars<-c('Sh', 'npxG',
           'KP', 'xA',
           'ProgPasses',
           #'DribAtt',
           'Touches',
           'PassPrgDist',
           #'TouchesAttPen', 'TouchesMid3rd',
           'ShperTouch', 'KPperTouch',
           'CarryPrgDist')

attscaled<-tadjdf %>% select(all_of(attvars)) %>% scale %>% as.data.frame %>%
  filter(!!majorclusts=='Attack')

attcordist<-factoextra::get_dist(attscaled, 'spearman')

set.seed(500)
dtsne<-Rtsne::Rtsne(attcordist, perplexity=22)
plot(dtsne$Y)
restsne<-dtsne$Y %>% as.data.frame()

resdb<-dbscan::dbscan(restsne, minPts=30, eps=4)
plot_cluster(restsne, as.factor(resdb$cluster))
plot_radars(resdb$cluster, attscaled)

attplayers$Player[resdb$cluster==0]

attclusts<-clar2$clustering %>% as.factor() %>%
  recode("1" = 'Shooter', "2" = 'Creator')

attplayers$minorclust<-attclusts
players$minorclust[players$majorclust=='Attack']<-attclusts

#### MF ####
#' 
#' mfvars<-c(#'npxG',
#'            'xA',
#'            'ProgPasses',
#'            'CarryPrgDist',
#'           'PassPrgDist',
#'            #'TouchesLive',
#'           'PPA',
#'           #'PassF3',
#'           #'PassLive',
#'           #'TouchesAtt3rd', 
#'           'TouchesMid3rd','TouchesDef3rd',
#'           'TouchesAttPen')
#' 
#' mfplayers<-players %>% 
#'   filter(majorclust=='MF') %>% select(c('Player', 'Squad', mfvars))
#' 
#' mfdf<-players %>% select(c('Player', 'Squad', mfvars))
#' 
#' for (player in 1:nrow(mfdf)){
#'   for (var in mfvars){
#'     playerteam<-mfdf$Squad[player]
#'     teamvar<-teams[[var]][teams$Squad==playerteam]
#'     mfdf[[paste0('team', var)]][player]<-teamvar
#'     mfdf[[paste0('tAdj', var)]][player]<-players[[var]][player]/teamvar
#'   }
#' }
#' 
#' 
#' mfdf<-mfdf %>%
#'   select(which(grepl(pattern = 'Player|Squad|tAdj', names(mfdf))))
#' 
#' colnames(mfdf)<-str_replace(colnames(mfdf), "tAdj", "")

# mfdf$PassComp<-players$PassCmp.
# mfdf$TouchperPass<-players$TouchesLive/players$PassLive
# mfdf$percLongPass<-players$LPassAtt/players$PassAtt
# 
# mfscaled<-mfdf %>% select_if(is.numeric) %>%
#   scale() %>% as.data.frame() %>%
#   filter(!!clusts=='MF')
# 
# mfcordist<-factoextra::get_dist(mfscaled, 'spearman')
# 
# set.seed(50)
# dtsne<-Rtsne::Rtsne(mfcordist, perplexity=8, is_dist=T)
# plot(dtsne$Y)
# restsne<-dtsne$Y %>% as.data.frame()
# 
# basic_clusts(restsne, 2)
# 
# plot_cluster(restsne, clar2$cluster)
# 
# resdb<-dbscan::dbscan(restsne, minPts = 15, eps=7.5)
# plot_cluster(restsne, resdb$cluster)
# 
# plot_radars(resdb$cluster, mfscaled)

### WIDE ####
#' 
#' wideplayers<-players %>% filter(majorclust=='Wide')
#' 
#' widevars<-c('Sh', 'KP', 
#'             'PPA', 
#'             #'CrsPA',
#'             'ProgPasses', 'PassPrgDist', 'CarryPrgDist',
#'             #'DribAtt', 
#'             'TouchesAttPen', 'TouchesDef3rd', 'TouchesAtt3rd')
#' 
#' wide<-players %>% filter(majorclust=='Wide') %>% select(c('Player', 'Squad', widevars))
#' 
#' for (player in 1:nrow(wide)){
#'   for (var in widevars){
#'     playerteam<-wide$Squad[player]
#'     teamvar<-teams[[var]][teams$Squad==playerteam]
#'     wide[[paste0('team', var)]][player]<-teamvar
#'     wide[[paste0('tAdj', var)]][player]<-players[[var]][player]/teamvar
#' 
#' 
#'   }
#' }
#' 
#' 
#' wide<-wide %>%select(which(grepl(pattern = 'Player|Squad|tAdj', names(wide))))
#' 
#' colnames(wide)<-str_replace(colnames(wide), "tAdj", "")
#' 
#' wide$PassComp<-players$PassCmp.[clusts=='Wide']
#' 
#' widescaled<-wide %>% select_if(is.numeric) %>% scale() %>% as.data.frame()
#' 
#' widecordist<-factoextra::get_dist(widescaled, 'spearman')
#' 
#' set.seed(50)
#' dtsne<-Rtsne::Rtsne(widecordist, perplexity=5, is_dist=T)
#' plot(dtsne$Y)
#' restsne<-dtsne$Y %>% as.data.frame
#' 
#' reshdb<-hdbscan(restsne, 10)
#' plot_cluster(restsne, reshdb$cluster)
#' plot_radars(reshdb$cluster, widescaled)
#' 
#' wideplayers$Player[reshdb$cluster==2]
