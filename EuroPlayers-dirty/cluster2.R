#' source('../MLSplayers-dirty/soccer_util_fxns.R')
#' library(tidyverse)
#' library(ggplot2)
#' library(psych)
#' library(GPArotation)
#' library(FactoMineR)
#' 
#' players<-read.csv('../top5players.csv')
#' teams<-read.csv('../top5teams.csv')
#' 
#' ## team adjustments ####
#' players$TotalProg<-players$PassPrgDist + players$CarryPrgDist
#' teams$TotalProg<-teams$PassPrgDist + teams$CarryPrgDist
#' 
#' players$nsSCA<-players$SCADrib+players$SCAPassLive+players$SCAFld
#' teams$nsSCA<-teams$SCADrib+teams$SCAPassLive+teams$SCAFld
#' 
#' players<-filter(players, Min > 1000)%>%filter(Pos !='GK')
#' 
#' players$Crs<-players$Crs-players$PassCK
#' teams$Crs<-teams$Crs-teams$PassCK
#' 
#' players$BallsWon<-players$Int+players$TklW+players$PressSucc
#' teams$BallsWon<-teams$Int+teams$TklW+teams$PressSucc
#' 
#' 
#' 
#' usedvars<-c(
#'             'Sh',
#'             'KP',
#'             'CrsPA',
#'             'ProgPasses',
#'             'TouchesAttPen',
#'             'TouchesAtt3rd',
#'             'TouchesMid3rd',
#'             'TouchesDef3rd')
#' 
#' df<-players %>% select(c('Player', 'Squad', usedvars))
#' 
#' for (player in 1:nrow(df)){
#'   for (var in usedvars){
#'     playerteam<-df$Squad[player]
#'     teamvar<-teams[[var]][teams$Squad==playerteam]
#'     df[[paste0('team', var)]][player]<-teamvar
#'     df[[paste0('tAdj', var)]][player]<-
#'       df[[var]][player]/df[[paste0('team', var)]][player]
#'   }
#' }
#' 
#' df<-df %>%select(which(grepl(pattern = 'Player|Squad|tAdj', names(df))))
#' 
#' 
#' colnames(df)<-str_replace(colnames(df), "tAdj", "")
#' 
#' df$PassComp<-players$PassCmp.
#' df$KPperTouch<-players$KP/players$Touches
#' df$ShperTouch<-players$Sh/players$Touches
#' 
#' scaled<-df %>% select_if(is.numeric) %>% scale() %>% as.data.frame()
#' 
#' cordist<-factoextra::get_dist(scaled, method='spearman')
#' 
#' set.seed(200)
#' dtsne<-Rtsne::Rtsne(scaled, perplexity=170)
#' plot(dtsne$Y)
#' restsne<-dtsne$Y %>% as.data.frame()
#' 
#' library(dbscan)
#' resdb<-dbscan::dbscan(restsne, minPts = 120, eps=2.35)
#' plot_cluster(restsne, as.factor(resdb$cluster))
#' plot_radars(resdb$cluster, scaled)
#' 
#' basic_clusts(restsne, 4)
#' plot_cluster(restsne, clar4$clustering)
#' 
#' plot_radars(clar4$clustering, scaled)
#' 
#' clusts<-resdb$cluster %>% as.factor %>%
#'   recode("1" = 'Backfield', "2" = 'Attack', "3" = 'MF', "4" = 'Wide')
#' players$majorclust<-clusts
#' 
#' ## attacking dims ####
#' attvars<-c('Sh',
#'            'KP',
#'            #'ProgPasses', 
#'            #'DribAtt',
#'            'Touches',
#'            'PassPrgDist',
#'            'TouchesAttPen', 'TouchesMid3rd')
#' 
#' attdf<-players %>% filter(majorclust=='Attack') %>% select(c('Player', 'Squad', attvars))
#' 
#' # for (player in 1:nrow(attdf)){
#' #   for (var in attvars){
#' #     playerteam<-attdf$Squad[player]
#' #     teamvar<-teams[[var]][teams$Squad==playerteam]
#' #     attdf[[paste0('team', var)]][player]<-teamvar
#' #     attdf[[paste0('tAdj', var)]][player]<-players[[var]][player]/teamvar
#' #     
#' #     
#' #   }
#' # }
#' # 
#' # 
#' # attdf<-attdf %>%select(which(grepl(pattern = 'Player|Squad|tAdj', names(attdf))))
#' # 
#' # colnames(attdf)<-str_replace(colnames(attdf), "tAdj", "")
#' # 
#' attdf$PassComp<-players$PassCmp.[clusts=='Attack']
#' attdf$KPperTouch<-players$KP[clusts=='Attack']/players$Touches[clusts=='Attack']
#' attdf$ShperTouch<-players$Sh[clusts=='Attack']/players$Touches[clusts=='Attack']
#' 
#' attscaled<-attdf %>% select_if(is.numeric) %>% scale() %>% as.data.frame()
#' 
#' attcordist<-factoextra::get_dist(attscaled, 'spearman')
#' 
#' set.seed(500)
#' dtsne<-Rtsne::Rtsne(attcordist, perplexity=70)
#' plot(dtsne$Y)
#' restsne<-dtsne$Y %>% as.data.frame()
#' reshdb<-hdbscan(restsne, 50)
#' plot_cluster(restsne, as.factor(reshdb$cluster))
#' 
#' plot_radars(reshdb$cluster, attscaled)
#' 
#' resdb<-dbscan::dbscan(restsne, minPts=50, eps=2.7)
#' plot_cluster(restsne, as.factor(resdb$cluster))
#' plot_radars(resdb$cluster, attscaled)
#' 
#' basic_clusts(restsne, 2)
#' plot_cluster(restsne, clar2$clustering)
#' plot_radars(clar2$clustering, attscaled)
#' 
#' attplayers<-players %>% filter(majorclust=='Attack')
#' attplayers$Player[clar2$clustering==1]
#' 
#' attclusts<-clar2$clustering %>% as.factor() %>%
#'   recode("1" = 'Shooter', "2" = 'Creator')
#' 
#' attplayers$minorclust<-attclusts
#' players$minorclust[players$majorclust=='Attack']<-attclusts

#### MF ####
#' 
#' mfvars<-c('Sh', 'npxG',
#'            'KP', 'xA',
#'            'ProgPasses', 
#'            #'CarryPrgDist',
#'           'PassPrgDist',
#'            'Touches',
#'            'TouchesAtt3rd', 'TouchesDef3rd')
#' 
#' mfdf<-players %>% filter(majorclust=='MF') %>% select(c('Player', 'Squad', mfvars))
#' 
#' for (player in 1:nrow(mfdf)){
#'   for (var in mfvars){
#'     playerteam<-mfdf$Squad[player]
#'     teamvar<-teams[[var]][teams$Squad==playerteam]
#'     mfdf[[paste0('team', var)]][player]<-teamvar
#'     mfdf[[paste0('tAdj', var)]][player]<-players[[var]][player]/teamvar
#' 
#' 
#'   }
#' }
#' 
#' 
#' mfdf<-mfdf %>%select(which(grepl(pattern = 'Player|Squad|tAdj', names(mfdf))))
#' 
#' colnames(mfdf)<-str_replace(colnames(mfdf), "tAdj", "")
#' 
#' mfdf$PassComp<-players$PassCmp.[clusts=='MF']
#' 
#' mfscaled<-mfdf %>% select_if(is.numeric) %>% scale() %>% as.data.frame()
#' 
#' mfcordist<-factoextra::get_dist(mfscaled, 'spearman')
#' 
#' set.seed(50)
#' dtsne<-Rtsne::Rtsne(mfcordist, perplexity=18)
#' plot(dtsne$Y)
#' restsne<-dtsne$Y %>% as.data.frame()
#' reshdb<-hdbscan(restsne, 20)
#' plot_cluster(restsne, as.factor(reshdb$cluster))
#' 
#' resdb<-dbscan(restsne, minPts = 20, eps=4)
#' plot_cluster(restsne, resdb$cluster)
#' plot_radars(resdb$cluster, mfscaled)
#' 
#' mfplayers<-players %>% filter(majorclust=='MF')
#' mfplayers$Player[resdb$cluster==4]
#' 
#' mfclusts<-resdb$cluster %>% as.factor %>%
#'   recode("0" = "PatientProg", '1' = 'DeepProg', '2' = 'RiskyProg', '3'='AttackingMF', '4' = 'PatientProg')
#' mfplayers$minorclust=mfclusts
#' players$minorclust[players$majorclust=='MF']<-mfclusts

### WIDE ####

wideplayers<-players %>% filter(majorclust=='Wide')

widevars<-c('Sh', 'KP', 
            'PPA', 
            #'CrsPA',
            'ProgPasses', 'PassPrgDist', 'CarryPrgDist',
            #'DribAtt', 
            'TouchesAttPen', 'TouchesDef3rd', 'TouchesAtt3rd')
wide<-players %>% filter(majorclust=='Wide') %>% select(c('Player', 'Squad', widevars))

for (player in 1:nrow(wide)){
  for (var in widevars){
    playerteam<-wide$Squad[player]
    teamvar<-teams[[var]][teams$Squad==playerteam]
    wide[[paste0('team', var)]][player]<-teamvar
    wide[[paste0('tAdj', var)]][player]<-players[[var]][player]/teamvar


  }
}


wide<-wide %>%select(which(grepl(pattern = 'Player|Squad|tAdj', names(wide))))

colnames(wide)<-str_replace(colnames(wide), "tAdj", "")

wide$PassComp<-players$PassCmp.[clusts=='Wide']

widescaled<-wide %>% select_if(is.numeric) %>% scale() %>% as.data.frame()

widecordist<-factoextra::get_dist(widescaled, 'spearman')

set.seed(50)
dtsne<-Rtsne::Rtsne(widecordist, perplexity=12, is_dist=T)
plot(dtsne$Y)
restsne<-dtsne$Y %>% as.data.frame

reshdb<-hdbscan(restsne, 10)
plot_cluster(restsne, reshdb$cluster)
plot_radars(reshdb$cluster, widescaled)

wideplayers$Player[reshdb$cluster==2]
