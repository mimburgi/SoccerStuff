#make dataframe containing team and player data from same years
filedf<-data.frame(
  playerfiles=c('top5players-upd.csv', 'top5players-py1.csv', 'top5players-py2.csv'),
  teamfiles=c('top5teams-upd.csv', 'top5teams-py1.csv', 'top5teams-py2.csv')
)
for (filepair in 1:nrow(filedf)){
  players<-read.csv(filedf$playerfiles[filepair])
  teams<-read.csv(filedf$teamfiles[filepair])
  
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
  
  #remove OGand ShBlkSav which convert incorrectly because some teams have zero
  tadjdf<-tadjdf %>% select(-c(tAdjOG, tAdjCrdR, tAdjBlockShSv))
  
}