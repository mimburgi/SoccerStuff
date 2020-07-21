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

players$CrsPAperc<-players$CrsPA/players$Crs
teams$CrsPAperc<-teams$CrsPA/teams$Crs


usedvars<-c(
  'PressSucc', 'Press', 
            'Sh',
            'nsSCA',  
            'Crs',
            'TotalProg',
            'BallsWon')

for (player in 1:nrow(df)){
  for (var in usedvars){
    playerteam<-df$Squad[player]
    teamvar<-teams[[var]][teams$Squad==playerteam]
    df[[paste0('team', var)]][player]<-teamvar
    df[[paste0('tAdj', var)]][player]<-
      df[[var]][player]/df[[paste0('team', var)]]
  }
}

topprogs<-c()
for (team in teams$Squad){
  teamplayers<-subset(players, Squad==team) %>% arrange(TotalProg)
  topteamprogs<-teamplayers$Player[1:4]
  topprogs<-c(topprogs, topteamprogs)
}

topprogs
