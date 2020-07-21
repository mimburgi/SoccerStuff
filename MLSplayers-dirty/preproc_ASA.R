library(stringr)
library(dplyr)
## data read in and preproc ####

chain<-read.csv('./2019/ASA_PlayerxGChain_per96table.csv', stringsAsFactors = F)
A3pass<-read.csv('./2019/ASApassingtable-attackthird.csv', stringsAsFactors = F)
M3pass<-read.csv('./2019/ASApassingtable-middlethird.csv', stringsAsFactors = F)
D3pass<-read.csv('./2019/ASApassingtable-defthird.csv', stringsAsFactors = F)
shoot<-read.csv('./2019/ASAshootertable.csv', stringsAsFactors = F)
totalpass<-read.csv('./2019/ASApassingtable-total.csv', stringsAsFactors = F)
chain<-subset(chain, Minutes > 1200)
A3pass<-subset(A3pass, Min > 1200)
M3pass<-subset(M3pass, Min > 1200)
D3pass<-subset(D3pass, Min > 1200)
shoot<-subset(shoot, Min > 1200)
totalpass<-subset(totalpass, Min > 1200)

TeamA3pass<-read.csv('./2019/ASAteampassingtable_A3.csv', stringsAsFactors = F)
TeamM3pass<-read.csv('./2019/ASAteampassingtable_M3.csv', stringsAsFactors = F)
TeamD3pass<-read.csv('./2019/ASAteampassingtable_D3.csv', stringsAsFactors = F)
Teamshoot<-read.csv('./2019/ASAteamshooter.csv', stringsAsFactors = F)
Teamtotalpass<-read.csv('./2019/ASAteampassingtable_total.csv', stringsAsFactors = F)



## player data ####
A3pass<-select(A3pass, matches(".96|Player|Pos"))
M3pass<-select(M3pass, matches(".96|Player|Pos"))
D3pass<-select(D3pass, matches(".96|Player|Pos"))
shoot<-select(shoot, matches(".96|Player|Pos|Dist"))
chain<-select(chain, matches(".96|Player|Pos|Team|xB."))
totalpass<-select(totalpass, matches(".96|Player|Pos|Score"))



allplayers<-c(chain$Player, A3pass$Player, M3pass$Player, D3pass$Player, shoot$Player, totalpass$Player)
allplayers<-unique(allplayers)

dat<-data.frame(Player=allplayers, 
                Pos=as.character(rep(NA, length(allplayers))),
                stringsAsFactors = F)
for (player in allplayers){
  
  #add Position and Minutes
  if(player %in% shoot$Player){
    #dat$InShooter[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-shoot$Pos[shoot$Player==player]
    dat$shots[dat$Player==player]<-shoot$Shots.96[shoot$Player==player]
    dat$KP[dat$Player==player]<-shoot$KeyP.96[shoot$Player==player]
    dat$xG[dat$Player==player]<-shoot$xG.96[shoot$Player==player]
    dat$xA[dat$Player==player]<-shoot$xA.96[shoot$Player==player]
    dat$xPlace[dat$Player==player]<-shoot$xPlace.96[shoot$Player==player]
    dat$ShotDist[dat$Player==player]<-shoot$Dist[shoot$Player==player]
    dat$KPDist[dat$Player==player]<-shoot$Dist.key[shoot$Player==player]
  }
  if(player %in% A3pass$Player){
    #dat$InA3[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-A3pass$Pos[A3pass$Player==player]
    dat$A3Passes[dat$Player==player]<-A3pass$Passes.96[A3pass$Player==player]
  }
  if(player %in% M3pass$Player){
    #dat$InM3[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-M3pass$Pos[M3pass$Player==player]
    dat$M3Passes[dat$Player==player]<-M3pass$Passes.96[M3pass$Player==player]
  }
  if(player %in% D3pass$Player){
    #dat$InD3[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-D3pass$Pos[D3pass$Player==player]
    dat$D3Passes[dat$Player==player]<-D3pass$Passes.96[D3pass$Player==player]
  }
  if(player %in% chain$Player){
    #dat$InChain[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-chain$Pos[chain$Player==player]
    dat$percChain[dat$Player==player]<-chain$TeamChain.[chain$Player==player]
    dat$xGChain[dat$Player==player]<-chain$xGChain.96[chain$Player==player]
    dat$xB[dat$Player==player]<-chain$xB.96[chain$Player==player]
    dat$Team[dat$Player==player]<-chain$Team[chain$Player==player]
    dat$ShotChainPerc[dat$Player==player]<-chain$PlayerShot.[chain$Player==player]
    dat$KPChainPerc[dat$Player==player]<-chain$PlayerKP.[chain$Player==player]
    dat$xBperc[dat$Player==player]<-chain$xB.[chain$Player==player]
    
  }
  if(player %in% totalpass$Player){
    dat$Vertical[dat$Player==player]<-totalpass$Vertical.96[totalpass$Player==player]
    dat$PassPct[dat$Player==player]<-totalpass$PassPct.96[totalpass$Player==player]
    dat$PassDistance[dat$Player==player]<-totalpass$Distance.96[totalpass$Player==player]
    #dat$TouchPerc[dat$Player==player]<-totalpass$Touch..96[totalpass$Player==player]
    dat$xPassPerc[dat$Player==player]<-totalpass$xPassPct.96[totalpass$Player==player]
    dat$Passes[dat$Player==player]<-totalpass$Passes.96[totalpass$Player==player]
    dat$PassScore[dat$Player==player]<-totalpass$Score.96[totalpass$Player==player]
  }
}

dat[is.na(dat)]<-0 #assuming missing vals mean zeros
dat<-subset(dat, Pos != "GK")

dat$A3perc<-dat$A3Passes/dat$Passes
dat$M3perc<-dat$M3Passes/dat$Passes
dat$D3perc<-dat$D3Passes/dat$Passes

dat$xGper<-dat$xG/dat$shots
dat$xAper<-dat$xA/dat$KP

dat[is.na(dat)]<-0 #assuming missing vals mean zeros


## team data ####
for (player in 1:nrow(dat)){
  playerteam<-dat$Team[player]
  
  if (!str_detect(playerteam, ',')){ #single team all season
    dat$teamA3pass[player]<-
      TeamA3pass$PassF[TeamA3pass$Team==playerteam]
    dat$teamM3pass[player]<-
      TeamM3pass$PassF[TeamM3pass$Team==playerteam]
    dat$teamD3pass[player]<-
      TeamD3pass$PassF[TeamD3pass$Team==playerteam]
    dat$teampass[player]<-
      Teamtotalpass$PassF[Teamtotalpass$Team==playerteam]
    dat$teamxG[player]<-
      Teamshoot$xGF[Teamshoot$Team==playerteam]
    dat$teamshots[player]<-
      Teamshoot$ShtF[Teamshoot$Team==playerteam]
  }#end single team if
  else{
    team1<-str_split(test, ',')[[1]][1]
    team2<-str_split(test, ',')[[1]][2]
    
    dat$teamA3pass[player]<-
    mean(TeamA3pass$PassF[TeamA3pass$Team==team1],
           TeamA3pass$PassF[TeamA3pass$Team==team2])
    dat$teamM3pass[player]<-
      mean(TeamM3pass$PassF[TeamM3pass$Team==team1],
           TeamM3pass$PassF[TeamM3pass$Team==team2])
    dat$teamD3pass[player]<-
      mean(TeamD3pass$PassF[TeamD3pass$Team==team1],
           TeamD3pass$PassF[TeamD3pass$Team==team2])
    dat$teampass[player]<-
      mean(Teamtotalpass$PassF[Teamtotalpass$Team==team1],
           Teamtotalpass$PassF[Teamtotalpass$Team==team2])
    dat$teamxG[player]<-
      mean(Teamshoot$xGF[Teamshoot$Team==team1],
           Teamshoot$xGF[Teamshoot$Team==team2])
    dat$teamshots[player]<-
      mean(Teamshoot$ShtF[Teamshoot$Team==team1],
           Teamshoot$ShtF[Teamshoot$Team==team2])

  }#end two teams if
  
}

dat$A3teamperc<-dat$A3Passes/dat$teamA3pass
dat$M3teamperc<-dat$M3Passes/dat$teamM3pass
dat$D3teamperc<-dat$D3Passes/dat$teamD3pass
dat$passteamperc<-dat$Passes/dat$teampass
dat$xGteamperc<-dat$Passes/dat$teamxG
dat$shotteamperc<-dat$Passes/dat$teamshots

## write out ####
write.table(dat, '2019summary.txt', row.names = F)


