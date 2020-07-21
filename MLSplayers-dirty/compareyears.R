library(dplyr)
library(tidyr)

## read in and preproc 2018 data ####
chain<-read.csv('./2018/ASA_PlayerxGChain_per96table.csv', stringsAsFactors = F)
A3pass<-read.csv('./2018/ASApassingtable-attackthird.csv', stringsAsFactors = F)
M3pass<-read.csv('./2018/ASApassingtable-middlethird.csv', stringsAsFactors = F)
D3pass<-read.csv('./2018/ASApassingtable-defthird.csv', stringsAsFactors = F)
shoot<-read.csv('./2018/ASAshootertable.csv', stringsAsFactors = F)
totalpass<-read.csv('./2018/ASApassingtable-total.csv', stringsAsFactors = F)
chain<-subset(chain, Minutes > 1200)
A3pass<-subset(A3pass, Min > 1200)
M3pass<-subset(M3pass, Min > 1200)
D3pass<-subset(D3pass, Min > 1200)
shoot<-subset(shoot, Min > 1200)
totalpass<-subset(totalpass, Min > 1200)

teams<-chain$Team

#trim to only per 90s
A3pass<-select(A3pass, matches(".96|Player|Pos"))
M3pass<-select(M3pass, matches(".96|Player|Pos"))
D3pass<-select(D3pass, matches(".96|Player|Pos"))
shoot<-select(shoot, matches(".96|Player|Pos|Dist"))
chain<-select(chain, matches(".96|Player|Pos|Team|xB."))
totalpass<-select(totalpass, matches(".96|Player|Pos"))



allplayers<-c(chain$Player, A3pass$Player, M3pass$Player, D3pass$Player, shoot$Player, totalpass$Player)
allplayers<-unique(allplayers)

prev<-data.frame(Player=allplayers, 
                 Pos=as.character(rep(NA, length(allplayers))),
                 stringsAsFactors = F)
for (player in allplayers){
  
  #add Position and Minutes
  if(player %in% shoot$Player){
    #prev$InShooter[prev$Player==player]<-1
    prev$Pos[prev$Player==player]<-shoot$Pos[shoot$Player==player]
    prev$shots[prev$Player==player]<-shoot$Shots.96[shoot$Player==player]
    prev$KP[prev$Player==player]<-shoot$KeyP.96[shoot$Player==player]
    prev$xG[prev$Player==player]<-shoot$xG.96[shoot$Player==player]
    prev$xA[prev$Player==player]<-shoot$xA.96[shoot$Player==player]
    prev$xPlace[prev$Player==player]<-shoot$xPlace.96[shoot$Player==player]
    prev$ShotDist[prev$Player==player]<-shoot$Dist[shoot$Player==player]
    prev$KPDist[prev$Player==player]<-shoot$Dist.key[shoot$Player==player]
  }
  if(player %in% A3pass$Player){
    #prev$InA3[prev$Player==player]<-1
    prev$Pos[prev$Player==player]<-A3pass$Pos[A3pass$Player==player]
    prev$A3Passes[prev$Player==player]<-A3pass$Passes.96[A3pass$Player==player]
  }
  if(player %in% M3pass$Player){
    #prev$InM3[prev$Player==player]<-1
    prev$Pos[prev$Player==player]<-M3pass$Pos[M3pass$Player==player]
    prev$M3Passes[prev$Player==player]<-M3pass$Passes.96[M3pass$Player==player]
  }
  if(player %in% D3pass$Player){
    #prev$InD3[prev$Player==player]<-1
    prev$Pos[prev$Player==player]<-D3pass$Pos[D3pass$Player==player]
    prev$D3Passes[prev$Player==player]<-D3pass$Passes.96[D3pass$Player==player]
  }
  if(player %in% chain$Player){
    #prev$InChain[prev$Player==player]<-1
    prev$Pos[prev$Player==player]<-chain$Pos[chain$Player==player]
    prev$percChain[prev$Player==player]<-chain$TeamChain.[chain$Player==player]
    prev$xGChain[prev$Player==player]<-chain$xGChain.96[chain$Player==player]
    prev$xB[prev$Player==player]<-chain$xB.96[chain$Player==player]
    prev$Team[prev$Player==player]<-chain$Team[chain$Player==player]
    prev$ShotChainPerc[prev$Player==player]<-chain$PlayerShot.[chain$Player==player]
    prev$KPChainPerc[prev$Player==player]<-chain$PlayerKP.[chain$Player==player]
    prev$xBperc[prev$Player==player]<-chain$xB.[chain$Player==player]
    
  }
  if(player %in% totalpass$Player){
    prev$Vertical[prev$Player==player]<-totalpass$Vertical.96[totalpass$Player==player]
    prev$PassPct[prev$Player==player]<-totalpass$PassPct.96[totalpass$Player==player]
    prev$PassDistance[prev$Player==player]<-totalpass$Distance.96[totalpass$Player==player]
    #prev$TouchPerc[prev$Player==player]<-totalpass$Touch..96[totalpass$Player==player]
    prev$xPassPerc[prev$Player==player]<-totalpass$xPassPct.96[totalpass$Player==player]
    prev$Passes[prev$Player==player]<-totalpass$Passes.96[totalpass$Player==player]
  }
}

prev[is.na(prev)]<-0 #assuming missing vals mean zeros
prev<-subset(prev, Pos != "GK")


## read in and preproc 2019 data ####


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

teams<-chain$Team

#trim to only per 90s
A3pass<-select(A3pass, matches(".96|Player|Pos"))
M3pass<-select(M3pass, matches(".96|Player|Pos"))
D3pass<-select(D3pass, matches(".96|Player|Pos"))
shoot<-select(shoot, matches(".96|Player|Pos|Dist"))
chain<-select(chain, matches(".96|Player|Pos|Team|xB."))
totalpass<-select(totalpass, matches(".96|Player|Pos"))



allplayers<-c(chain$Player, A3pass$Player, M3pass$Player, D3pass$Player, shoot$Player, totalpass$Player)
allplayers<-unique(allplayers)

current<-data.frame(Player=allplayers, 
                Pos=as.character(rep(NA, length(allplayers))),
                stringsAsFactors = F)
for (player in allplayers){
  
  #add Position and Minutes
  if(player %in% shoot$Player){
    #current$InShooter[current$Player==player]<-1
    current$Pos[current$Player==player]<-shoot$Pos[shoot$Player==player]
    current$shots[current$Player==player]<-shoot$Shots.96[shoot$Player==player]
    current$KP[current$Player==player]<-shoot$KeyP.96[shoot$Player==player]
    current$xG[current$Player==player]<-shoot$xG.96[shoot$Player==player]
    current$xA[current$Player==player]<-shoot$xA.96[shoot$Player==player]
    current$xPlace[current$Player==player]<-shoot$xPlace.96[shoot$Player==player]
    current$ShotDist[current$Player==player]<-shoot$Dist[shoot$Player==player]
    current$KPDist[current$Player==player]<-shoot$Dist.key[shoot$Player==player]
  }
  if(player %in% A3pass$Player){
    #current$InA3[current$Player==player]<-1
    current$Pos[current$Player==player]<-A3pass$Pos[A3pass$Player==player]
    current$A3Passes[current$Player==player]<-A3pass$Passes.96[A3pass$Player==player]
  }
  if(player %in% M3pass$Player){
    #current$InM3[current$Player==player]<-1
    current$Pos[current$Player==player]<-M3pass$Pos[M3pass$Player==player]
    current$M3Passes[current$Player==player]<-M3pass$Passes.96[M3pass$Player==player]
  }
  if(player %in% D3pass$Player){
    #current$InD3[current$Player==player]<-1
    current$Pos[current$Player==player]<-D3pass$Pos[D3pass$Player==player]
    current$D3Passes[current$Player==player]<-D3pass$Passes.96[D3pass$Player==player]
  }
  if(player %in% chain$Player){
    #current$InChain[current$Player==player]<-1
    current$Pos[current$Player==player]<-chain$Pos[chain$Player==player]
    current$percChain[current$Player==player]<-chain$TeamChain.[chain$Player==player]
    current$xGChain[current$Player==player]<-chain$xGChain.96[chain$Player==player]
    current$xB[current$Player==player]<-chain$xB.96[chain$Player==player]
    current$Team[current$Player==player]<-chain$Team[chain$Player==player]
    current$ShotChainPerc[current$Player==player]<-chain$PlayerShot.[chain$Player==player]
    current$KPChainPerc[current$Player==player]<-chain$PlayerKP.[chain$Player==player]
    current$xBperc[current$Player==player]<-chain$xB.[chain$Player==player]
    
  }
  if(player %in% totalpass$Player){
    current$Vertical[current$Player==player]<-totalpass$Vertical.96[totalpass$Player==player]
    current$PassPct[current$Player==player]<-totalpass$PassPct.96[totalpass$Player==player]
    current$PassDistance[current$Player==player]<-totalpass$Distance.96[totalpass$Player==player]
    current$TouchPerc[current$Player==player]<-totalpass$Touch..96[totalpass$Player==player]
    current$xPassPerc[current$Player==player]<-totalpass$xPassPct.96[totalpass$Player==player]
    current$Passes[current$Player==player]<-totalpass$Passes.96[totalpass$Player==player]
  }
}

current[is.na(current)]<-0 #assuming missing vals mean zeros
current<-subset(current, Pos != "GK")


## trim both to only players within both ####
commonplayers<-intersect(prev$Player, current$Player)

prevtrim<-subset(prev, Player %in% commonplayers) %>% arrange(Player) %>% 
  select_if(is.numeric) %>% scale() %>% as.data.frame()
currenttrim<-subset(current, Player %in% commonplayers) %>% arrange(Player) %>% 
  select_if(is.numeric) %>% scale() %>% as.data.frame()

currenttrim<-select(currenttrim, -TouchPerc)

chg<-abs(prevtrim - currenttrim)
statchanges<-as.data.frame(colMeans(chg))
colnames(statchanges)<-'stat'
statchanges<-arrange(statchanges, stat)
