library(dplyr)

chain<-read.csv('ASA_PlayerxGChain_per96table.csv', stringsAsFactors = F)
A3pass<-read.csv('ASApassingtable-attackthird.csv', stringsAsFactors = F)
M3pass<-read.csv('ASApassingtable-middlethird.csv', stringsAsFactors = F)
D3pass<-read.csv('ASApassingtable-defthird.csv', stringsAsFactors = F)
shoot<-read.csv('ASAshootertable.csv', stringsAsFactors = F)
chain<-subset(chain, Minutes > 1200)
A3pass<-subset(A3pass, Min > 1200)
M3pass<-subset(M3pass, Min > 1200)
D3pass<-subset(D3pass, Min > 1200)
shoot<-subset(shoot, Min > 1200)

#trim to only per 90s
A3pass<-select(A3pass, matches(".96|Player|Pos"))
M3pass<-select(M3pass, matches(".96|Player|Pos"))
D3pass<-select(D3pass, matches(".96|Player|Pos"))
shoot<-select(shoot, matches(".96|Player|Pos"))
chain<-select(chain, matches(".96|Player|Pos"))



allplayers<-c(chain$Player, A3pass$Player, M3pass$Player, D3pass$Player, shoot$Player)
allplayers<-unique(allplayers)

dat<-data.frame(Player=allplayers, 
                Min=as.numeric(rep(NA, length(allplayers))),
                Pos=as.character(rep(NA, length(allplayers))),
                InShooter=as.numeric(rep(NA, length(allplayers))),
                InA3=as.numeric(rep(NA, length(allplayers))),
                InM3=as.numeric(rep(NA, length(allplayers))),
                InD3=as.numeric(rep(NA, length(allplayers))),
                InChain=as.numeric(rep(NA, length(allplayers))),
                stringsAsFactors = F)
for (player in allplayers){
  
  #add Position and Minutes
  if(player %in% shoot$Player){
    dat$InShooter[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-shoot$Pos[shoot$Player==player]
    dat$shots[dat$Player==player]<-shoot$Shots.96[shoot$Player==player]
    dat$KP[dat$Player==player]<-shoot$KeyP.96[shoot$Player==player]
  }
  if(player %in% A3pass$Player){
    dat$InA3[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-A3pass$Pos[A3pass$Player==player]
    dat$A3Passes[dat$Player==player]<-A3pass$Passes.96[A3pass$Player==player]
  }
  if(player %in% M3pass$Player){
    dat$InM3[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-M3pass$Pos[M3pass$Player==player]
    dat$M3Passes[dat$Player==player]<-M3pass$Passes.96[M3pass$Player==player]
  }
  if(player %in% D3pass$Player){
    dat$InD3[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-D3pass$Pos[D3pass$Player==player]
    dat$D3Passes[dat$Player==player]<-D3pass$Passes.96[D3pass$Player==player]
  }
  if(player %in% chain$Player){
    dat$InChain[dat$Player==player]<-1
    dat$Pos[dat$Player==player]<-chain$Pos[chain$Player==player]
    dat$NumChains[dat$Player==player]<-chain$NumChains.96[chain$Player==player]
  }
}

dat[is.na(dat)]<-0 #assuming missing vals mean zeros

scaled<-scale(dat[is.numeric(dat)])

library()