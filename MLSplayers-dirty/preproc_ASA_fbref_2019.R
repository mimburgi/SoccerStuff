library(stringr)
library(dplyr)
## ASA data read in and preproc ####

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



## ASA player data ####
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




## ASA team data ####
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
    team1<-str_split(playerteam, ',')[[1]][1]
    team2<-str_split(playerteam, ',')[[1]][2]
    
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

## FBRef data read in and preproc ####
poss<-read.csv('./2019/FBRef-Possession.csv', stringsAsFactors = F)
passtype<-read.csv('./2019/FBRef-PassType.csv')
fbshoot<-read.csv('./2019/FBRef-Shoot.csv')
fbpass<-read.csv('./2019/FBRef-Pass.csv')
  
colnames(poss)[1]<-'Rank'
colnames(passtype)[1]<-'Rank'
colnames(fbshoot)[1]<-'Rank'
colnames(fbpass)[1]<-'Rank'


#reformat player names
poss<-tidyr::separate(poss, Player, into=c(NA, "Player"), sep='/') %>% 
  select(-c(Rank, Pos, Squad, Nation, Age, Born, Matches)) %>% 
  mutate(Player=gsub("-", " ", Player))
passtype<-tidyr::separate(passtype, Player, into=c(NA, "Player"), sep='/') %>% 
  select(-c(Rank, Pos, Squad, Nation, Age, Born, Matches)) %>% 
  mutate(Player=gsub("-", " ", Player))
fbshoot<-tidyr::separate(fbshoot, Player, into=c(NA, "Player"), sep='/') %>% 
  select(-c(Rank, Pos, Squad, Nation, Age, Born, Matches)) %>% 
  mutate(Player=gsub("-", " ", Player))
fbpass<-tidyr::separate(fbpass, Player, into=c(NA, "Player"), sep='/') %>% 
  select(-c(Rank, Pos, Squad, Nation, Age, Born, Matches)) %>% 
  mutate(Player=gsub("-", " ", Player))


#average together rows for two team players
twoplayers<-poss$Player[duplicated(poss$Player)]

oneposs<-poss[!(poss$Player %in% twoplayers),]
twoposs<-poss[(poss$Player %in% twoplayers),]
twopossaggs<-aggregate(.~Player, data = twoposs, FUN=mean)
poss<-rbind(oneposs, twopossaggs)


onepasstype<-passtype[!(passtype$Player %in% twoplayers),]
twopasstype<-passtype[(passtype$Player %in% twoplayers),]
twopasstypeaggs<-aggregate(.~Player, data = twopasstype, FUN=mean)
passtype<-rbind(onepasstype, twopasstypeaggs)
passtype<-passtype[passtype$Player!='Marquinhos Pedroso',]

onefbshoot<-fbshoot[!(fbshoot$Player %in% twoplayers),]
twofbshoot<-fbshoot[(fbshoot$Player %in% twoplayers),]
twofbshootaggs<-aggregate(.~Player, data = twofbshoot, FUN=mean)
fbshoot<-rbind(onefbshoot, twofbshootaggs)
fbshoot<-fbshoot[fbshoot$Player!='Marquinhos Pedroso',]

onefbpass<-fbpass[!(fbpass$Player %in% twoplayers),]
twofbpass<-fbpass[(fbpass$Player %in% twoplayers),]
twofbpassaggs<-aggregate(.~Player, data = twofbpass, FUN=mean)
fbpass<-rbind(onefbpass, twofbpassaggs)
fbpass<-fbpass[fbpass$Player!='Marquinhos Pedroso',]


noshotplayers<-passtype$Player[!(passtype$Player %in% fbshoot$Player)]
for (player in noshotplayers){
  fbshoot[nrow(fbshoot)+1,]<-
    c(player, rep(0, length(fbshoot)-1))
}

colnames(fbpass)<-paste0(colnames(fbpass), 'Pass')
colnames(fbpass)[1]<-'Player'


fbref<-cbind(poss %>% arrange(Player), 
             passtype %>% arrange(Player) %>% select(-Player), 
             fbshoot %>% arrange(Player) %>% select(-Player),
             fbpass %>% arrange(Player) %>% select(-Player))



#find any differences in spellings or anything
allplayers[!(allplayers %in% fbref$Player)]
#replace (is there a better way to do this?)
fbref_matchnames<-fbref 
fbref_matchnames$Player[fbref_matchnames$Player=="Mark Anthony Kaye"]<-"Mark-Anthony Kaye"
fbref_matchnames$Player[fbref_matchnames$Player=='Marky Delgado']<-'Marco Delgado'
fbref_matchnames$Player[fbref_matchnames$Player=='Jorge Luis Corrales']<-'Jorge Corrales'
fbref_matchnames$Player[fbref_matchnames$Player=='Kaku']<-'Alejandro Romero Gamarra'
fbref_matchnames$Player[fbref_matchnames$Player=='Gonzalo Nicolas Martinez']<-'Gonzalo Martinez'
fbref_matchnames$Player[fbref_matchnames$Player=='Boniek Garcia']<-'Oscar Boniek Garcia'
fbref_matchnames$Player[fbref_matchnames$Player=='Jhegson Mendez']<-'Sebastian Mendez'
fbref_matchnames$Player[fbref_matchnames$Player=='Adam Lundqvist']<-'Adam Lundkvist'
fbref_matchnames$Player[fbref_matchnames$Player=='Christopher Mueller']<-'Chris Mueller'
fbref_matchnames$Player[fbref_matchnames$Player=='Fabio Alvarez']<-'Favio Alvarez'
fbref_matchnames$Player[fbref_matchnames$Player=='Ray Gaddis']<-'Raymon Gaddis'
fbref_matchnames$Player[fbref_matchnames$Player=='Hwang In beom']<-'Hwang In-Beom'
fbref_matchnames$Player[fbref_matchnames$Player=='Michael Amir Murillo']<-'Michael Murillo'
fbref_matchnames$Player[fbref_matchnames$Player=='Kim Kee hee']<-'Kim Kee-Hee'
fbref_matchnames$Player[fbref_matchnames$Player=='Harold Santiago Mosquera']<-'Santiago Mosquera'
fbref_matchnames$Player[fbref_matchnames$Player=='Marcelo dos Santos Ferreira']<-'Marcelo'
fbref_matchnames$Player[fbref_matchnames$Player=='Ali Adnan Kadhim']<-'Ali Adnan'
fbref_matchnames$Player[fbref_matchnames$Player=='Antonio Delamea Mlinar']<-'Antonio Mlinar Delamea'
fbref_matchnames$Player[fbref_matchnames$Player=='Cristian Casseres']<-'Cristian Casseres Jr'
fbref_matchnames$Player[fbref_matchnames$Player=='Gerso Fernandes']<-'Gerso'
fbref_matchnames$Player[fbref_matchnames$Player=='Thomas McNamara']<-'Tommy McNamara'
fbref_matchnames$Player[fbref_matchnames$Player=='Steve Birnbaum']<-'Steven Birnbaum'
fbref_matchnames$Player[fbref_matchnames$Player=='Fafa Picault']<-'Fabrice-Jean Picault'
fbref_matchnames$Player[fbref_matchnames$Player=='Jake Nerwinski']<-'Jakob Nerwinski'
fbref_matchnames$Player[fbref_matchnames$Player=='CJ Sapong']<-'C.J. Sapong'
fbref_matchnames$Player[fbref_matchnames$Player=='Bradley Wright Phillips']<-'Bradley Wright-Phillips'
fbref_matchnames$Player[fbref_matchnames$Player=='Dom Dwyer']<-'Dominic Dwyer'
allplayers[!(allplayers %in% fbref_matchnames$Player)]


for (player in allplayers){
  dat$SuccDrib[dat$Player==player]<-fbref_matchnames$Succ[fbref_matchnames$Player==player]
  dat$AttDrib[dat$Player==player]<-fbref_matchnames$Att[fbref_matchnames$Player==player]
  dat$PlayerDrib[dat$Player==player]<-fbref_matchnames$X.Pl[fbref_matchnames$Player==player]
  dat$Carries[dat$Player==player]<-fbref_matchnames$Carries[fbref_matchnames$Player==player]
  dat$CarryPrgDis[dat$Player==player]<-fbref_matchnames$PrgDist[fbref_matchnames$Player==player]
  dat$TB[dat$Player==player]<-fbref_matchnames$TB[fbref_matchnames$Player==player]
  dat$PressPass[dat$Player==player]<-fbref_matchnames$Press[fbref_matchnames$Player==player]
  dat$SwPass[dat$Player==player]<-fbref_matchnames$Sw[fbref_matchnames$Player==player]
  dat$Crs[dat$Player==player]<-
    fbref_matchnames$Crs[fbref_matchnames$Player==player] - 
    fbref_matchnames$CK[fbref_matchnames$Player==player]
  dat$npxG[dat$Player==player]<-
    fbref_matchnames$npxG[fbref_matchnames$Player==player]
  dat$targeted[dat$Player==player]<-
    fbref_matchnames$Targ[fbref_matchnames$Player==player]
  dat$miscontrol[dat$Player==player]<-fbref_matchnames$Miscon[fbref_matchnames$Player==player]
  dat$passprg[dat$Player==player]<-fbref_matchnames$PrgDistPass[fbref_matchnames$Player==player]
  dat$penCrs[dat$Player==player]<-fbref_matchnames$CrsPAPass[fbref_matchnames$Player==player]
  dat$shortpasspct[dat$Player==player]<-
    fbref_matchnames$Att.1Pass[fbref_matchnames$Player==player]/
    fbref_matchnames$AttPass[fbref_matchnames$Player==player]
  dat$medpasspct[dat$Player==player]<-
    fbref_matchnames$Att.2Pass[fbref_matchnames$Player==player]/
    fbref_matchnames$AttPass[fbref_matchnames$Player==player]
  dat$longpasspct[dat$Player==player]<-
    fbref_matchnames$Att.3Pass[fbref_matchnames$Player==player]/
    fbref_matchnames$AttPass[fbref_matchnames$Player==player]
}


## write out ####
write.table(dat, '2019summary.txt', row.names = F)


