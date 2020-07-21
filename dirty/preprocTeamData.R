raw<-read.csv("Top4andMLS-Whoscored.csv")

## packages ####
library(ggiraphExtra)
library(FactoMineR)
library(factoextra)
library(tibble)
library(dplyr)

## preproc whoscored ####
raw<-raw[,2:ncol(raw)]
library(dplyr)
teams<-raw$Team
raw<-dplyr::select(raw, -(contains("Rating")))
raw<-dplyr::select(raw, -(contains("R.", ignore.case = F)))
raw<-dplyr::select(raw, -(contains("Team.")))
colnames(raw)<-c("Team", "TotalTackles", "DribbledPast", "TotalAttemptedTackles", 
                 "R", "Interceptions", "Fouled", "Fouls", 
                 "Yellow", "Red",  "CaughtOffside", "Clearances", 
                 "ShotsBlocked", "CrossesBlocked", "PassesBlocked", "TotalSaves",  
                 "SixYardBoxSaves",   "PenaltyAreaSaves",  "OutOfBoxSaves",  "TotalShots",  
                 "OutOfBoxShots",   "SixYardBoxShots", "PenaltyAreaShots", "TotalGoals", 
                 "SixYardBoxGoals", "PenaltyAreaGoals", "OutOfBoxGoals",  "Unsuccessful", 
                 "Successful", "TotalDribbles", "UnsuccessfulTouches", "Dispossessed", 
                 "TotalAerial", "AerialWon",   "AerialLost", "TotalPasses",
                 "AccLB", "InAccLB",   "AccSP", "InAccSP",
                 "TotalKeyPasses","LongKeyPasses",  "ShortKeyPasses", "CrossAss", 
                 "CornerAss","ThroughballAss",  "FreekickAss","ThrowinAss",
                 "OtherAss", "TotalAss","TotalShots.2", "OpenPlayShots", 
                 "CounterShots", "SetPieceShots", "PenaltyShots", "CrossKP", 
                 "CornerKP", "ThroughballKP", "FreekickKP",  "ThrowinKP", "OtherKP",
                 "League", "SA", "Tackles.pg", "Ints.pg", "Fouls.pg", "Offsides",
                 "GF","Shots.pg2", "Discipline", "Possesion", "percPass", "AerialsWon",
                 "AccCrossPasses", "InaccCrossPasses", "AccCornerPAass", "InaccCornerPass", 
                 "AccFKPasses", "InaccFKPasses")
#raw<-dplyr::select(raw, -"R")
raw<-dplyr::select(raw, -"ThrowinAss")
raw<-dplyr::select(raw, -"TotalShots.2")
raw<-dplyr::select(raw, -"Shots.pg2")

raw$Team<-raw$Team

actZones<-read.csv('Top4andMLS-actionzones.csv', stringsAsFactors = F)
for (team in raw$Team){
  raw$percOppThird[raw$Team==team]<-
    actZones$Opposition.Third[actZones$Team==team]
  raw$percMiddleThird[raw$Team==team]<-
    actZones$Middle.Third[actZones$Team==team]
}

attackSide<-read.csv('Top4andMLS-attacksides.csv',  stringsAsFactors = F)
for (team in raw$Team){
  raw$attackMiddle[raw$Team==team]<-
    attackSide$Middle.of.the.pitch[attackSide$Team==team]
}

raw$attackMiddle<-substr(raw$attackMiddle, 1, nchar(raw$attackMiddle)-1)
raw$attackMiddle<-as.numeric(raw$attackMiddle)

raw$percOppThird<-substr(raw$percOppThird, 1, nchar(raw$percOppThird)-1)
raw$percOppThird<-as.numeric(raw$percOppThird)

raw$percMiddleThird<-substr(raw$percMiddleThird, 1, nchar(raw$percMiddleThird)-1)
raw$percMiddleThird<-as.numeric(raw$percMiddleThird)

## add in understat stuff####
usdat<-read.csv('understat.csv', stringsAsFactors = F)
colnames(usdat)[1:2]<-c('League', 'Year')
usdat<-subset(usdat, Year==2020)
usdat$team[usdat$team=="FC Cologne"]<-"FC Koln"
usdat$team[usdat$team=="Athletic Club"]<-"Athletic Bilbao"
usdat$team[usdat$team=="Alaves"]<-"Deportivo Alaves"


speed<-read.csv('understatspeed_processed.csv', stringsAsFactors = F)
speed$Team[speed$Team=="FC Cologne"]<-"FC Koln"
speed$Team[speed$Team=="Athletic Club"]<-"Athletic Bilbao"
speed$Team[speed$Team=="Alaves"]<-"Deportivo Alaves"


for (team in raw$Team[raw$League!="MLS"]){
  raw$npxG[raw$Team==team]<-
    usdat$npxG[usdat$team==team]
  raw$npxGA[raw$Team==team]<-
    usdat$npxGA[usdat$team==team]
  raw$ppda_coef[raw$Team==team]<-
    usdat$ppda_coef[usdat$team==team]
  raw$oppda_coef[raw$Team==team]<-
    usdat$oppda_coef[usdat$team==team]
  raw$deep[raw$Team==team]<-
    usdat$deep[usdat$team==team]
  raw$deep_allowed[raw$Team==team]<-
    usdat$deep_allowed[usdat$team==team]
  raw$xG_diff[raw$Team==team]<-
    usdat$xG_diff[usdat$team==team]
  raw$xGA_diff[raw$Team==team]<-
    usdat$xGA_diff[usdat$team==team]
  raw$xGA[raw$Team==team]<-
    usdat$xGA[usdat$team==team]
  raw$xG[raw$Team==team]<-
    usdat$xG[usdat$team==team]
  
  raw$FastSF[raw$Team==team]<-
    speed$FastSF[speed$Team==team]
  raw$FastSA[raw$Team==team]<-
    speed$FastSA[speed$Team==team]
  raw$FastGF[raw$Team==team]<-
    speed$FastGF[speed$Team==team]
  raw$FastGA[raw$Team==team]<-
    speed$FastGA[speed$Team==team]
  raw$FastxG[raw$Team==team]<-
    speed$FastxGF[speed$Team==team]
  raw$FastxGA[raw$Team==team]<-
    speed$FastxGA[speed$Team==team]
  raw$FastxGperShot[raw$Team==team]<-
    speed$FastxGFperShot[speed$Team==team]
  raw$FastxGAperShot[raw$Team==team]<-
    speed$FastxGAperShot[speed$Team==team]
  
  raw$NormalSF[raw$Team==team]<-
    speed$NormalSF[speed$Team==team]
  raw$NormalSA[raw$Team==team]<-
    speed$NormalSA[speed$Team==team]
  raw$NormalGF[raw$Team==team]<-
    speed$NormalGF[speed$Team==team]
  raw$NormalGA[raw$Team==team]<-
    speed$NormalGA[speed$Team==team]
  raw$NormalxG[raw$Team==team]<-
    speed$NormalxGF[speed$Team==team]
  raw$NormalxGA[raw$Team==team]<-
    speed$NormalxGA[speed$Team==team]
  raw$NormalxGperShot[raw$Team==team]<-
    speed$NormalxGFperShot[speed$Team==team]
  raw$NormalxGAperShot[raw$Team==team]<-
    speed$NormalxGAperShot[speed$Team==team]
  
  raw$StandardSF[raw$Team==team]<-
    speed$StandardSF[speed$Team==team]
  raw$StandardSA[raw$Team==team]<-
    speed$StandardSA[speed$Team==team]
  raw$StandardGF[raw$Team==team]<-
    speed$StandardGF[speed$Team==team]
  raw$StandardGA[raw$Team==team]<-
    speed$StandardGA[speed$Team==team]
  raw$StandardxG[raw$Team==team]<-
    speed$StandardxGF[speed$Team==team]
  raw$StandardxGA[raw$Team==team]<-
    speed$StandardxGA[speed$Team==team]
  raw$StandardxGperShot[raw$Team==team]<-
    speed$StandardxGFperShot[speed$Team==team]
  raw$StandardxGAperShot[raw$Team==team]<-
    speed$StandardxGAperShot[speed$Team==team]
  
  raw$SlowSF[raw$Team==team]<-
    speed$SlowSF[speed$Team==team]
  raw$SlowSA[raw$Team==team]<-
    speed$SlowSA[speed$Team==team]
  raw$SlowGF[raw$Team==team]<-
    speed$SlowGF[speed$Team==team]
  raw$SlowGA[raw$Team==team]<-
    speed$SlowGA[speed$Team==team]
  raw$SlowxG[raw$Team==team]<-
    speed$SlowxGF[speed$Team==team]
  raw$SlowxGA[raw$Team==team]<-
    speed$SlowxGA[speed$Team==team]
  raw$SlowxGperShot[raw$Team==team]<-
    speed$SlowxGFperShot[speed$Team==team]
  raw$SlowxGAperShot[raw$Team==team]<-
    speed$SlowxGAperShot[speed$Team==team]
}




## add in american soccer analysis stuff ####
asa<-read.csv('MLSxG.csv')

for (team in asa$Team){
  raw$xG[raw$Team==team]<-asa$xGF[asa$Team==team]
  raw$GF[raw$Team==team]<-asa$GF[asa$Team==team]
  raw$TotalGoals[raw$Team==team]<-asa$GF.g[asa$Team==team]
  raw$xG[raw$Team==team]<-asa$xGF[asa$Team==team]
  raw$xGA[raw$Team==team]<-asa$xGA[asa$Team==team]
}




#add in basic table info ####
top4table<-read.csv('top4tables.csv')
for (team in top4table$Team){
  raw$GP[raw$Team==team]<-top4table$P[top4table$Team==team]
  raw$GF[raw$Team==team]<-top4table$GF[top4table$Team==team]
  raw$GA[raw$Team==team]<-top4table$GA[top4table$Team==team]
}

MLStable<-read.csv('MLStable.csv')
for (team in MLStable$Team){
  raw$GP[raw$Team==team]<-MLStable$Pld[MLStable$Team==team]
  raw$GA[raw$Team==team]<-MLStable$GA[MLStable$Team==team]
}


## add in my own metrics####
raw_extra<-raw

raw_extra$percOutofBoxShots<-raw_extra$OutOfBoxShots/raw_extra$TotalShots
raw_extra$percSixYardBoxShots<-raw_extra$SixYardBoxShots/raw_extra$TotalShots
raw_extra$percPenaltyAreaShots<-raw_extra$PenaltyAreaShots/raw_extra$TotalShots

raw_extra$percOutofBoxGoals<-raw_extra$OutOfBoxGoals/raw_extra$TotalGoals
raw_extra$percSixYardBoxGoals<-raw_extra$SixYardBoxGoals/raw_extra$TotalGoals
raw_extra$percPenaltyAreaGoals<-raw_extra$PenaltyAreaGoals/raw_extra$TotalGoals

raw_extra$percAerial<-raw_extra$AerialWon/raw_extra$TotalAerial
raw_extra$TotalLB<-raw_extra$InAccLB + raw_extra$AccLB
raw_extra$accLB<-raw_extra$AccLB/raw_extra$TotalLB
raw_extra$percLB<-raw_extra$TotalLB/raw_extra$TotalPasses
raw_extra$TotalSP<-raw_extra$InAccSP + raw_extra$AccSP
raw_extra$accSP<-raw_extra$AccSP/raw_extra$TotalSP


raw_extra$percLongKeyPasses<-raw_extra$LongKeyPasses/raw_extra$TotalKeyPasses

raw_extra$percCrossAss<-raw_extra$CrossAss/raw_extra$TotalAss
raw_extra$percCornerAss<-raw_extra$CornerAss/raw_extra$TotalAss
raw_extra$percThroughballAss<-raw_extra$ThroughballAss/raw_extra$TotalAss
raw_extra$percFreekickAss<-raw_extra$FreekickAss/raw_extra$TotalAss
raw_extra$percSetPieceAss<-raw_extra$percFreekickAss + raw_extra$percCornerAss

raw_extra$percDribbles<-raw_extra$Successful/raw_extra$TotalDribbles

raw_extra$percSetPieceShots<-raw_extra$SetPieceShots/(raw_extra$SetPieceShots+raw_extra$OpenPlayShots)
raw_extra$percCounterShots<-raw_extra$CounterShots/(raw_extra$OpenPlayShots)
raw_extra$percLongKP<-raw_extra$LongKeyPasses/(raw_extra$TotalKeyPasses)

raw_extra$percTBKP<-raw_extra$ThroughballKP/(raw_extra$ThroughballKP+raw_extra$CrossKP+raw_extra$OtherKP)
raw_extra$percCrossKP<-raw_extra$CrossKP/(raw_extra$ThroughballKP+raw_extra$CrossKP+raw_extra$OtherKP)
raw_extra$percSetPieceKP<-(raw_extra$CornerKP + raw_extra$FreekickKP)/raw_extra$TotalKeyPasses

raw_extra$xGper<-raw_extra$xG/raw_extra$GP
raw_extra$xGdiffper<-(raw_extra$GF-raw_extra$xG)/raw_extra$GP
raw_extra$xGAper<-raw_extra$xGA/raw_extra$GP
raw_extra$xGAdiffper<-(raw_extra$GA-raw_extra$xGA)/raw_extra$GP


raw_extra$SuccessDA<-raw_extra$Tackles.pg + raw_extra$Interceptions

raw_extra$CounterDA<-raw_extra$CounterShots/raw_extra$SuccessDA


raw_extra$npXGAperShot<-raw_extra$npxGA/raw_extra$SA

raw_extra$xGshot<-raw_extra$xGper/raw_extra$TotalShots

raw_extra$pAdjTackles<-raw_extra$TotalTackles*(2/(1+exp(-.1*(raw_extra$Possesion-50))))
raw_extra$pAdjInts<-raw_extra$Interceptions*(2/(1+exp(-.1*(raw_extra$Possesion-50))))

raw_extra$pAdjClearances<-raw_extra$Clearances*(2/(1+exp(-.1*(raw_extra$Possesion-50))))


write.table(raw_extra, 'rawdata_full.txt', row.names = F)
