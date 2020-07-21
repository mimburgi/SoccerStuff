raw<-read.csv("Top4andMLS-Whoscored.csv")

comparisonmetrics<-c("percSixYardBoxShots", "percOutofBoxShots",
                     "percCrossKP", "percTBKP", "percSetPieceShots",
                     "percCounterShots", "percSetPieceKP")

## packages ####
library(dplyr)
library(ggiraphExtra)
library(FactoMineR)
library(factoextra)
library(tibble)
## functions ####
compareTeam2Clust<-function(rawdata, kmeans, team){
  require(fmsb)
  require(dplyr)
  rawdata_df <- as.data.frame(rawdata) %>% rownames_to_column()
  cluster_pos <- as.data.frame(kmeans$cluster) %>% rownames_to_column()
  colnames(cluster_pos) <- c("rowname", "cluster")
  output <- inner_join(cluster_pos, rawdata_df)
  teamoutput <- subset(output, rowname==team)
  teamcluster<- teamoutput$cluster[1]
  teamoutput<-select(teamoutput, -rowname)
  
  clusteroutput <- subset(output, cluster==teamcluster)
  clusteroutput<-select(clusteroutput, -rowname)
  clusteroutput[nrow(clusteroutput+1),]<-
    colMeans(clusteroutput)
  clustermeans<-clusteroutput[nrow(clusteroutput),]
  gginput<-rbind(rep(3, ncol(teamoutput)),
                 rep(-3, ncol(teamoutput)),
                 teamoutput, clustermeans)
  gginput<-select(gginput, -cluster)
  rownames(gginput)<-c("max", "min", team, "cluster")
  return(radarchart(gginput))
}

compareMLSTeam<-function(mlsdata, mlsteam, otherdata, otherteam,
                         maxnum=3, minnum=-3, segments=4,
                         linealpha=.9, fillalpha=.2,
                         color1="tomato", color2="skyblue"){
  require(fmsb)
  require(dplyr)
  df1<- mlsdata[rownames(mlsdata)==mlsteam,c(1:(ncol(mlsdata)-1))]
  df2<- otherdata[rownames(otherdata)==otherteam,]
  gginput<-rbind(rep(maxnum, ncol(df1)),
                 rep(minnum, ncol(df1)),
                 df1, df2)
  rownames(gginput)<-c("max", "min", mlsteam, otherteam)
  colors_fill <- c(scales::alpha(color1, fillalpha),
                   scales::alpha(color2, fillalpha))
  
  # Define line colors
  colors_line <- c(scales::alpha(color1, linealpha),
                   scales::alpha(color2, linealpha))
  radarchart(gginput, seg=segments, pcol=colors_line, pfcol=colors_fill, plwd=2)
  legend(x=0.6, 
         y=1.35, 
         legend = rownames(gginput[-c(1,2),]), 
         bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
}


kmeans2radar<-function(rawdata, kmeans){
  rawdata_df <- as.data.frame(rawdata) %>% rownames_to_column()
  cluster_pos <- as.data.frame(kmeans$cluster) %>% rownames_to_column()
  colnames(cluster_pos) <- c("rowname", "cluster")
  output <- inner_join(cluster_pos, rawdata_df)
  output$cluster<-as.factor(output$cluster)
  ggRadar(output[-1], aes(group = cluster), rescale = TRUE, legend.position = "none", size = 1, interactive = FALSE, use.label = TRUE) + facet_wrap(~cluster) + scale_y_discrete(breaks = NULL) + # don't show ticks
    theme(axis.text.x = element_text(size = 10)) + scale_fill_manual(values = rep("#1c6193", nrow(output))) +
    scale_color_manual(values = rep("#1c6193", nrow(output)))
}

predict.kmeans <- function(object, newdata, method = c("centers", "classes")) {
  method <- match.arg(method)
  
  centers <- object$centers
  ss_by_center <- apply(centers, 1, function(x) {
    colSums((t(newdata) - x) ^ 2)
  })
  best_clusters <- apply(ss_by_center, 1, which.min)
  
  if (method == "centers") {
    centers[best_clusters, ]
  } else {
    best_clusters
  }
}

raw2radardata<-function(rawdata){
  tmpdata<-rawdata
  rmcols <- rev(seq(1,ncol(tmpdata))[!as.logical(sapply(tmpdata, is.numeric))])
  for (i in rmcols) tmpdata[[i]] <- NULL
  tmpdata[nrow(tmpdata)+1,]<-colMeans(tmpdata)
  return(tmpdata[nrow(tmpdata),])
}


## preproc ####
raw<-raw[,2:ncol(raw)]
library(dplyr)
teams<-raw$Team
raw<-select(raw, -(contains("Rating")))
raw<-select(raw, -(contains("R.", ignore.case = F)))
raw<-select(raw, -(contains("Team.")))
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
                 "League")
raw<-select(raw, -"R")
raw<-select(raw, -"ThrowinAss")
raw<-select(raw, -"TotalShots.2")
rownames(raw)<-raw$Team

actZones<-read.csv('Top4andMLS-actionzones.csv', stringsAsFactors = F)
for (team in rownames(raw)){
  raw$percOppThird[rownames(raw)==team]<-
    actZones$Opposition.Third[actZones$Team==team]
  raw$percMiddleThird[rownames(raw)==team]<-
    actZones$Middle.Third[actZones$Team==team]
}

attackSide<-read.csv('Top4andMLS-attacksides.csv',  stringsAsFactors = F)
for (team in rownames(raw)){
  raw$attackMiddle[rownames(raw)==team]<-
    attackSide$Middle.of.the.pitch[attackSide$Team==team]
}

raw$attackMiddle<-substr(raw$attackMiddle, 1, nchar(raw$attackMiddle)-1)
raw$attackMiddle<-as.numeric(raw$attackMiddle)

raw$percOppThird<-substr(raw$percOppThird, 1, nchar(raw$percOppThird)-1)
raw$percOppThird<-as.numeric(raw$percOppThird)

raw$percMiddleThird<-substr(raw$percMiddleThird, 1, nchar(raw$percMiddleThird)-1)
raw$percMiddleThird<-as.numeric(raw$percMiddleThird)

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

raw_extra$team<-rownames(raw_extra)
write.csv(raw_extra, 'MLSandTop4-whoscored-clean.csv',
          row.names = F)

## trim df ####

trim<-select(raw_extra, c(comparisonmetrics, "League"))







## compare radars ####

allTop4<-subset(trim, League!="MLS")
allTop4scaled<-as.data.frame(lapply(allTop4[sapply(allTop4, is.numeric)], scale))

# for (league in c("PL", "Bundesliga", "La Liga", "Serie A")){
#   leaguedat<-raw2radardata(allTop4scaled[allTop4$League==league,])
#   gginput<-rbind(rep(1, ncol(allTop4scaled)),
#                  rep(-1, ncol(allTop4scaled)),
#                  leaguedat)
#   rownames(gginput)<-c("max", "min", league)
#   radarchart(gginput, seg = 4, )
# }

for (league in c("PL", "Bundesliga", "La Liga", "Serie A")){
  leaguedat<-raw2radardata(allTop4scaled[allTop4$League==league,])
  colnames(leaguedat)<-c("Shots", "Wingplay", "KP", "Goals", "Assists",
                         "Aerial Duels", "Fouls Called", "Interceptions", 
                         "Tackles", "Successful Dribbles", "Midfield Battles")
  leaguedat$Wingplay<-leaguedat$Wingplay*-1
  plot(ggRadar(leaguedat, ylim=c(-1, 1), rescale=F))
}

leagueAvs<-raw2radardata(select(allTop4[allTop4$League=="PL",], -"League"))
leagueAvs$league="PL"
for (league in c("Bundesliga", "La Liga", "Serie A")){
  leaguedat<-raw2radardata(select(allTop4[allTop4$League==league,], -"League"))
  leagueAvs[nrow(leagueAvs)+1,]<-c(leaguedat[1,], league)
}

leagueAvs_scaled<-as.data.frame(lapply(leagueAvs[sapply(leagueAvs, is.numeric)], scale))
leagueAvs_scaled$league<-leagueAvs$league

allTop4scaled$League<-allTop4$League
ggRadar(as.data.frame(allTop4scaled), aes(group=League)) +
          facet_wrap(~League)






