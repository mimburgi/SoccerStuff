raw<-read.csv("PLandMLS-Whoscored.csv")

comparisonmetrics<-c("percOutofBoxShots", "percSixYardBoxShots", 
                     "percPenaltyAreaShots", "percSetPieceShots", 
                     "percCounterShots","TotalShots",
                     "percTBKP", "percCrossKP", "TotalKeyPasses",
                     "Dispossessed", "TotalDribbles",
                     "percOppThird", "attackMiddle")

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
                 "CornerKP", "ThroughballKP", "FreekickKP", "OtherKP", 
                 "League")
raw<-select(raw, -"R")
raw<-select(raw, -"ThrowinAss")
raw<-select(raw, -"TotalShots.2")
rownames(raw)<-raw$Team


offense<-raw[,c("TotalShots", "OutOfBoxShots" ,        "SixYardBoxShots"  ,     "PenaltyAreaShots" ,    
                "TotalGoals"     ,       "SixYardBoxGoals"   ,    "PenaltyAreaGoals"  ,   
                "OutOfBoxGoals"   ,      "Unsuccessful"       ,   "Successful"         ,  
                "TotalDribbles"    ,     "UnsuccessfulTouches" ,  "Dispossessed"        , 
                "TotalAerial"       ,    "AerialWon"          ,   "AerialLost"           ,
                "TotalPasses"        ,   "AccLB"          ,       "InAccLB"              ,
                "AccSP"               ,  "InAccSP"         ,      "TotalKeyPasses"       ,
                "LongKeyPasses"     ,    "ShortKeyPasses"   ,     "CrossAss"             ,
                "CornerAss"          ,   "ThroughballAss"    ,    "FreekickAss"          ,
                "TotalAss", "OpenPlayShots", 
                "CounterShots", "SetPieceShots", "PenaltyShots", "CrossKP", 
                "CornerKP", "ThroughballKP", "FreekickKP", "OtherKP", 
                "League")]



actZones<-read.csv('PLandMLS-actionzones.csv', stringsAsFactors = F)
for (team in rownames(offense)){
  offense$percOppThird[rownames(offense)==team]<-
    actZones$Opposition.Third[actZones$Team==team]
}

attackSide<-read.csv('PLandMLS-attacksides.csv',  stringsAsFactors = F)
for (team in rownames(offense)){
  offense$attackMiddle[rownames(offense)==team]<-
    attackSide$Middle.of.the.pitch[actZones$Team==team]
}

offense$attackMiddle<-substr(offense$attackMiddle, 1, nchar(offense$attackMiddle)-1)
offense$attackMiddle<-as.numeric(offense$attackMiddle)

offense$percOppThird<-substr(offense$percOppThird, 1, nchar(offense$percOppThird)-1)
offense$percOppThird<-as.numeric(offense$percOppThird)

## add in my own metrics####
offense_extra<-offense

offense_extra$percOutofBoxShots<-offense_extra$OutOfBoxShots/offense_extra$TotalShots
offense_extra$percSixYardBoxShots<-offense_extra$SixYardBoxShots/offense_extra$TotalShots
offense_extra$percPenaltyAreaShots<-offense_extra$PenaltyAreaShots/offense_extra$TotalShots

offense_extra$percOutofBoxGoals<-offense_extra$OutOfBoxGoals/offense_extra$TotalGoals
offense_extra$percSixYardBoxGoals<-offense_extra$SixYardBoxGoals/offense_extra$TotalGoals
offense_extra$percPenaltyAreaGoals<-offense_extra$PenaltyAreaGoals/offense_extra$TotalGoals

offense_extra$percAerial<-offense_extra$AerialWon/offense_extra$TotalAerial
offense_extra$TotalLB<-offense_extra$InAccLB + offense_extra$AccLB
offense_extra$accLB<-offense_extra$AccLB/offense_extra$TotalLB
offense_extra$percLB<-offense_extra$TotalLB/offense_extra$TotalPasses
offense_extra$TotalSP<-offense_extra$InAccSP + offense_extra$AccSP
offense_extra$accSP<-offense_extra$AccSP/offense_extra$TotalSP


offense_extra$percLongKeyPasses<-offense_extra$LongKeyPasses/offense_extra$TotalKeyPasses

offense_extra$percCrossAss<-offense_extra$CrossAss/offense_extra$TotalAss
offense_extra$percCornerAss<-offense_extra$CornerAss/offense_extra$TotalAss
offense_extra$percThroughballAss<-offense_extra$ThroughballAss/offense_extra$TotalAss
offense_extra$percFreekickAss<-offense_extra$FreekickAss/offense_extra$TotalAss
offense_extra$percSetPieceAss<-offense_extra$percFreekickAss + offense_extra$percCornerAss

offense_extra$percDribbles<-offense_extra$Successful/offense_extra$TotalDribbles

offense_extra$percSetPieceShots<-offense_extra$SetPieceShots/(offense_extra$SetPieceShots+offense_extra$OpenPlayShots)
offense_extra$percCounterShots<-offense_extra$CounterShots/(offense_extra$OpenPlayShots)
offense_extra$percLongKP<-offense_extra$LongKeyPasses/(offense_extra$TotalKeyPasses)

offense_extra$percTBKP<-offense_extra$ThroughballKP/(offense_extra$ThroughballKP+offense_extra$CrossKP+offense_extra$OtherKP)
offense_extra$percCrossKP<-offense_extra$CrossKP/(offense_extra$ThroughballKP+offense_extra$CrossKP+offense_extra$OtherKP)
offense_extra$percSetPieceKP<-(offense_extra$CornerKP + offense_extra$FreekickKP)/offense_extra$TotalKeyPasses




## trim df ####
offense_totals<-offense_extra[,c("TotalShots","TotalGoals","TotalDribbles","TotalAerial","TotalPasses","TotalKeyPasses","TotalAss")] 
offense_percs<-select(offense_extra, contains("perc"))

otrimraw<-select(offense_extra, c(comparisonmetrics, "League"))
#scale each league independently
pltrim<-subset(otrimraw, League=="PL")
plscale<-scale(select(pltrim, -League))

mlstrim<-subset(otrimraw, League=="MLS")
mlsscale<-scale(select(mlstrim, -League))

otrimscale<-rbind(plscale, mlsscale)

## kmeans ####
decomp4<-kmeans(otrimscale, 4, nstart=50)
fviz_cluster(decomp4, data = otrimscale)

decomp5<-kmeans(otrimscale, 5, nstart=50)
fviz_cluster(decomp5, data = otrimscale)

decomp6<-kmeans(otrimscale, 6, nstart=50)
fviz_cluster(decomp6, data = otrimscale)

#6 looks reasonable



## vis ####
kmeans2radar(otrimscale, decomp6)

compareTeamRadar(otrimscale, decomp6, "Liverpool")


## try classifying mls based on pl teams ####
plteams <- kmeans(plscale, 19, nstart=50)
plclusters<-as.data.frame(plteams$cluster)
colnames(plclusters)<-"cluster"


mlspreds<-predict.kmeans(object=plteams, newdata=mlsscale)
mlspreds<-as.data.frame(mlspreds)
mlspreds$team<-rownames(mlsscale)
mlspreds$cluster<-rownames(mlspreds)
mlspreds$cluster<-sub(".", "", mlspreds$cluster) #remove X that gets added
mlspreds$cluster<-sapply(strsplit(mlspreds$cluster, "\\."), `[`, 1)

mlscompare<-as.data.frame(mlsscale)

for (team in rownames(mlscompare)){
  clustnum<-mlspreds$cluster[mlspreds$team==team]
  plteam<-rownames(plclusters)[plclusters$cluster==clustnum]
  mlscompare$plcomparison[rownames(mlscompare)==team]<-plteam
}

pldat<-as.data.frame(plscale)

compareMLSTeam(mlscompare, "Los Angeles FC", pldat, "Chelsea")



