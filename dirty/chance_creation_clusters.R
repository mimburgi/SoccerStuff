raw<-read.csv("Top4andMLS-Whoscored.csv")

comparisonmetrics<-c("npxG", "deep","ppda_coef",
                     "xG_diff", "CounterShots", 
                     "xGA", "oppda_coef", "xGA_diff", "SA")

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
  teamoutput<-dplyr::select(teamoutput, -rowname)
  
  clusteroutput <- subset(output, cluster==teamcluster)
  clusteroutput<-dplyr::select(clusteroutput, -rowname)
  clusteroutput[nrow(clusteroutput+1),]<-
    colMeans(clusteroutput)
  clustermeans<-clusteroutput[nrow(clusteroutput),]
  gginput<-rbind(rep(3, ncol(teamoutput)),
                 rep(-3, ncol(teamoutput)),
                 teamoutput, clustermeans)
  gginput<-dplyr::select(gginput, -cluster)
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
                 "League", "SA", "Tackles.pg", "Ints.pg", "Fouls.pg", "Offsides")
raw<-dplyr::select(raw, -"R")
raw<-dplyr::select(raw, -"ThrowinAss")
raw<-dplyr::select(raw, -"TotalShots.2")
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


raw_extra$npXGAperShot<-raw_extra$npxGA/raw_extra$SA


## add in understat stuff####
usdat<-read.csv('understat.csv', stringsAsFactors = F)
colnames(usdat)[1:2]<-c('League', 'Year')
usdat<-subset(usdat, Year==2020)
usdat$team[usdat$team=="FC Cologne"]<-"FC Koln"
usdat$team[usdat$team=="Athletic Club"]<-"Athletic Bilbao"
usdat$team[usdat$team=="Alaves"]<-"Deportivo Alaves"


for (team in rownames(raw_extra[raw_extra$League!="MLS",])){
  raw_extra$npxG[rownames(raw_extra)==team]<-
    usdat$npxG[usdat$team==team]
  raw_extra$npxGA[rownames(raw_extra)==team]<-
    usdat$npxGA[usdat$team==team]
  raw_extra$ppda_coef[rownames(raw_extra)==team]<-
    usdat$ppda_coef[usdat$team==team]
  raw_extra$oppda_coef[rownames(raw_extra)==team]<-
    usdat$oppda_coef[usdat$team==team]
  raw_extra$deep[rownames(raw_extra)==team]<-
    usdat$deep[usdat$team==team]
  raw_extra$deep_allowed[rownames(raw_extra)==team]<-
    usdat$deep_allowed[usdat$team==team]
  raw_extra$xG_diff[rownames(raw_extra)==team]<-
    usdat$xG_diff[usdat$team==team]
  raw_extra$xGA_diff[rownames(raw_extra)==team]<-
    usdat$xGA_diff[usdat$team==team]
  raw_extra$xGA[rownames(raw_extra)==team]<-
    usdat$xGA[usdat$team==team]
}

## trim df ####

KPmeasures<-c("OtherKP", "CrossKP", "ThroughballKP", "LongKeyPasses", "ShortKeyPasses")
for (measure in KPmeasures){
  colname=paste0(measure, "_scaled")
  raw_extra<-mutate(raw_extra, !!colname := !!scale(raw_extra[[measure]]))
}

write.csv(raw_extra, 'top4extra.csv', row.names = F)

#trim<-dplyr::select(raw_extra, c(comparisonmetrics, "League"))
trim<-dplyr::select(raw_extra, contains("_scaled"))
trim<-dplyr::select(trim, -"OtherKP_scaled")


trim$league=raw$League


###
top4<-subset(trim, league!="MLS")

k2<-kmeans(dplyr::select(top4, -"league"), 2, nstart = 30)
k3<-kmeans(dplyr::select(top4, -"league"), 3, nstart = 30)
k4_top4<-kmeans(dplyr::select(top4, -"league"), 4, nstart = 30)
k5<-kmeans(dplyr::select(top4, -"league"), 5, nstart = 30)

fviz_cluster(k2, dplyr::select(top4, -"league"))
fviz_cluster(k3, dplyr::select(top4, -"league"))
fviz_cluster(k4, dplyr::select(top4, -"league"))

cleandat<-top4
colnames(cleandat)<-c('OtherType', 'Cross', 'TB', 'Long', 'Short', 'League')



top4<-subset(trim, league!="MLS")

k2<-kmeans(dplyr::select(trim, -"league"), 2, nstart = 30)
k3<-kmeans(dplyr::select(trim, -"league"), 3, nstart = 30)
k4_all<-kmeans(dplyr::select(trim, -"league"), 4, nstart = 30)
k5<-kmeans(dplyr::select(trim, -"league"), 5, nstart = 30)

fviz_cluster(k2, dplyr::select(trim, -"league"))
fviz_cluster(k3, dplyr::select(trim, -"league"))
fviz_cluster(k4, dplyr::select(trim, -"league"))

cleandat4<-top4
colnames(cleandat4)<-c('Cross', 'TB', 'Long', 'Short', 'League')

kmeans2radar(dplyr::select(cleandat4, -"league"), k4_all)

allwclust<- trim %>% mutate(cluster=k4_all$cluster)
allwclust$cluster<-as.factor(allwclust$cluster)
summary(allwclust$cluster[allwclust$league=="MLS"])

rownames(allwclust)<-raw$Team

rownames(allwclust)[allwclust$league=="MLS" & allwclust$cluster==4]

compareTeam2Clust(select(allwclust, -"league"), k4_all, "Los Angeles FC")

## soft clustering####

library(mclust)
res.mclust<-Mclust(dplyr::select(trim, -"league"), 4)
fviz_cluster(res.mclust)
plot(res.mclust, what='density')
plot(res.mclust, what='uncertainty')


mclustall<-trim
mclustall$clust<-res.mclust$classification
mclustall<-as.data.frame(mclustall)
library(Rmisc)
mclustall %>% mutate(mean_all = rowMeans(c("CrossKP_scaled", "ThroughballKP_scaled")),
              mean_sel = rowMeans(select(., cluster)))


## test chance creation graph####

chcr<-trim
chcr$CrossTB<-chcr$CrossKP_scaled+chcr$ThroughballKP_sc
chcr$totalKP<-raw_extra$TotalKeyPasses

mls<-subset(mls)
plot(raw_extra$CrossKP, raw_extra$ThroughballKP)
