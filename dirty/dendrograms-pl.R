## preproc ####
raw<-read.csv("PL-Whoscored.csv")
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
                 "CornerKP", "ThroughballKP", "FreekickKP", "ThrowinKP", 
                 "OtherKP")
raw<-select(raw, -"R")
raw<-select(raw, -"ThrowinAss")
raw<-select(raw, -"TotalShots.2")

rownames(raw)<-raw$Team
raw<-select(raw, -"Team")
scaled<-scale(raw)
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
                "CornerKP", "ThroughballKP", "FreekickKP", "ThrowinKP", 
                "OtherKP")]

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

offense_trim<-cbind(offense_totals, select(offense_percs, -c("percFreekickAss", "percCornerAss")))

offense_trim2<-select(offense_trim, -contains("Goals"))
offense_trim2<-select(offense_trim2, -contains("Ass"))


otrim3raw<-select(offense_percs, c("percOutofBoxShots", "percAerial", "percLB",
                                "percLongKeyPasses", "percDribbles", "percSetPieceShots",
                                "percCounterShots", "percTBKP", "percCrossKP"))
otrim3<-scale(select(otrim3raw, -c("percAerial")))
## pca ####
res.pca <- PCA(otrim3raw,  graph = FALSE)
# Visualize eigenvalues/variances
library(factoextra)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
# Extract the results for variables
var <- get_pca_var(res.pca)
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC3
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
# Contributions of variables to PC4
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10)

## compare algs ####
intern <- clValid(otrim3, nClust = 2:8, 
                  clMethods = c("hierarchical","kmeans","pam", "som"), validation = "internal")

## hc ####


# Compute dissimilarity matrix with euclidean distances
d <- dist(otrim3, method = "euclidean")
# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "ward.D2" )
# Cut tree until we hit 9 groups
for (k in 1:9){
  grp<- cutree(res.hc, k = k)
  assign(paste0("group", k), data.frame(teams=names(grp), clusters=unname(grp)))
}
# Visualize
plot(res.hc, cex = 0.6) # plot tree
rect.hclust(res.hc, k = 6, border = 2:9) # add rectangle


clustertots2<-as.data.frame(otrim3) %>% mutate(Cluster = group2$clusters) %>% group_by(Cluster) %>% summarise_all("mean")

clustertots3<-as.data.frame(otrim3) %>% mutate(Cluster = group3$clusters) %>% group_by(Cluster) %>% summarise_all("mean")


clustertots4<-as.data.frame(otrim3) %>% mutate(Cluster = group4$clusters) %>% group_by(Cluster) %>% summarise_all("mean")



clustertots5<-as.data.frame(otrim3) %>% mutate(Cluster = group5$clusters) %>% group_by(Cluster) %>% summarise_all("mean")
clustertots6<-as.data.frame(otrim3) %>% mutate(Cluster = group6$clusters) %>% group_by(Cluster) %>% summarise_all("mean")


summary()
