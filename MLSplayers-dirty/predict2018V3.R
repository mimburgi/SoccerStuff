library(caret)
library(dplyr)
allcurrent<-read.table('labeled2.txt', header = T) #2019 data
stats<-c("shots","KP","xG","xA","ShotDist","A3Passes","M3Passes","D3Passes","percChain","xGChain","xB","ShotChainPerc","KPChainPerc")


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

## match players in each dataframe ####

#keep only the stats we want
allcurrent<-select(allcurrent, one_of(c(stats, 'Player', 'role', 'subtype')))
prev<-select(prev, one_of(c(stats, 'Player', 'role', 'subtype')))

commonplayers<-intersect(allcurrent$Player, prev$Player)
#current<-subset(allcurrent, Player %in% commonplayers)
current=allcurrent

players<-current$Player #keep these in a vector for later so we can remove it from dfs

#separate labels that will be used for training
current_roles<-current$role
#separate predictors in both sets
currentX<-select_if(current, is.numeric)

## classifier training for major roles####
set.seed(23)
rf1<-train(x = currentX, y = current_roles, method = "rf", preProcess = c('scale'), 
             trControl = trainControl(method = 'repeatedcv'), tuneLength = 6)


## add role classification on 2018 data ####

predroles_prev<-predict(rf1, newdata=prevX)

## add roles to each dataset ####
prevX2<-prevX %>% mutate(role = predroles_prev)
currentX2<-currentX %>% mutate(role = predict(rf1))
## subtype classifiers using datasets including roles ####
currentsubtypes<-current$subtype

set.seed(23)
rf2<-train(x = currentX2, y = currentsubtypes, method = "rf", preProcess = c('scale'),
           trControl = trainControl(method = 'repeatedcv'), tuneLength = 6)

set.seed(23)
gbm2<-train(x = currentX2, y = currentsubtypes, method = "gbm", preProcess = c('scale'),
           trControl = trainControl(method = 'repeatedcv'), tuneLength = 6)

## examine misclassifications from round 2 ####
confusionMatrix(rf2)
## test subtype classification on 2018 data ####
currentlabeled<-current %>% mutate(subtype=predict(rf2))
shared_current<-subset(current, Player %in% commonplayers)
shared_current<-shared_current[order(shared_current$Player),]
prevsubtype_preds<-predict(rf2, newdata = prevX2)
prevlabeled<-prev %>% mutate(subtype=prevsubtype_preds)

prevlabeled_t<-subset(prevlabeled, Player %in% commonplayers) %>% arrange(Player)
currentlabeled_t<-subset(currentlabeled, Player %in% commonplayers) %>% arrange(Player)

sum(prevlabeled_t$subtype == currentlabeled_t$subtype)/length(commonplayers)

confusionMatrix(prevlabeled_t$subtype, currentlabeled_t$subtype)

shared_current$prevsubtype<-prevsubtype_preds
