library(caret)
library(dplyr)
current<-read.table('labeled3.txt', header = T) #2019 data

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

#drop touchperc because it isn't available from 2018
commonplayers<-intersect(current$Player, prev$Player)

#separate labels that will be used for training
currentroles<-current$role
#separate predictors in both sets

descstats<-c("shots", "KP", 'A3Passes', 'M3Passes', 'D3Passes', 'percChain', 'ShotChainPerc', 'KPChainPerc', 'ShotDist')

currentX<-select(current, one_of(descstats))
prevX<-select(prev, one_of(descstats))



## basic classifier training ####
set.seed(20)
gbm<-train(x = currentX, y = currentroles, method = "gbm", preProcess = c('scale', 'center'),
           trControl = trainControl(method = 'repeatedcv'),
           tuneGrid=expand.grid(interaction.depth=1,
                                n.trees=seq(50,350,50),
                                shrinkage=c(.01, .1),
                                n.minobsinnode=c(2,6,10)))

### predict on 2018 ####
predgbm<-predict(gbm, newdata = prevX, preProcess=c('scale', 'center'))

library(dplyr)
prevwithpreds<-prev %>% mutate(predrole=predgbm)
currentwithpreds<-current %>% mutate(predrole=predict(gbm))

prevcommon<-subset(prevwithpreds, Player %in% commonplayers)
currentcommon<-subset(currentwithpreds, Player %in% commonplayers) %>% arrange(Player)

length(prevcommon$predrole == currentcommon$predrole)/length(commonplayers)

## predict attacking styles ####
attackerstats<-c('xG', 'xA', 'shots', 'KP', 'ShotChainPerc', 'KPChainPerc', 'xGChain', 'ShotDist')
currentA<-subset(currentwithpreds, predrole=='attacker')
currentAlabs<-droplevels(currentA$subtype)
currentAX<-select(currentA, one_of(attackerstats)) 

set.seed(200)
knnA<-train(x = currentAX, y = currentAlabs, method = "knn",
          trControl = trainControl(method = 'repeatedcv'),
          preProcess = c('scale', 'center'), tuneLength = 10)
rfA<-train(x = currentAX, y = currentAlabs, method = "rf",
            trControl = trainControl(method = 'repeatedcv'),
            preProcess = c('scale', 'center'), tuneLength = 10)


currentA$predtypeknn<-predict(knnA)
currentA$predtyperf<-predict(rfA)

prevA<-subset(prevwithpreds, predrole=='attacker')
prevAX<-select(prevA, one_of(attackerstats))
prevA$predtypeknn=predict(knnA, newdata = prevAX, preProcess=c('scale', 'center'))
prevA$predtyperf=predict(rfA, newdata = prevAX, preProcess=c('scale', 'center'))

commonA<-intersect(currentA$Player, prevA$Player)

bothcurrentA<-subset(currentA, Player %in% commonA) %>% arrange(Player)
bothprevA<-subset(prevA, Player %in% commonA) %>% arrange(Player)

sum(bothcurrentA$predtypeknn==bothprevA$predtypeknn)/length(bothcurrentA$Player)


### examine misclassifications within 2019 players ####
confusionMatrix(bothcurrentA$predtypeknn, bothprevA$predtypeknn)

### examine misclassifications within 2018 players ####
confusionMatrix(current_labels, predrf)
