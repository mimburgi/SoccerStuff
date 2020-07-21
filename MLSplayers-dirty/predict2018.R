library(caret)
library(dplyr)
allcurrent<-read.table('labeled2.txt', header = T) #2019 data

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
allcurrent<-select(allcurrent, -TouchPerc)

commonplayers<-intersect(allcurrent$Player, prev$Player)
current<-subset(allcurrent, Player %in% commonplayers)
prev<-subset(prev, Player %in% commonplayers)
#order by player name so order is the same across dataframes
current<-current[order(current$Player),]
prev<-prev[order(prev$Player),]

players<-current$Player #keep these in a vector for later so we can remove it from dfs

#separate labels that will be used for training
current_labels<-current$subtype
#separate predictors in both sets
currentpreds<-select_if(current, is.numeric)
prevpreds<-select_if(prev, is.numeric)

## basic classifier training ####
rfmod<-train(x = currentpreds, y = current_labels, method = "rf", preProcess = c('scale'), 
             trControl = trainControl(method = 'repeatedcv', savePredictions=T))
#accuracy isn't great here, but not terrible

nnetmod<-train(x = currentpreds, y = current_labels, method = "nnet", preProcess = c('scale'),
             trControl = trainControl(method = 'repeatedcv', savePredictions=T),
             tuneGrid = expand.grid(size = c(9,11, 15, 17, 19, 22), decay = c(.01, .02, .05)))
#same, not great but not terrible

crf<-train(x = currentpreds, y = current_labels, method = "cforest", preProcess = c('scale'),
               trControl = trainControl(method = 'repeatedcv', savePredictions=T), tuneLength = 7)
#a little worse

gbm<-train(x = currentpreds, y = current_labels, method = "gbm", preProcess = c('scale'),
           trControl = trainControl(method = 'repeatedcv', savePredictions=T),
           tuneGrid=expand.grid(interaction.depth=c(5,9,14,19,22),
                                n.trees=c(100,250,350),
                                shrinkage=c(.01),
                                n.minobsinnode=c(2,4,10)))
#better than I expected

knn<-train(x = currentpreds, y = current_labels, method = "knn", preProcess = c('scale'),
           trControl = trainControl(method = 'repeatedcv', savePredictions=T))
#eh

nb<-train(x = currentpreds, y = current_labels, method = "naive_bayes", preProcess = c('scale'),
           trControl = trainControl(method = 'repeatedcv', savePredictions=T))
#good

hdda<-train(x = currentpreds, y = current_labels, method = "hdda", preProcess = c('scale'),
          trControl = trainControl(method = 'repeatedcv', savePredictions=T),
          tuneGrid=expand.grid(threshold=seq(.01, .5, .05),
                               model='all'))


treebag<-train(x = currentpreds, y = current_labels, method = "treebag", preProcess = c('scale'),
                trControl = trainControl(method = 'repeatedcv', savePredictions=T))

multinom<-train(x = currentpreds, y = current_labels, method = "multinom", preProcess = c('scale'),
               trControl = trainControl(method = 'repeatedcv', savePredictions=T),
               tuneGrid = expand.grid(decay=seq(.01,.1, .02)))

parRF<-train(x = currentpreds, y = current_labels, method = "parRF", preProcess = c('scale'),
                trControl = trainControl(method = 'repeatedcv', savePredictions=T), tuneLength = 7)


### predict on 2018 ####
predrf<-predict(rfmod, newdata = prevpreds, preProcess=c('scale'))
sum(predrf==current_labels)/length(current_labels) #thats fucking terrible

predparRF<-predict(parRF, newdata = prevpreds, preProcess=c('scale'))
sum(predparRF==current_labels)/length(current_labels) #thats fucking terrible

### predict on new 2019 players ####
newcurrent<-subset(allcurrent, !(Player %in% commonplayers))

#separate labels that will be used for training
newcurrent_truelabels<-newcurrent$subtype
#separate predictors in both sets
newcurrentpreds<-select_if(newcurrent, is.numeric)

newcurrent_predlabels<-predict(rfmod, newdata = newcurrentpreds, preProcess=c('scale'))
sum(newcurrent_predlabels == newcurrent_truelabels)/length(newcurrent_predlabels)

newcurrent_predlabels<-predict(parRF, newdata = newcurrentpreds, preProcess=c('scale'))
sum(newcurrent_predlabels == newcurrent_truelabels)/length(newcurrent_predlabels)

### examine misclassifications within 2019 players ####
confusionMatrix(newcurrent_predlabels, newcurrent_truelabels)

### examine misclassifications within 2018 players ####
confusionMatrix(current_labels, predrf)
