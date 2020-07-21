library(kknn)
library(tidyr)

knnset<-select(raw_extra, c(Team, League,
                            #percTBKP, 
                            #attackMiddle,
                            #percOppThird,
                            #ThroughballKP,
                            #CrossKP,
                            FastSF,
                            #percLongKeyPasses,
                            #LongKeyPasses,
                            #ShortKeyPasses,
                            #percLongKP,
                            #OtherKP,
                            TotalKeyPasses,
                            #percOutofBoxShots,
                            #OutOfBoxShots,
                            #SixYardBoxShots,
                            #PenaltyAreaShots,
                            #TotalShots,
                            xGper,
                            #xGshot,
                            #xGdiffper,
                            #xGAper,
                            #xGAdiffper,
                            SetPieceShots,
                            #percSetPieceShots,
                            #TotalPasses,
                            #percLB,
                            OpenPlayShots
                            #TotalDribbles,
                            #TotalTackles,
                            #SuccessDA,
                            #Interceptions,
                            #Clearances,
                            #TotalAerial
))
knnsetscaled<-as.data.frame(scale(select_if(knnset, is.numeric)))
mlsteams<-which(knnset$League=="MLS")
euroteams<-which(knnset$League!="MLS")

topeuroteams<-which(raw_extra$League!='MLS' & raw_extra$R < 5)

knnsetscaled$Team<-knnset$Team

rawscaled<-select_if(raw_extra, is.numeric) %>% scale() %>% as.data.frame()

RSDs<-apply(rawscaled[mlsteams,], 2,sd)

library(kknn)
fit<-kknn(Team ~ ., knnsetscaled[euroteams,], knnsetscaled[mlsteams,],
     k = 3, scale = F)

mlscomp<-subset(knnset, League=='MLS') %>%
  mutate(eurocomp=as.character(fit$fitted.values)) %>%
  mutate(team=subset(raw, League=='MLS')[['Team']])

View(mlscomp)

length(unique(mlscomp$eurocomp))

