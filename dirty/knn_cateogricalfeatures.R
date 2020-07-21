library(tidyr)
source('soccer_util_fxns.R')
##initial feature trimming, keep only what might be valuable ####

raw<-read.table('rawdata_full.txt', header = T)

trim<-select(raw, c(Team, League,
                    #percTBKP, 
                    attackMiddle,
                    percOppThird,
                    ThroughballKP,
                    CrossKP, 
                    CounterShots,
                    #FastSF,
                    #percLongKeyPasses,
                    LongKeyPasses,
                    ShortKeyPasses,
                    #percLongKP,
                    #OtherKP,
                    TotalKeyPasses,
                    #percOutofBoxShots,
                    OutOfBoxShots,
                    SixYardBoxShots,
                    #PenaltyAreaShots,
                    #TotalShots,
                    xGper,
                    xGshot,
                    #xGdiffper,
                    xGAper,
                    #xGAdiffper,
                    SetPieceShots,
                    #percSetPieceShots,
                    TotalPasses,
                    #percLB,
                    OpenPlayShots,
                    TotalDribbles,
                    pAdjTackles,
                    pAdjInts,
                    pAdjClearances,
                    TotalAerial
))
