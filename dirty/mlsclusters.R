library(tidyr)
library(dplyr)
source('soccer_util_fxns.R')
##initial feature trimming, keep only what might be valuable ####

raw<-read.table('rawdata_full.txt', header = T)

trim<-select(raw, c(Team, League,
                    TotalKeyPasses, 
                    ppda_coef,
                    deep, deep_allowed, FastxG, 
                    percOutofBoxShots, percSixYardBoxShots,
                    percPenaltyAreaShots, accLB, percLB,
                    accSP, percDribbles, percSetPieceShots,
                    percCounterShots, percLongKP, percTBKP, percCrossKP, 
                    xGper, xGdiffper, xGAper, xGAdiffper,
                    xGshot, pAdjTackles, pAdjInts, pAdjClearances))

## scale data by league ####
teams<-trim$Team
grouped<-trim %>% mutate(LeagueNum=as.numeric(as.factor(League))) %>%
  select_if(is.numeric) %>% group_by(LeagueNum)
scaled<-grouped %>% scale() %>% as.data.frame() %>% select(-LeagueNum)
scaledcomplete<- scaled[complete.cases(scaled), ]


##reduce dimensionality ####
library(psych) #for principal
library(GPArotation) #for rotation within principal
fit <- principal(cor(scaledcomplete), nfactors=4, rotate="oblimin")
print(fit)
simple<-fit$scores %>% as.data.frame()


library(FactoMineR)
library(factoextra)
res.pca <- PCA(scaledcomplete, graph=FALSE, ncp=6)

fviz_screeplot(res.pca)

res.pca$var$cor %>% # table with factor loadings
  varimax # but ask for a varimax rotation to improve interpretability
fviz_pca_var(res.pca, repel = TRUE) # the repel = TRUE argument makes sure the text is displayed nicely on the graph




###look for most variability across candidate features ####



relsds<-trim2 %>% select_if(is.numeric) %>% apply(2, rsd) %>% 
  as.data.frame() %>% rownames_to_column() %>% arrange(desc(.))

#take top 10 including xGdiffper which is negative but huge
top10<-relsds$rowname[c(1:10)]
trim3<-select(trim2, one_of(top10))
trim3$Team<-trim$Team

## try regular knn ####
mlsteams<-which(trim$League=="MLS")
euroteams<-which(trim$League!="MLS")

fit<-kknn::kknn(Team ~ ., trim3[euroteams,], trim3[mlsteams,],
                k = 3)
mlscomp<-trim3[mlsteams,] %>%
  mutate(eurocomp=as.character(fit$fitted.values)) %>%
  mutate(team=subset(trim, League=='MLS')[['Team']])

View(mlscomp)

length(unique(mlscomp$eurocomp)) #only twelve comparison teams

#need to exaggerate features?

###capping scaled values at 1####

scaledat<-select_if(trim3, is.numeric) %>% scale() %>% as.data.frame()
scaledat[scaledat > 1]<- 2
scaledat[scaledat < -1]<- -2
scaledat$Team<-trim3$Team

fit<-kknn::kknn(Team ~ ., scaledat[euroteams,], scaledat[mlsteams,],
                k = 3, scale = F)
mlscomp<-trim3[mlsteams,] %>%
  mutate(eurocomp=as.character(fit$fitted.values)) %>%
  mutate(team=subset(trim, League=='MLS')[['Team']])

View(mlscomp)

length(unique(mlscomp$eurocomp)) #only twelve comparison teams