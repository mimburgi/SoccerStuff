source('../MLSplayers-dirty/soccer_util_fxns.R')
library(dplyr)


df<-read.csv('../top5teams-upd.csv')


usevars<-c('Touches',
           'PPA','CrsPA','PassF3', 
           'PassHead', 
           'PassHigh', 
           #'PassLow', 
           'LPassAtt', 'SPassAtt', 'MPassAtt',
           'Sh', 'SoT', 'npxG', 
           'PassPrgDist', 'PassCmp', 'PassAtt', 'PassTotDist',
           'KP', 'xA',
           'PassLive','ProgPasses','Carries', 'CarryPrgDist')

trimmed <- df %>% select(all_of(c(usevars))) 

## pca dim reduction, hard to interpret ####

library(psych)
library(GPArotation)
fit <- principal(trimmed, nfactors=5, rotate="promax")
print(fit)
simple<-fit$scores %>% as.data.frame()

simple %>% mutate(team=df$Squad) %>% View

colnames(simple)<-c('Possession', 'ChanceCr', 'AerPass',
                    'PassLen', 'Crossing' )

simple$PassLen<-simple$PassLen*-1
simple %>% mutate(team=df$Squad) %>% View


## try to find teams nearest ####
team='Inter'
teamdims<-simple[df$Squad==team,] %>% unlist %>% unname
restdims<-simple[df$Squad!=team,]
nearest.idx <- which.min(colSums((t(restdims) - teamdims)^2))
df$Squad[nearest.idx]


View(simple[df$Squad %in% c('Inter', 'Lecce'),])
