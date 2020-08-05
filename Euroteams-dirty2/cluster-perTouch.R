source('./MLSplayers-dirty/soccer_util_fxns.R')
library(dplyr)


df<-read.csv('../top5teams-upd.csv')


usevars<-c('Touches',
           'PPA','CrsPA','PassF3', 
           #'PassHead', 
           'PassHigh', 'PassLow', 'LPassAtt', 'SPassAtt', 'MPassAtt',
           'Sh', 'SoT', 'npxG', 
           'PassPrgDist', 'PassCmp', 'PassAtt', 'PassTotDist',
           'KP', 'xA',
           'PassLive','ProgPasses','Carries', 'CarryPrgDist')

trimmed <- df %>% select(all_of(c(usevars))) %>%
  mutate_at(vars(-Touches), ~ . / Touches) %>% scale() %>% as.data.frame()

## pca dim reduction, hard to interpret ####

library(psych)
library(GPArotation)
fit <- principal(trimmed, nfactors=5, rotate="varimax")
print(fit)
simple<-fit$scores %>% as.data.frame()

simple %>% mutate(team=df$Squad) %>% View

colnames(simple)<-c('ChanceCr', 'PatientPass', 'QualChances',
                    'LongProg', 'ShProg', )
att<-simple

## try straight tsne - dont think there are enough samples####
library(Rtsne)
restsne<-Rtsne(trimmed, perplexity=6)
plot(restsne$Y)

## try a pca preproc, then a simple clustering ####
library(FactoMineR)
library(factoextra)
res.pca<-PCA(trimmed, scale.unit = F, graph = F)
fviz_screeplot(res.pca) #3 dimensions, 6 if we're being very generous
res.pca$ind$coord


