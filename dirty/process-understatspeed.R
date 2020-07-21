library(dplyr)

dat<-read.csv('understatspeed.csv') %>% {(.)[,-1]}
head(dat)
dat$xGFperShot<-dat$xGF/dat$SF
dat$xGAperShot<-dat$xGA/dat$SA

fast<-subset(dat, Speed=='Fast') %>% dplyr::select(-Speed) %>% as.data.frame()
normal<-subset(dat, Speed=='Normal') %>% dplyr::select(-Speed) %>% as.data.frame()
slow<-subset(dat, Speed=='Slow') %>% dplyr::select(-Speed) %>% as.data.frame()
standard<-subset(dat, Speed=='Standard') %>% dplyr::select(-Speed) %>% as.data.frame()

colnames(fast)[3:10]<-paste0("Fast", colnames(fast)[3:10])
colnames(normal)[3:10]<-paste0("Normal", colnames(normal)[3:10])
colnames(slow)[3:10]<-paste0("Slow", colnames(slow)[3:10])
colnames(standard)[3:10]<-paste0("Standard", colnames(standard)[3:10])

speeds<-cbind(fast, normal[,3:10], slow[,3:10], standard[,3:10])
speeds<-dplyr::select(speeds, -contains("Speed")) %>% as.data.frame()

write.csv(speeds, 'understatspeed_processed.csv', row.names = F)
