puli<-read.csv('Pulisic1720Matches.csv', na.strings = '') 
library(dplyr)
puli<-puli %>% filter(Comp %in% c('Premier League')) %>%
  select(all_of(c('Date', 'Start', 'Min',
                  'Gls', 'Ast', 'xG', 'xA',
                  'SCA', 'DribblesSucc', 'DribblesAtt', 'Carries', 'CarriesPrgDist')))
puli[,-c(1,2)] <- mutate_all(puli[,-c(1,2)],as.numeric)
puli<-filter(puli, !is.na(Gls))%>% filter(Min > 5)
puli<- puli %>% mutate(GA=Gls+Ast,
                       xGA=xG+xA,
                       DrbPercent=DribblesSucc/DribblesAtt,
                       GA90=GA*(90/Min),
                       xGA90=xGA*(90/Min),
                       SDrb90=DribblesSucc*(90/Min),
                       ADrb90=DribblesAtt*(90/Min),
                       Start=ifelse(Start=="Y", 1, 0),
                       SCA90=SCA*(90/Min),
                       Carries90=Carries*(90/Min),
                       CarriesPrgDist90=CarriesPrgDist*(90/Min))
puli$DrbPercent[puli$DrbPercent=='NaN']<-NA
puli$intervalnum <- c(rep(1:8, each=3)) %>% as.factor()
#puli$intervalnum <- c(rep(1:12, each=2)) %>% as.factor()
#puli$intervalnum <- c(rep(1:24)) %>% as.factor()

finalstats=c('Min', 'Start', 
             'GA', 'xGA', 'DribblesSucc', 'SCA',
             'GA90', 'xGA90', 'SDrb90', 'SCA90',
             'DrbPercent', 'Carries', 'Carries90',
             'CarriesPrgDist', 'CarriesPrgDist90',
             'DribblesAtt', 'ADrb90')

pulisums<-puli %>% group_by(intervalnum) %>% 
  select(all_of(finalstats)) %>% summarise_all(mean)

library(ggplot2)
## DribblesAtt plot ####

spline_int_AD=as.data.frame(spline(pulisums$intervalnum, 
                                scale(pulisums$DribblesAtt)))
spline_int_xGA=as.data.frame(spline(pulisums$intervalnum, 
                                     scale(pulisums$xGA)))
spline_int_GA=as.data.frame(spline(pulisums$intervalnum, 
                                    scale(pulisums$GA)))

spline_int_AD90=as.data.frame(spline(pulisums$intervalnum, 
                                   scale(pulisums$ADrb90)))
spline_int_xGA90=as.data.frame(spline(pulisums$intervalnum, 
                                    scale(pulisums$xGA90)))

# spline_int_Min=as.data.frame(spline(pulisums$intervalnum, 
#                                       scale(pulisums$Min)))

dev.off()
png("pulisic1920.png", units="in", width=5, height=3, res=200)
ggplot(pulisums) + 
  geom_point(aes(x = intervalnum, y = scale(xGA90)), size = 3) +
  geom_point(aes(x = intervalnum, y = scale(ADrb90)), size = 3) +
  geom_line(data = spline_int_AD90, aes(x = x, y = y, color='Dribbles/90'), size=1) + 
  #geom_line(data = spline_int_GA, aes(x = x, y = y, color='G+A'), size=1) +
  geom_line(data = spline_int_xGA90, aes(x = x, y = y, color='xG+xA/90'), size=1) +
  #geom_line(data = spline_int_Min, aes(x = x, y = y, color='Minutes'), size=1) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_text(aes(0,0,label = 'Pulisic\n   Avg'), size=2, hjust=-.25) +
  labs(y='z-score', x = '3-game period, 19-20 PL games', color='') + 
  theme(legend.background = element_rect(fill="transparent", colour=NA),
        legend.key        = element_rect(fill="transparent", colour=NA),
        panel.background = element_rect(fill = "ivory1", colour = NA), 
        panel.border = element_rect(fill = NA, colour = "grey50"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"),
        legend.position = c(0.5, 0.1), legend.direction = "horizontal") +
  scale_color_manual(values=c('coral3','midnightblue', 'seagreen'))
dev.off()
