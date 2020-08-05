#remember to open the docker first!!!
source('FBref-scraper.R')

prevYrs=NULL #null for this season's data, 1 for last season's, 2 for two seasons ago, etc.

outfile='Pulisic1720Matches.csv'
years=c('2017-2018','2018-2019','2019-2020')

#formatting of these is important
baseurl='https://fbref.com/en/players/1bf33a9a/matchlogs/'
player='Christian-Pulisic'

################
### do work ####
################

## initial scraping ####


for (year in years){#league is the row index number within the leaguevals df
  
    if (year==years[1]){#initialize df if its the first year in the list
      matches<-getFBrefStats(
        paste0(baseurl,year, "/summary/", player, "-Match-Logs"), 
        '#matchlogs_all')
      matches$Year=year
    }
    else{#after that add to the df
      tmp<-getFBrefStats(
        paste0(baseurl,year, "/summary/", player, "-Match-Logs"), 
        '#matchlogs_all')
      tmp$Year=year
      matches<-rbind(matches, tmp)
    }
    
}#end years loop





## reformat to make the table easier to use ####

formatted<-matches[-1,]
colnames(formatted)<-matches[1,]
library(tidyverse)


#rename a few of columns
torename<-which(colnames(formatted)%in%c("Succ",'Att','Cmp','Cmp%', 'PrgDist'))
colnames(formatted)[torename]<-paste0(colnames(matches)[torename], colnames(formatted)[torename])

#remove rows with variable names and empty rows and totals
formatted<-subset(formatted, Squad!="Squad")
formatted<-subset(formatted, Squad!="")


## save out ####
write.csv(formatted,outfile, row.names = F)
