#remember to open the docker first!!!
source('FBref-scraper.R')

prevYrs=1 #null for this season's data, 1 for last seasons, 2 for two seasons ago, etc.

outfile='top5teams-py1.csv'

#df with page names and matching table names
leaguevals<-data.frame(leaguename=c(
  'Bundesliga', 'Premier-League',
  'Serie-A', 'La-Liga', 'Ligue-1'),
                       leaguenumber=c(
                         '20', '9', 
                         '11', '12', '13')
)

################
### do work ####
################

## initial scraping ####



tablenames<-c('standard', 'keeper', 'keeper_adv', 'shooting',
              'passing', 'passing_types', 'gca', 'defense',
              'possession', 'playing_time', 'misc')


for (league in 1:nrow(leaguevals)){#league is the row index number within the leaguevals df

  leaguename<-leaguevals$leaguename[league]
  leaguenumber<-leaguevals$leaguenumber[league]
  leagueurl<-paste0("https://fbref.com/en/comps/", leaguenumber, "/", leaguename, "-Stats")

  #get all tables for the league, stored in df called thisLeague
  for (tablename in tablenames){
    if (tablename==tablenames[1]){#initialize df if its the first table
      thisLeague<-getFBrefStats(leagueurl, paste0('#stats_',tablename,'_squads'),
                                numYearsBack=prevYrs)
    }
    else{#after that add to the df
      tmp = getFBrefStats(leagueurl, paste0('#stats_',tablename,'_squads'),
                          numYearsBack=prevYrs)
      thisLeague<- cbind(thisLeague, tmp)
    }
  }

  #add league name to df
  thisLeague$League<-c("League", rep(leaguename, nrow(thisLeague)-1))

  #add the league to final df called teams
  if (league==1){ #for the first league, initialize teams
    teams<-thisLeague
  }
  else{ #after that add to teams
    teams<-rbind(teams, thisLeague[-1,]) #first row is variable names
  }

}

write.csv(teams, 'top5teamsraw.csv')

## reformat to make the table easier to use ####

formatted<-teams[-1,]
colnames(formatted)<-teams[1,]
library(tidyverse)


#rename a bunch of columns

#df containing string for marker in top column of teams colnames and addon for those columns
replacevals<-data.frame(tag=c('Per 90 Minutes', 'Launched', 'Passes','Crosses', 'Goal Kicks', 
                              'Total', 'Short', 'Medium', 'Long', 
                              'Pass Types','Corner Kicks', 'Height', 'Body Parts', 'Outcomes',
                              'SCA Types', 'GCA Types', 'Tackles', 'Vs Dribbles',
                              'Pressures', 'Blocks', 'Touches', 'Dribbles', 'Carries', 'Aerial Duels'),
                        prefix=c('per90', 'GKLongPass', 'GKPass', 'GKCross', 'GKick', 
                                 'Pass', 'SPass', 'MPass', 'LPass',
                                 'Pass','CK', 'Pass', 'Pass', 'Pass',
                                 'SCA', 'GCA', 'Tkl', 'vDrib',
                                 'Press','Block', 'Touches','Drib', 'Carry', 'Aer'))
#loop through the df and add prefixes to variables in formatted
for (i in 1:nrow(replacevals)){
  names(formatted)[names(teams)==replacevals$tag[i]]<-
    paste0(
      replacevals$prefix[i], 
      names(formatted)[names(teams)==replacevals$tag[i]]
      )
}

#remove duplicate columns
formatted<-formatted[,which(!duplicated(names(formatted)))] #original scraped data with weird first row

#remove columns which are blank for some reason
formatted<-select(formatted, -c(CrdR, CrdY, `Mn/Start`))
#change some now redundant names
formatted<-formatted %>% 
  rename(Press=PressPress, Blocks=BlockBlocks, Touches=TouchesTouches, Carries=CarryCarries)

#remove spaces in column names beacuse I don't like that
colnames(formatted)<-str_replace(colnames(formatted), " ", "")

#remove per 90s (we want to convert everything to per 90)
formatted<-subset(formatted, select=which(!grepl( "90" , names(formatted))))

#remove comma in Min column so we can convert it to a number
formatted$Min<-str_replace(formatted$Min, ",", "")

#convert everything to numeric (other than actual character columns)
numbers<-formatted %>% mutate_all(as.numeric) %>% select_if(~ any(!is.na(.))) #numbers
chars<-formatted %>% select(!any_of(names(numbers))) #chars
formatted<-cbind(chars, numbers) #merge them back


#remove per 90 columns (we'll convert everything to per 90)
formatted<-subset(formatted, select=which(!grepl( "90" , names(formatted))))




#convert everything in formatted to per 90s
dontconvert<-which(grepl(pattern = 'Squad|League|/Sh|/SoT|%|MP|Min|Starts|Poss', names(formatted)))
for (row in 1:nrow(formatted)){
  formatted[row,-dontconvert]<-formatted[row,-dontconvert]/formatted$MP[row]
}

#rename some stray columns
colnames(formatted)[colnames(formatted)=="1/3"]<-'PassF3'
colnames(formatted)[colnames(formatted)=="Prog"]<-'ProgPasses'


## save out ####
write.csv(formatted,outfile)
