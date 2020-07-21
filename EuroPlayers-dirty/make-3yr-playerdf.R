library(tidyverse)

df1<-read.csv('../top5players-upd.csv')
df2<-read.csv('../top5players-py1.csv')
df3<-read.csv('../top5players-py2.csv')

#list of dfs used for loops later
dflist<-c('df1', 'df2', 'df3')

#rename MP to matches played
#makes it easier to distinguish from MPass columns which contain the string MP
for (year in dflist){
  yeardf<-get(year) %>% rename(MatchesPlayed=MP) #rename to MatchesPlayed
  assign(year, yeardf) #assign it to the original variable name
}

#identify types of variables to help aggregate across years

#fixedvars wont be aggregated year to year
fixedvars<-which(grepl(pattern = 'Player|Pos|Squad|League|Age|Born|Nation', names(df1)))
#playtimevars can be used to calculate aggregated vars
playtimevars<-'Matches|Min|Starts'
#vars to aggregate across years
#to aggregate, we'll calculated averages per year weighted by number of 90s played that year
#equates to per90 over three years for counting stats and percentages weighted by play time
toagg<-which(!(names(df1) %in% c(fixedvars, playtimevars)))

#for (year in dflist){ #loop through each year and process the same way
  yeardf<-get(year) #assign the df to a variable called yeardf to manipulate within the loop
  
  #convert back from per 90s to totals
  for (row in 1:nrow(yeardf)){
    yeardf[row,-notper90]<-yeardf[row,-notper90]*(df1$Min[row]/90)
  }
  
  #matching players across years is a pain in the ass because of duplicate player names within years
  #some are actually different players (eg, there are two Marcelos)
  #some are the same player on diff teams
  #using DOB can separate the two
  yeardf$PlayerDOB<-paste(yeardf$Player, yeardf$Born, sep=':')
  
  #make vector of players that are on two teams so theyre listed twice 
  dups<-yeardf$PlayerDOB[duplicated(yeardf$PlayerDOB)]
  
  #separate them out so we can collapse across teams
  nodupdf<-filter(yeardf, !(PlayerDOB %in% dups))
  dupdf<-filter(yeardf, PlayerDOB %in% dups)
  
  #collapse across teams for duplicated players
  #calculate weighted avs when needed
  dupdf_procd<-dupdf[1,] %>% .[-1,] #makes empty df with same colnames
  for (playerDOB in dupdf$PlayerDOB){ #for each duplicated player
    playerrows<-dupdf[dupdf$PlayerDOB==playerDOB,] #isolate rows for the player
    dupdf_procd<-dupdf_procd %>% #add rows with players name, DOB, Nation, etc
      add_row(
        Player=strsplit(playerDOB, ":")[[1]][1],
        Born=as.numeric(strsplit(playerDOB, ":")[[1]][2]),
        PlayerDOB=playerDOB,
        Nation=playyerrows$Nation[1],
        Pos=playyerrows$Pos[1]
      )
    #add weighted averages
    for (var in toagg){
      dupdf_procd[[var]][nrow(dupdf_procd)]<-
        
    }
  }
  
  
  assign(paste0(year, "procd"), yeardf) #save the processed df as df1procd, df2procd, etc
#}


#matching players across years is a pain in the ass because of duplicate player names within years
#some are actually different players (eg, there are two Marcelos)
#some are the same player on diff teams
#using DOB can separate the two
df1$PlayerDOB<-paste(df1$Player, df1$Born, sep=':')
df2$PlayerDOB<-paste(df2$Player, df2$Born, sep=':')
df3$PlayerDOB<-paste(df3$Player, df3$Born, sep=':')

#add a row to mark duplicate player+DOB 
for (df in c("df1", "df2", "df3")){
  assign(df, get(df))
}
df1$duplicateplayer<-duplicated(df1$PlayerDOB)
df2$duplicateplayer<-duplicated(df2$PlayerDOB)
df3$duplicateplayer<-duplicated(df3$PlayerDOB)

for (player in df1$duplicateplayer){
  
}

#this vector is every player plus DOB active in those three years
allplayers<-c(df1$PlayerDOB, df2$PlayerDOB, df3$PlayerDOB)

#vectors of missing players in each df
missingdf1<-allplayers[!(allplayers %in% df1$PlayerDOB)]
missingdf2<-allplayers[!allplayers %in% df2$PlayerDOB]
missingdf3<-allplayers[!allplayers %in% df3$PlayerDOB]

#add empty columns for missing players in df1
for (playerDOB in missingdf1){
  df1<-df1 %>% add_row(Player=strsplit(playerDOB, ":")[[1]][1],
                       Born=as.numeric(strsplit(playerDOB, ":")[[1]][2]),
                       PlayerDOB=playerDOB)
}

#add empty columns for missing players in df2
for (playerDOB in missingdf2){
  df2<-df2 %>% add_row(Player=strsplit(playerDOB, ":")[[1]][1],
                       Born=as.numeric(strsplit(playerDOB, ":")[[1]][2]),
                       PlayerDOB=playerDOB)
}

#add empty columns for missing players in df3
for (playerDOB in missingdf3){
  df3<-df3 %>% add_row(Player=strsplit(playerDOB, ":")[[1]][1],
                       Born=as.numeric(strsplit(playerDOB, ":")[[1]][2]),
                       PlayerDOB=playerDOB)
}

#sort them all
df1<-df1 %>% arrange(PlayerDOB)
df2<-df2 %>% arrange(PlayerDOB)
df3<-df3 %>% arrange(PlayerDOB)

