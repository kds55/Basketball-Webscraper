#use these URL to compare summaries below to ensure data complete
#https://www.basketball-reference.com/leagues/NBA_1990.html use these URL for season summaries
#https://www.basketball-reference.com/playoffs/NBA_1990.html 

#could add so it pulls in data from website and automatically tests for completeness

###############################################################################################################
#Completeness testing on the game summaries
#Below summarizes each teams wins/losses and games played for the season.
#This can be also be double checked to league standings for those seasons to ensure data appears complete.

#SLIGHT ERROR IN 2006 & 2007 missing around 2 games for each team and maybe. Basketball Reference is missing them for some reason.

#libraries
library(readr) #for importing csv
library(readxl) #for importing xlx
library(lubridate) #for changing the date to column to an appropriate date format. Currently a string
library(dplyr)
library(stringi)

#importing boxscore data and playoff dates data
games <- read_csv("~/Data Mining/Basketball/Finals/games1990to2020.csv", 
                  col_types = cols(X1 = col_skip()))

playoffstarts <- read_excel("~/Data Mining/Basketball/Setup & misc excels/Season lengths and playoff starts.xlsx", 
                            col_types = c("numeric", "text", "date"))


#merges data so it brings in playoff start date to each line in the game summary based off of the season
df = merge(games, playoffstarts, 'season')

# change columns to the correct types
df$date_game   <- mdy(df$date_game)
df$visitor_pts <- as.numeric(df$visitor_pts)
df$home_pts    <- as.numeric(df$home_pts)
df$attendance  <- as.numeric(gsub(",", "", df$attendance))

#adjusting to set games as either playoff or regular season.
#to do so added new column gametype and set all values to regular. From here filtered through based off playoff date
#using a simple if formula to set the value to playoff if the date is past the playoff start date
df$gametype = 'regular'

for(i in 1:nrow(df)){
  if(df$date_game[i] >= df$playoffstartdate[i]){
    df$gametype[i] = 'playoff'
  }
}

#deletes the playoff start columns as not relevant anymore
#also delete boxscoretext and season months as column. Not used for anything
df$seasonmonths <- NULL
df$playoffstartdate <- NULL
df$box_score_text <- NULL

# get winner and loser of each game
df$winner <- with(df, ifelse(visitor_pts > home_pts, 
                             TeamIDAway, TeamIDHome))
df$loser <- with(df, ifelse(visitor_pts < home_pts, 
                            TeamIDAway, TeamIDHome))

# We now need to create the standings for each team for each season
# to do this we first create a blank data frame and create a list of the seasons in our data set
seasons = unique(df$season)
finalstandings = data.frame()

#populate W-L column, W pct by cycling through each seasons subset
#now we subset the data into each season and create a list of the teams that year and set the win/loss for each season to 0
#we then go through the season and sum each teams wins and losses line by line  based off of the games for that year (regular_df)
#this works by going through each row (or team) and then going to the regular_df to sum all wins and losses with that team in it
#Ex. it would choose row 1 which would be ATL and then it wowuld go through regular_ df to sum all the wins and losses that exist
#additional columns are added for clarification and to seperate each season
#As it goes through each year it then adds this to the final standings df. 
for( year in seasons){
  regular_df <- subset(df, gametype == "regular" & season == year)
  teams <- sort(unique(regular_df$TeamIDAway))
  standings <- data.frame(team = teams, stringsAsFactors = FALSE)
  standings$win <- 0; standings$loss <- 0
  for (i in 1:nrow(standings)) {
    standings$win[i]  <- sum(regular_df$winner == standings$team[i])
    standings$loss[i] <- sum(regular_df$loser  == standings$team[i])
  }
standings$wl_pct <- with(standings, win / (win + loss))
standings$gamesplayed <- with(standings, win + loss)
standings$season = year
finalstandings = rbind (finalstandings, standings)
}




#################################################################################################################
#below summarizes the boxscore data into various different tables
#1 - into team toatals for the year
#2 - into team totals for each game

#importing boxscores
boxscores <- read_csv("Data Mining/Basketball/Finals/boxscores1990to2020.csv", 
                      col_types = cols(X1 = col_skip(), mp = col_character(), plus_minus = col_number()))

#this wasn't removed from all boxscores when data was scraped - found later. As this screws up the boxscore data, 
#removed it again here to ensure no errors.
boxscores = boxscores[boxscores$mp != "Player Suspended",]

#goes through the boxscores and changes the minutes into a number (currently format is string - ex 32:30 rather then 32.5)
for (i in 1:nrow(boxscores)){
  if(nchar(boxscores$mp[i])>4){
    boxscores$mp[i] <- as.numeric(stri_sub(boxscores$mp[i],1, 2)) + as.numeric(stri_sub(boxscores$mp[i],-2, -1))/60
  }else {
    boxscores$mp[i] <- as.numeric(stri_sub(boxscores$mp[i],1,1)) + as.numeric(stri_sub(boxscores$mp[i],-2, -1))/60
  }
}

# change columns to the correct types
boxscores$mp <- as.numeric(boxscores$mp)
boxscores$fg <- as.numeric(boxscores$fg)
boxscores$fga <- as.numeric(boxscores$fga)
boxscores$fg_pct <- as.numeric(boxscores$fg_pct)
boxscores$fg3 <- as.numeric(boxscores$fg3)
boxscores$fg3a <- as.numeric(boxscores$fg3a)
boxscores$fg3_pct <- as.numeric(boxscores$fg3_pct)
boxscores$ft <- as.numeric(boxscores$ft)
boxscores$fta <- as.numeric(boxscores$fta)
boxscores$ft_pct <- as.numeric(boxscores$ft_pct)
boxscores$orb <- as.numeric(boxscores$orb)
boxscores$drb <- as.numeric(boxscores$drb)
boxscores$trb <- as.numeric(boxscores$trb)
boxscores$ast <- as.numeric(boxscores$ast)
boxscores$stl <- as.numeric(boxscores$stl)
boxscores$blk <- as.numeric(boxscores$blk)
boxscores$tov <- as.numeric(boxscores$tov)
boxscores$pf<- as.numeric(boxscores$pf)
boxscores$pts <- as.numeric(boxscores$pts)


#creating a subset that includes game_id and the season year so we can merge this into the boxscores data to subset it
gameidseasons = df[,c('game_id', 'season', 'gametype')]

#merging the season year into boxscores data
boxscores = merge(boxscores, gameidseasons)

#Tallying up team totals for the season, below redoes this time teamtotals2 so it adds opt stats as its not possible yet.
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#can prob delete these two team totals
teamtotals = summarize( group_by(boxscores, team, gametype, season), mp = sum(mp), fg = sum(fg), fga = sum(fga), fg_pct = sum(fg)/sum(fga),
                        fg3 = sum(fg3), fg3a = sum(fg3a), fg3_pct = sum(fg3)/sum(fg3a), fg2 = sum(fg) - sum(fg3), fg2a = sum(fga) - sum(fg3a),
                        fg2_pct = (sum(fg) - sum(fg3))/ (sum(fga) - sum(fg3a)), ft = sum(ft), fta = sum (fta), ft_pct = sum(ft)/sum(fta), orb = sum(orb), drb = sum(drb), trb = sum(trb),
                        ast = sum(ast), stl = sum(stl), blk = sum(blk), tov = sum(tov), pf = sum(pf), pts = sum(pts))

#adds the win/losses for the season into the team totals
teamtotals = merge(teamtotals, finalstandings, c('team','season'))

#summarizing totals for each game per team. Ex how many PTS Atl scored against BKN on Feb 3/16
gametotals= summarize(group_by(boxscores, team, game_id, gametype, season), mp = sum(mp), fg = sum(fg), fga = sum(fga), fg_pct = sum(fg)/sum(fga),
                      fg3 = sum(fg3), fg3a = sum(fg3a), fg3_pct = sum(fg3)/sum(fg3a), fg2 = sum(fg) - sum(fg3), fg2a = sum(fga) - sum(fg3a),
                      fg2_pct = (sum(fg) - sum(fg3))/ (sum(fga) - sum(fg3a)), ft = sum(ft), fta = sum (fta), ft_pct = sum(ft)/sum(fta), orb = sum(orb), drb = sum(drb), trb = sum(trb),
                      ast = sum(ast), stl = sum(stl), blk = sum(blk), tov = sum(tov), pf = sum(pf), pts = sum(pts))


#adding in if the team was either home or away for that game
for(i in 1:nrow(gametotals)){
  if(gametotals$team[i] == substring(gametotals$game_id[i], 10)){
    gametotals$homeoraway[i] = 'home'
  } else {
     gametotals$homeoraway[i] = 'away'
   }
}

#######################################################################################################
#below adds in the opt column into the gametotals

teamdata= data.frame()
gametotalsfinal= data.frame()

gametotalsoptteam = gametotals

#creates the data frame with all stats listed as o. Includes every game every team
for(i in 1:ncol(gametotalsoptteam)){
  names(gametotalsoptteam)[i] = paste( 'o', names(gametotalsoptteam)[i], sep='')
}

for(i in 1:nrow(gametotals)){
teamdata= data.frame()
game = gametotals$game_id[i]
teamselected = gametotals$team[i]
  
#subsets out the first sample and add its to the final data frame to have the opt stats merged to it
temp1 = subset(gametotals, game_id == game & team == teamselected)
#final = rbind(final, test1)

#subsets the opt games to be the opt team of the game selected.
#merges this into the final data farme
temp2 = subset(gametotalsoptteam, ogame_id == game)
temp2 = subset(temp2, oteam != teamselected)
teamdata = merge(temp1, temp2, by.x ='game_id',by.y = 'ogame_id')
gametotalsfinal = rbind(gametotalsfinal, teamdata)


}

#gets rid of columns in the opt df that got merged in and are now duplicated/not useful
gametotalsfinal$ogametype <- NULL
gametotalsfinal$oseason <- NULL

#As games occured where a team shot 0 3pts it caused a NaN to be populated as your dividing 0/0 which is an error/undefined.
for(i in 1:nrow(gametotalsfinal)){
  if(gametotalsfinal$fg3_pct[i] == 'NaN'){
    gametotalsfinal$fg3_pct[i]= 0
  }
  
  if(gametotalsfinal$ofg3_pct[i] == 'NaN'){
    gametotalsfinal$ofg3_pct[i]= 0
  }
}


###############################################################################
#Below creates a final team totals summary, this includes both the teams stats as well as the opponent stats for each season
teamtotals2 = summarize( group_by(gametotalsfinal, team, season, gametype), mp = sum(mp), fg = sum(fg), fga = sum(fga), fg_pct = sum(fg)/sum(fga),
                         fg3 = sum(fg3), fg3a = sum(fg3a), fg3_pct = sum(fg3)/sum(fg3a), fg2 = sum(fg) - sum(fg3), fg2a = sum(fga) - sum(fg3a),
                         fg2_pct = (sum(fg) - sum(fg3))/ (sum(fga) - sum(fg3a)), ft = sum(ft), fta = sum (fta), ft_pct = sum(ft)/sum(fta), orb = sum(orb), drb = sum(drb), trb = sum(trb),
                         ast = sum(ast), stl = sum(stl), blk = sum(blk), tov = sum(tov), pf = sum(pf), pts = sum(pts),
                         
                         #opt team stats
                         omp = sum(omp), ofg = sum(ofg), ofga = sum(ofga), ofg_pct = sum(ofg)/sum(ofga),
                         ofg3 = sum(ofg3), ofg3a = sum(ofg3a), ofg3_pct = sum(ofg3)/sum(ofg3a), ofg2 = sum(ofg) - sum(ofg3), ofg2a = sum(ofga) - sum(ofg3a),
                         ofg2_pct = (sum(ofg) - sum(ofg3))/ (sum(ofga) - sum(ofg3a)), oft = sum(oft), ofta = sum (ofta), oft_pct = sum(oft)/sum(ofta), oorb = sum(oorb), odrb = sum(odrb), otrb = sum(otrb),
                         oast = sum(oast), ostl = sum(ostl), oblk = sum(oblk), otov = sum(otov), opf = sum(opf), opts = sum(opts))

teamtotals2 = merge(teamtotals2, finalstandings, c('team','season'))


######################################
#setting up a df for gametotalsfinal where it gets rid of duplicates. 
#right now each line in our data frame includes the selected team plus the opt team.
#our data is not setup to get rid of the potential duplicates that result from each game showing up twice (as two rows), 
#once where there the selected team and once where there the opt team.
#Ex So if BOS played ATL there would be a line where it had BOS with Oteam as ATL, 
#and a line where it has ATL with oteam as BOS. This causes duplicates in our data and essentialy doubles the games.
#to get rid of duplicate games, filtered the games to only include the away games as the selected team for that game_id. 
#at this point all the home team games are gone (where they show up as the selected team. 
#It does still include the home teams stats as they show up as the oteam in the data frame
#below then changes it so the oteam shows up as t2 or home, and the original teams data gets changed to t1 or away team


df1 = subset(gametotalsfinal, homeoraway == 'away')


for(i in c(2,5:27)){
  names(df1)[i] = paste( 't1', names(df1)[i], sep='_')
}

for(i in c(28:ncol(df1))){
  names(df1)[i] = substring(names(df1[i]), 2)
  names(df1)[i] = paste( 't2', names(df1)[i], sep='_')
}



check = sum(gametotalsfinal$opts) - sum(gametotals$pts)
check = sum(gametotalsfinal$pts) - sum(df1$t1_pts)- sum(df1$t2_pts)

#write.csv(gametotalsfinal,'gametotals.csv')
