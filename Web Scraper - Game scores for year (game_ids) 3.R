
#libaries
library(rvest)
library(lubridate)

#setting the seasons you want to scrape
seasonlist = c(2007)

#################################
#Scraping starts here

#creates empty data frame
df <- data.frame()

#code essentially creates a series of url's based off of the season/year and the months in that season
#the code then runs through each season's months and pulls all the games from there schedule page
#This is used to pull game_id's for all games which is used for the main boxscore scraper part
for(season in seasonlist){
  
  #adjusts for the different season lengths (months in season). Important as month used in website URL 
  #so need to know which months for each season
  if (season %in% c("1990", "1991" , "1992" , "1993" , "1994" , "1995" , "1996" , "1997"  ,"2000" , "2005" , "2006")){
   monthList = c("november", "december", "january", "february", "march", "april", "may", "june")
  } else if (season %in% c("1998" , "2001" , "2002" , "2003" , "2004" , "2007" , "2008" , "2009" , "2010" , "2011" , "2013" , "2014" , "2015" , "2016" , "2017" , "2018" , "2019" )){
   monthList = c("october", "november", "december", "january", "february","march", "april", "may", "june")
  } else if (season == "1999") {
    monthList =c("february","march", "april", "may", "june")
  } else if (season == "2012") {  
    monthList = c("december", "january", "february","march", "april", "may", "june")
  } else if (season == "2020") {    
    monthList = c("october", "november", "december", "january", "february","march", "july","august", "september") #includes playoffs in october (will have 2020 playoffs and 2019 regular season games)
  } else
    print("error")

   
  for (month in monthList) {
    #Creates formula for the website
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_", season, 
                 "_games-", month, ".html")
    webpage <- read_html(url)
  
    #Gets the column names from the website
    col_names <- webpage %>% 
      html_nodes("table#schedule > thead > tr > th") %>% 
     html_attr("data-stat")    
    col_names <- c("game_id","season", col_names)

    # extract dates column
    # note that in April, there is a break in the table which just says 
    # "Playoffs". this messes with the data merging later, so we get rid of it
    dates <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > th") %>% 
      html_text()
    dates <- dates[dates != "Playoffs"]
  
    # extracts game id
    # we need to remove the NA that is due to the "Playoffs" row in April
    game_id <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > th") %>%
      html_attr("csk")
    game_id <- game_id[!is.na(game_id)]
  
    # extract all other columns (except date - which is contained in a different part that is already scraped above)
    data <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > td") %>% 
      html_text() %>%
      matrix(ncol = length(col_names) - 3, byrow = TRUE)
  
    #As they didnt keep track of game start time before 2001, this will fill in and create N/A columns for these years.
    if(season < 2001){
      col_names = c("game_start_time", col_names)
      game_start_time = "NA"
    }

    #combine game IDs, dates, etc data data into the dataframe for each month (later merged with all months) 
    #If formula used here as before 2001 they did not keep track of game_start_time so this is manually inputed as per above
    #If formula needed so we can add the additional column to the prior to 2001 seasons for game start time. 
    #If this column wasn't added it would screw up the merge of years prior to 2001 and after as there would be a diff # of columns
    if (season < 2001){
      month_df <- as.data.frame(cbind( game_start_time, game_id, season, dates, data), stringsAsFactors = FALSE)
    }else
      month_df <- as.data.frame(cbind(game_id, season, dates, data), stringsAsFactors = FALSE)
    
    #Sets the column names for the data frame
    names(month_df) <- col_names
  
    #merges each month into the final overall dataframe
    df <- rbind(df, month_df)

}
}

########################################
#Main webscraper is completed. Now just need to add in columns for away and home teams abbreviated ID's
#This is needed as this is whats used in the box score scraper to identify which team teams boxes exist to scrape per webpage

#creating a list of all the teams long name to associate with there abbreviated names below
home_team_name = visitor_team_name = c('Milwaukee Bucks', 'Toronto Raptors', 'Boston Celtics', 'Indiana Pacers', 'Miami Heat', 'Philadelphia 76ers', 'Brooklyn Nets', 'Orlando Magic', 'Washington Wizards', 'Charlotte Hornets', 
                   'Chicago Bulls', 'New York Knicks', 'Detroit Pistons', 'Atlanta Hawks', 'Cleveland Cavaliers', 'Los Angeles Lakers', 'Los Angeles Clippers', 'Denver Nuggets', 'Houston Rockets', 'Oklahoma City Thunder', 
                   'Utah Jazz', 'Dallas Mavericks', 'Portland Trail Blazers', 'Memphis Grizzlies', 'Phoenix Suns', 'San Antonio Spurs', 'Sacramento Kings', 'New Orleans Pelicans', 'Minnesota Timberwolves', 
                   'Golden State Warriors', 'Vancouver Grizzlies', 'New Orleans Hornets', 'Seattle SuperSonics', 'Charlotte Bobcats', 'New Jersey Nets', 'Washington Bullets')

#listing all the teams abbreviated names
TeamIDHome = TeamIDAway = c('MIL', 'TOR', 'BOS', 'IND', 'MIA', 'PHI', 'BRK', 'ORL', 'WAS', 'CHO', 'CHI', 'NYK', 'DET', 'ATL', 'CLE', 'LAL', 'LAC', 'DEN', 'HOU', 
               'OKC', 'UTA', 'DAL', 'POR', 'MEM', 'PHO', 'SAS', 'SAC', 'NOP', 'MIN', 'GSW', 'VAN', 'NOH', 'SEA', 'CHA', 'NJN', 'WSB')

#creating data frames for the long team name with there abbreviated short name
#Easist to create two, one for home and away, as when merging need to bring in column for both home and away teams
#This is easier as it will bring in the appropriate column names already (home/away team)
TeamsListHome = data.frame(home_team_name, TeamIDHome)
TeamsListAway = data.frame(visitor_team_name, TeamIDAway)

#merging the data frames so now the home and away teams show abbreviated teams
df = merge(df, TeamsListHome, by = "home_team_name")
df = merge(df, TeamsListAway, by = "visitor_team_name")

#Charlotte Hornets changed to the New Orleans Hornets and then the New Orleans Pelicans, 
#and then a new team was added to the league via expansion which was also called the Charlotte Bobcats.
#A issue arises as the Two Charlotte Hornets teams in history had different abbreviated names.
#This needs to be fixed as these abbreviated names is what drives the webscraper for boxscores.
#As above assigns all Charlotte Hornets games to the CHO that the current team is,
#below adjusts the Charlotte Hornets prior to 2005 to be CHH, which corresponds to the correct abbreviated name.
df[df$season < 2005 & df$home_team_name == 'Charlotte Hornets', "TeamIDHome"] = 'CHH'
df[df$season < 2005 & df$visitor_team_name == 'Charlotte Hornets', "TeamIDAway"] = 'CHH'
