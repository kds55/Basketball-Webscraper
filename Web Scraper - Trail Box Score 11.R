
#libraries
library(rvest)
library(lubridate)
library(stringi)
library(plyr)

#Setting up identifier for different box scores (basic vs advanced stats)
boxtypelist = c("basic", "advanced")

#Imoprting the game_id's that are looped through and scraped. This is what fills out the website
gamelist = df$game_id

#Setting up empty data frames
boxscores <- data.frame()
boxscoresfinal <- data.frame()


#The beef of the webscraper
#Starts by creating a for loop that cycles through the different game_id's and changes the website/URL for each game
#So this essentially just sets up all the websites its going to scroll through
for (game in gamelist){
  url = paste0("https://www.basketball-reference.com/boxscores/",game,".html")
  webpage = read_html(url)
  #Pulls in the away team and  home team identifiers (short hand) for each game_id and sets it up as the two teams for the current website/link
  #This is needed as the tables in the website are referenced by each teams short hand name (Ex. MIL, CHI, ATL)
  teamlist = c(df[df$game_id == game, "TeamIDAway"],df[df$game_id == game, "TeamIDHome"]) 
  
  #Sets up the loop to cycle through each team on the website and pull there data
  for (teams in teamlist){
    
    #The actual code which runs twice, once each for the basic and advanced stats per team. This is then cycled through 
    #for each team on the website. So the code runs 4 times per URL
    for (boxtype in boxtypelist){
    #Sets up the variables for where the player names exist and where the data table is (stats and headings)
    playernames = paste0("table#box-", teams, "-game-",boxtype," > tbody > tr > th") #Used to be called playertable
    playerdata = paste0("table#box-", teams, "-game-",boxtype," > tbody > tr > td")
    #columns = paste0("table#box-", teams,"-game-",boxtype," > tbody > tr > td") this can be deleted, link col_names below to playerdata table insatead

    #Pulls in and creates a list of the the column names from the tables on the website.
    col_names = webpage %>% 
     html_nodes(playerdata) %>% 
     html_attr("data-stat") 
    col_names = col_names[ col_names != 'reason']
   
    #Gets rid of duplicate column names (MP shows on basic and advanced boxscores)
    col_names = unique(col_names)
  
    #Adds other relevant column names
    col_names = c("player", col_names)
    col_names = c("team", col_names)
    col_names = c("date", col_names)
    col_names = c("game_id", col_names)
    
    #Setting up where the newly added column names will be pulled from/calculated from
    team = teams
    game_id = stri_sub(url, -17, -6)
    dates = stri_sub(game_id, 1, 8)
    
    #Pulling the list of player names
    player = webpage %>% 
      html_nodes(playernames) %>% 
      html_attr("csk") 
    player = player[!is.na(player)] 
  
    #pulling all of the player data and putting it in as a data frame
    data = webpage %>% 
      html_nodes(playerdata) %>% 
      html_text() %>%
      matrix(ncol = length(col_names) -4, byrow = TRUE)
    
    #setting season so it can filter and adjust data frames for plus minus not being recorded prior to 1997 (see below)
    season = unique (df$season [df$game_id == game])
   
    
    #In 1997 they started tracking plus/minus in the boxscores. Below adjusts so if it was before 1997 
    #it adds in plus/minus column and sets it to NA
    if(season < 1997 & boxtype == 'basic'){
      col_names = c("plus_minus", col_names)
      plus_minus = "NA"
    }
 
  
    #Setting up the boxscore. This is done in a temporary one as it is moved to a final one later.
    #This is done so it can be cleared of all player data at the end of each loop and restarted (avoids duplicates)
    #Probably a way I could avoid using a temp one but unsure how to do so as I am pulling basic and advanced stats separate then merging together
    # Wouldn't be able to combine two data frames that dot have the same columns to preexisting boxscores, so need to combine advanced and basic first then move over
    #Also includes a IF forumla to adjust the formula appropriately for 1997 not having plus/minus column already (would screw up merge cause diff # of columns)
    if(season < 1997 & boxtype == 'basic'){
      boxscorestemp = as.data.frame(cbind(plus_minus,game_id, dates, team, player, data), stringsAsFactors = FALSE)
    } else
      boxscorestemp = as.data.frame(cbind(game_id, dates, team, player, data), stringsAsFactors = FALSE)
    
    if (boxtype == 'basic'){
      boxscorestemp ['starting'] = c(c(1,1,1,1,1), rep( 0, sum(boxscorestemp$team == teams)-5))
    }
    
    #Labelling the boxscores column names in the data frame
    names(boxscorestemp) = col_names
    
    #Removing any players who did not play.
    boxscorestemp = boxscorestemp[boxscorestemp$mp != "Did Not Play",]
    boxscorestemp = boxscorestemp[boxscorestemp$mp != "Did Not Dress",]
    boxscorestemp = boxscorestemp[boxscorestemp$mp != "Not With Team",]
    boxscorestemp = boxscorestemp[boxscorestemp$mp != "Player Suspended",]
    
    #Setting up the starting column name. This is only done if the box score is basic to avoid duplicate columns/work done.
    if (boxtype == 'basic'){
      if(season < 1997){
        names(boxscorestemp)[25] = 'starting'
      } else{
      names(boxscorestemp)[25] = 'starting'
      }
    }
    
    #Setting it up so that when it is doing the basic boxscores it sets this up in the empty variable boxscores
    #When it runs through the advanced boxscores it then merges this data into the already existing boxscores (basic)
    if (boxtype == 'advanced'){
      
      #the game 202008150POR does not have BPM on the website for some reason. As its missing this column and causes an error
      #in the rbind, manually inputted it and fill to 0. Likely wont cause a significant difference in our data.
      if(game == '202008150POR'){
        boxscorestemp$bpm = 0
      }
      
      boxscores = merge(boxscores, boxscorestemp, c('player', 'game_id', 'date', 'team', 'mp'))
      
    } else {
      
    boxscores <- rbind(boxscores, boxscorestemp)
    }
    
    #Webscraper has now been completed for one teams basic and advanced boxscores on that URL
    }
    
    #Merging the completed team data into the final boxscores and clearing the temporary one so it is ready for the next team.
    boxscoresfinal <- rbind(boxscoresfinal, boxscores)
    boxscores <- data.frame()
  }
  
  #both teams have now been scraped on this URL. The Scraper now goes to the next game_id to pull the 2 new teams
  
  }


#write.csv(boxscoresfinal,'boxscores4.csv')



