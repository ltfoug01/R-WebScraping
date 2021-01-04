###### EPL_Standings_v2 #####
"
Luke Fougerousse
EPL_Standings Function

  Description: 
  The following R-Script allows the user to enter a date and English Premier
  League season in the EPL_Standings function call at the bottom, which will
  return the EPL standings for the date and season specified.
  
  Parameters: (date,season) 
  Call Example: EPL_Standings('04/25/2017', '2016/17')
  
  Output:
    TeamName
    Record
    HomeRec
    AwayRec
    MatchesPlayed
    Points
    Points per Match (PPM)
    Point Percentage (PtPct)
    Goals Scored (GS)
    Goals Scored per Match (GSM)
    Goals Allowed (GA)
    Goals Allowed per Match (GAM)
    Team's record over the last 10 games played (Last 10)
    Team's current streak (Streak)
"

########## EPL STANDINGS FUNCTION ##########
EPL_Standings <- function(date, season) {
  
  #libraries
  library(tidyverse)
  library(lubridate)
  library(dplyr)
  
  new_date = as.Date(date, '%m/%d/%Y') #converts date parameter to date type
  
  season <- str_remove(str_extract(season, '\\d\\d\\/\\d\\d'), '\\/') #retrieves correct season format to add to URL

  epl_url <- str_c("http://www.football-data.co.uk/mmz4281/", season, "/E0.csv") #build the correct EPL stats URL 
  
  
  #Read csv from URL into a data frame

  #ADD TEAM RECORD COUNTS, POINTS FOR EACH GAME, AND SEASON---------------------------------------------------------
  EPL_df <- read_csv(url(epl_url)) %>%
    select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) %>%
    mutate(Date = dmy(Date), #Parse Date
           
           Win_Count_Home = ifelse(FTR == 'H', 1, 0),   #Win Counter based on Home wins
           Draws = ifelse(FTR == 'D', 1, 0),            #Draw counter 
           Loss_Count_Home = ifelse(FTR == 'A', 1, 0),  #Loss Counter based on Away wins
           
           Points_HT = ifelse(FTR == 'H', 3, ifelse(FTR == 'D', 1, ifelse(FTR == 'A', 0, NA))), #Home Points
           Points_AT = ifelse(FTR == 'A',3, ifelse(FTR == 'D', 1, ifelse(FTR == 'H', 0, NA))),  #Away Points
           )  
  

  #EPL_HOME contains the Stats as the Home Team--------------------------------------------------------------------
  EPL_HOME <- EPL_df %>%
    select(Date, HomeTeam, FTHG, FTAG, FTR, Win_Count_Home, Draws, Loss_Count_Home, Points_HT) %>%
    filter(Date <= new_date) %>%
    group_by(TeamName = HomeTeam) %>%
    summarise(Games_Played = n(),               #Games played at home
              Points = sum(Points_HT),          #Points at home
              Wins = sum(Win_Count_Home),       #Home Wins
              Losses = sum(Loss_Count_Home),    #Home Losses
              Draws = sum(Draws),               #Home Draws
              GS = sum(FTHG),                   #Goals scored at home
              GA = sum(FTAG)                    #Goals allowed at home
    ) 
  
  #EPL Away contains the Stats as the Away Team--------------------------------------------------------------------
  EPL_AWAY <- EPL_df %>%
    select(Date, AwayTeam, FTHG, FTAG, FTR, Win_Count_Home, Draws, Loss_Count_Home, Points_AT) %>%
    filter(Date <= new_date) %>%
    group_by(TeamName = AwayTeam) %>%
    summarise(Games_Played = n(),               #Games played as away team
              Points = sum(Points_AT),          #Points scored as away team
              Wins = sum(Loss_Count_Home),      #Wins as away team
              Losses = sum(Win_Count_Home),     #Losses as away team
              Draws = sum(Draws),               #Draws as away team
              GS = sum(FTAG),                   #Goals scored as away team
              GA = sum(FTHG)                    #Goals allowed as away team
    )
  
  

  #Join the two data frames into one table - adds home and away stats
  #Calculate W/L/D, Matches Played, Points, PPM, PtPct, GS, GSM, GA, GAM-------------------------------------------
  EPL_ALL <- EPL_HOME %>%
    full_join(EPL_AWAY, by = c('TeamName')) %>%
    mutate(Wins = Wins.x + Wins.y,                                
           Losses = Losses.x + Losses.y,                          
           Draws = Draws.x + Draws.y,                             
           Matches_Played = Games_Played.x + Games_Played.y,       
           Points = Points.x + Points.y,
           PPM = round(Points / Matches_Played, 2),
           PtPct = round(Points / (3 * Matches_Played), 2),
           GS = GS.x + GS.y,
           GSM = round(GS / Matches_Played, 2),
           GA = GA.x + GA.y,
           GAM = round(GA / Matches_Played, 2)) %>%
    select(TeamName, Matches_Played, Wins, Losses, Draws, Points, PPM, PtPct, GS, GSM, GA, GAM)
  
  
  
  
  
  #Gather the last 10 games for home and away games----------------------------------------------------------------
  HomeRecords <- EPL_df %>%
    select(Date, TeamName = HomeTeam, Win = Win_Count_Home, Loss = Loss_Count_Home, Draws) %>%
    filter(Date <= new_date) 
  
  AwayRecords <- EPL_df %>%
    select(Date, TeamName = AwayTeam, Win = Loss_Count_Home, Loss = Win_Count_Home, Draws) %>%
    filter(Date <= new_date) 
  
  last10_Total <- rbind(HomeRecords, AwayRecords) %>%
    group_by(TeamName) %>%
    top_n(10, wt = Date) %>% #Last 10 games based on the date column
    summarise(Wins = sum(Win),
              Losses = sum(Loss),
              Draw = sum(Draws)) %>%
    mutate(Last10 = str_c(Wins, '-', Losses, '-', Draw)) %>% #Last 10 record
    select(TeamName, Last10)
  
  
  
  #Get the Win/Loss Streak-----------------------------------------------------------------------------------------
  streak <- rbind(HomeRecords, AwayRecords) %>%
    arrange(TeamName, desc(Date)) %>%
    mutate(outcome = ifelse(Win == 1, "W", NA),
           outcome = ifelse(Loss == 1, "L", outcome),
           outcome = ifelse(Draws == 1, "D", outcome)) %>%
    group_by(TeamName) %>%
    mutate(outcome_lag = ifelse(outcome == lag(outcome), 1, 0)) %>%  #outcome_lag variable for streak calculation
    summarise(streak_type = outcome[min(row_number())],
              streak_len = min(which(outcome_lag == 0)) - 1) %>%
    mutate(Streak = str_c(streak_type, streak_len)) %>%
    select(TeamName, Streak)
  
  
  #EPL Standings without Last10 and Streak-------------------------------------------------------------------------
  Standings <- EPL_ALL %>%
    mutate(Record = str_c(EPL_ALL$Wins, '-', EPL_ALL$Losses, '-', EPL_ALL$Draws),           #Show Record
           HomeRec = str_c(EPL_HOME$Wins, '-', EPL_HOME$Losses, '-', EPL_HOME$Draws),       #Home Record
           AwayRec = str_c(EPL_AWAY$Wins, '-', EPL_AWAY$Losses, '-', EPL_AWAY$Draws)) %>%   #Away Record
    select(TeamName, Record, HomeRec, AwayRec, Matches_Played, Wins, Losses, Draws,
           Points, PPM, PtPct, GS, GSM, GA, GAM) %>%
    arrange(desc(PPM), desc(Wins), desc(GSM), GAM)             #Sorting by PPM,Wins,GSM, & GAM
  
  
  #Returns the final results with the last 10 games and Streak-----------------------------------------------------
  EPL_Standings <- Standings %>%
    select(TeamName, Record, HomeRec, AwayRec, Matches_Played, Points, PPM, PtPct, GS, GSM, GA, GAM) %>%
    inner_join(last10_Total, by = 'TeamName') %>%              #Join the final standings with the last 10 games  
    inner_join(streak, by = 'TeamName') %>%               
    arrange(desc(PPM), desc(Standings$Wins), desc(GSM), GAM)   #Sort
  
  
  view(EPL_Standings)
  return(EPL_Standings)
}


#Tests the function
EPL_Standings('04/25/2017', '2016/17') #(mm/dd/yyyy , yyyy/yy)

