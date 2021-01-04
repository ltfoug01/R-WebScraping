##### NBA_Team_Stats_2021 Function #####
"

Luke Fougerousse
NBA_Team_Stats_2021 
  
  Description:
  The following R-script contains a function that allows the user to retrieve
  the 2021 per game stats of an NBA team of their choice. The function is
  called using the abbreviation of the team of choice as a parameter.
  
  Example: NBA_Team_Stats_2021('POR') for the Portland Trailblazers
  
"


NBA_Team_Stats_2021 <- function(team_abbreviations){
  
  library(tidyverse)
  library(dplyr)
  library(rvest)
  
  #create team URL
  team_url <- str_c('https://www.basketball-reference.com/teams/', team_abbreviations,'/2021/gamelog/')
  
  
  #Get the teams stats for each game of the 2021 season from basketball reference-------------------------------
  raw_data <- read_html(team_url) %>%
    html_table(fill = T, header = F)    #extracts table elements
  
  df <- data.frame(raw_data)[-1,]       #put data into table and remove first row
  
  
  #Create Pacers Game Log and Per Game Stats--------------------------------------------------------------------
  team <- df %>%
    select(X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24)
  
  row1_team <- head(team, 1)                          #get correct header from 1st row
  
  Team_Game_Log <- team[-1,]                          #remove first row
  
  cols.num <- c('X2','X7','X8','X9','X10','X11','X12','X13','X14','X15','X16','X17','X18','X19',
                'X20', 'X21', 'X22', 'X23', 'X24')    #cols to change to numeric for calculations
  
  Team_Game_Log[cols.num] <- lapply(Team_Game_Log[cols.num], as.numeric)   
  
  
  Team_per_Game <- Team_Game_Log %>%                  #Calculate Per Game Stats
    group_by() %>%
    summarise(Games_Played = n(),
              Points = round(mean(X7), digits = 1),
              PA = round(mean(X8), digits = 1),
              FG = round(mean(X9), digits = 1),
              FGA = round(mean(X10), digits = 1),
              'FG%' = round(mean(X11), digits = 3),
              '3' = round(mean(X12), digits = 1),
              '3PA' = round(mean(X13), digits = 1),
              '3P%' = round(mean(X14), digits = 3),
              FT = round(mean(X15), digits = 1),
              FTA = round(mean(X16), digits = 1),
              'FT%' = round(mean(X17), digits = 3),
              ORB = round(mean(X18), digits = 1),
              TRB = round(mean(X19), digits = 1),
              AST = round(mean(X20), digits = 1),
              STL = round(mean(X21), digits = 1),
              BLK = round(mean(X22), digits = 1),
              TOV = round(mean(X23), digits = 1),
              PF = round(mean(X24), digits = 1))
  
  
  colnames(Team_Game_Log) <- c(row1_team)             #replace header
  
  Team_Game_Log <- Team_Game_Log %>%
    rename('H/A' = '', 'Opponent' = 'Opp')            #rename blank column and Opp
  
  
  
  #Create Opponents Game Log------------------------------------------------------------------------------------
  opp <- df %>%
    select(X2, X3, X4, X5, X6, X7, X8, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, X40, X41)
  
  row1_opp <- head(opp, 1)                            #get correct header from 1st row
  
  Opp_Game_Log <- opp[-1,]
  
  cols.num <- c('X2', 'X7','X8','X26','X27','X28','X29','X30','X31','X32','X33','X34','X35','X36',
                'X37', 'X38', 'X39', 'X40', 'X41')    #cols to change to numeric for calculations
  
  Opp_Game_Log[cols.num] <- lapply(Opp_Game_Log[cols.num], as.numeric)
  
  
  Opponent_per_Game <- Opp_Game_Log %>%               #Calculate Per Game Stats for Opponents
    group_by() %>%
    summarise(Games_Played = n(),
              Points = round(mean(X8), digits = 1),
              PA = round(mean(X7), digits = 1),
              FG = round(mean(X26), digits = 1),
              FGA = round(mean(X27), digits = 1),
              'FG%' = round(mean(X28), digits = 3),
              '3' = round(mean(X29), digits = 1),
              '3PA' = mean(X30),
              '3P%' = round(mean(X31), digits = 3),
              FT = round(mean(X32), digits = 1),
              FTA = round(mean(X33), digits = 1),
              'FT%' = round(mean(X34), digits = 3),
              ORB = round(mean(X35), digits = 1),
              TRB = round(mean(X36), digits = 1),
              AST = round(mean(X37), digits = 1),
              STL = round(mean(X38), digits = 1),
              BLK = round(mean(X39), digits = 1),
              TOV = round(mean(X40), digits = 1),
              PF = round(mean(X41), digits = 1))
  
  
  colnames(Opp_Game_Log) <- c(row1_opp)        #replace header
  
  Opp_Game_Log <- Opp_Game_Log %>%             #rename blank column & Opponent
    rename('H/A' = '', 'Opponent' = 'Opp')
  
  
  #Display Team per Game Stats and Opponent per Game Stats------------------------------------------------------
  view(Team_per_Game)
  view(Opponent_per_Game)
}


NBA_Team_Stats_2021('POR')

"
ATL	- Atlanta Hawks
BKN	- Brooklyn Nets
BOS	- Boston Celtics
CHA	- Charlotte Hornets
CHI	- Chicago Bulls
CLE	- Cleveland Cavaliers
DAL	- Dallas Mavericks
DEN	- Denver Nuggets
DET	- Detroit Pistons
GSW	- Golden State Warriors
HOU	- Houston Rockets
IND	- Indiana Pacers
LAC	- Los Angeles Clippers
LAL	- Los Angeles Lakers
MEM	- Memphis Grizzlies
MIA	- Miami Heat
MIL	- Milwaukee Bucks
MIN	- Minnesota Timberwolves
NOP	- New Orleans Pelicans
NYK	- New York Knicks
OKC	- Oklahoma City Thunder
ORL	- Orlando Magic
PHI	- Philadelphia 76ers
PHX	- Phoenix Suns
POR	- Portland Trail Blazers
SAC	- Sacramento Kings
SAS	- San Antonio Spurs
TOR	- Toronto Raptors
UTA	- Utah Jazz
WAS	- Washington Wizards
"











