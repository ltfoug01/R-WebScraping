# WEBSCRAPING AND DATABASES IN R

library(tidyverse)
library(rvest) #part of tidyverse


#BUILD THE FLIMS TABLE FROM WIKIPEDIA------------------------------------------


# scrape academy award nominees from wikipedia
aa_films_raw <- read_html("https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture") %>% 
  html_table(fill = T) #extracts all the tables from the web page's html source code

aa_films <- do.call(bind_rows, aa_films_raw) %>% 
  select(Year, Film) %>% 
  filter(!is.na(Film)) %>%                          #removes all NA values
  mutate(Year = str_extract(Year, "^\\d{4}")) %>%   #get the year - first 4 digits
  group_by(Year) %>% 
  mutate(winner_index = rank(row_number()),
         Winner = ifelse(winner_index == min(winner_index), 1, 0)) %>% #shows the winners from each year
  select(-winner_index)


#BUILD THE ROTTEN TOMATOES TABLE-----------------------------------------------

# scrape rotten tomatoes info for each film - empty df the size of aa_films
rotten_tomatoes_scores <- data.frame(Tomatometer = rep(NA, length(aa_films$Film)), 
                                     Audience_score = rep(NA, length(aa_films$Film)))

for(i in seq_along(aa_films$Film)){
  
  # handle errors and continue looping - some URLs might cause an error
  tryCatch({
    
    #Example: https://www.rottentomatoes.com/m/joker_2019
    # for each film title, convert to rotten tomatoes URL suffix format
    #to lowercase and replace space with underscore(_)
    film_url_suffix <- tolower(str_replace_all(aa_films$Film[i], "\\s", "_"))
    
    # build the URL
    rt_url <- paste0("https://www.rottentomatoes.com/m/", film_url_suffix)
    
    # NULL page contents object
    page <- NULL
    
    # scrape page info for URL
    page <- read_html(rt_url)
    
    # if page is null then create missing values
    if(is.null(page)){
      rotten_tomatoes_scores$Tomatometer[i] <- NA
      rotten_tomatoes_scores$Audience_score[i] <- NA
      
      # if page was scrape successfully, then extract the ratings info     
    } else {
      
      ratings1 <- page %>% 
        html_nodes(".scoreboard") %>%  #scoreboard node
        html_attr('tomatometerscore')  #tomato score attribute
      
      ratings2 <- page %>% 
        html_nodes(".scoreboard") %>%  #scoreboard node
        html_attr('audiencescore')     #audience score attribute
      
      rotten_tomatoes_scores$Tomatometer[i] <- ratings1
      rotten_tomatoes_scores$Audience_score[i] <- ratings2
    }
    
    print(i)
    
  }, error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
  
}


# create film IDs for both tables
rotten_tomatoes_scores$Film_ID <- 1001:(1000 + length(aa_films$Film))

aa_films$Film_ID <- 1001:(1000 + length(aa_films$Film))

# Merges the two tables into one.
Films_and_Scores <- merge(aa_films, rotten_tomatoes_scores, by = 'Film_ID')



# WORKING WITH DATABASES AND SQL IN R------------------------------------------

library(DBI)
library(odbc)
library(RSQLite)
library(dbplyr)

# The dbConnect function from the DBI package allows us to create a SQLite database
# directly from R.
# SQLite databases are saved as files in the current working directory with this method. 
# As described 
# in the RSQLite package vignette, if you simply want to use a temporary database, 
# you can create either an on-disk database or an in-memory database with this same method. 
# For this example, we will create a new SQLite in-memory database.

# connect to a database (can be MYSQL, etc.) - creates an empty database
filmDB <- dbConnect(SQLite(), ":memory:")  

dbListTables(filmDB)  # show tables in connected db - will show no tables


# writing film tables to a database
dbWriteTable(filmDB, "aa_films", aa_films)
dbWriteTable(filmDB, "film_ratings", rotten_tomatoes_scores)

dbListTables(filmDB)  # check that tables were written into film db


# query the database - all films with academy award
dbGetQuery(filmDB, "SELECT film 
                 FROM aa_films 
                 WHERE winner == 1")

dbGetQuery(filmDB, 'SELECT a.Film, r.Tomatometer, r.Audience_score
                 FROM   aa_films a INNER JOIN film_ratings r ON a.Film_ID =
                        r.Film_ID
                 WHERE  winner == 1')


# Using dbplyr instead of SQL queries------------------------------------------

# in addition to querying a SQL database directly by passing SQL statements
# into the dbGetQuery function, the dbplyr package (related to dplyr) allows
# R users to avoid having to write SQL queries at all.

# the dbplyr package allows users to create a reference to a sql table and 
# interact with it using dplyr verbs



# first create the reference to the SQL table using tbl()
aa_films_tbl <- tbl(filmDB, "aa_films")


# then use dplyr like normal
year_summary <- aa_films_tbl %>% 
  select(Film_ID, Film, Year, winner) %>% 
  filter(Year > 1945) %>% 
  group_by(Year, winner) %>% 
  count() 

# behind the scenes, dbplyr writes your SQL for you
year_summary %>% show_query()

# until you tell dbplyr to return the full query, using collect, it only 
# evaluates the query lazily
year_summary <- aa_films_tbl %>% 
  select(Film_ID, Film, Year, winner) %>% 
  filter(Year > 1945) %>% 
  group_by(Year, winner) %>% 
  count() %>% 
  collect() %>% # collect is important because it returns the full query result
  as_tibble()
