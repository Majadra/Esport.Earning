#####################################
#Here I would be mining information from EsportsEarnings.com
#in order to upload dataset  kaggle
#
#Auther - Ran K.
#July 2020
#####################################

#Installing Packages
library(tidyverse)
library(stringr)
library(rvest)

################
#Text mining
################

initial_link <- "https://www.esportsearnings.com/games/browse-by-genre"
webpg <- read_html(initial_link)
#Preparing list of links for each game
link_list <- paste("https://www.esportsearnings.com",webpg %>% html_nodes(".games_main_game_title a") %>% html_attr("href"),sep = "")

#The list of games is actually not necessary as we obtien a name for each game in its proper page.
#game_list <- webpg %>% html_nodes(".games_main_game_title a") %>% html_text()

game_stats <- webpg %>% html_nodes(".games_main_game_stats") %>% html_text()
#game_stats variable will need some treatment before we add to the database

genres <- webpg %>% html_nodes(".games_main_genre_header span") %>% html_text()
#genres variable will need some treatment before we add to the database

#Preparing an empty df to directly recieve the data that I will mine
df <- data.frame()

#Looping throgh each game and mining more information
for (i in 1:length(link_list)) {
  game_pg <- read_html(link_list[i])
  
  
  #Name of game
  df[i,"Name"] <- game_pg %>% html_node(".info_box_title") %>% html_text()
  
  #Release year
  df[i,"ReleaseYear"] <- game_pg %>% html_node(".format_row:nth-child(1) .info_text_value") %>% html_text()
  
  #Largest tournement prize pool (will be taken from the game webpage)
  df[i,"LargestPrizePool"] <- game_pg %>% html_node(".highlight:nth-child(2) .detail_list_tournament+ .detail_list_prize") %>% html_text()
  
  #Name of biggest tourny - as tournemens could be one of two types of 
  # links, ill use an if statement
  if (!is.na(game_pg %>% html_node(".highlight:nth-child(2) .detail_list_tournament a+ a") %>% html_text())) {
    df[i,"LargestTournementName"] <- game_pg %>% html_node(".highlight:nth-child(2) .detail_list_tournament a+ a") %>% html_text()
    tourny_link <- game_pg %>% html_node(".highlight:nth-child(2) .detail_list_tournament a+ a") %>% html_attr("href")
  }
  else {
    df[i,"LargestTournementName"] <- game_pg %>% html_node(".highlight:nth-child(2) .detail_list_tournament a") %>% html_text()
    tourny_link <- game_pg %>% html_node(".highlight:nth-child(2) .detail_list_tournament a") %>% html_attr("href")
  }
  
  #If the is a tournement we gather more information about it
  if (!is.na(tourny_link)) {
    
    tourny_link <- paste("https://www.esportsearnings.com",tourny_link,sep = "")
    tourny_pg <- read_html(tourny_link)
    #Tournement date
    df[i,"DateOfTounement"] <- tourny_pg %>% html_node("time") %>% html_text()
  
    #Tournement location
    df[i,"LocationOfTournement"] <- tourny_pg %>% html_node(".info_box_title+ .info_box_inner .format_row:nth-child(1) .info_text_value") %>% html_text()
  }
}  

##############################
#Treating game_stats variable
##############################

  ##As game_stats  is actully a string containing 3 different variables, ill split it first then add to the database.
  #Splitting into 3 variables
TotalPrizePool <- regexpr("(?<=\\$)(.*?)\\.\\d{2}",game_stats,perl = TRUE)
TotalPrizePool <- regmatches(game_stats,TotalPrizePool)
TotalPlayers<- regexpr("(?<=\\d{2})\\d*(?= Pl)",game_stats,perl = TRUE)
TotalPlayers <- regmatches(game_stats,TotalPlayers)
TotalTournements <- regexpr("(?<=ers|er)\\d*(?= T)",game_stats,perl = TRUE)
TotalTournements <- regmatches(game_stats,TotalTournements)

#Adding the variables into the database

df["TotalPrizePool"] <- TotalPrizePool
df["TotalPlayers"] <- TotalPlayers
df["TotalTournements"] <- TotalTournements

############################
#Treating and adding the genre variable
############################

#Creating a varibale which will will contain the list of genres
gameGenres <- as.character()
#using a loop i will duplicate the string indicating the
# genre based on the number of games in each category

for (i in seq(1,length(genres),2)) {
  temp <- as.numeric(regmatches(genres[i+1],regexpr("\\d*",genres[i+1])))
  gameGenres <- append(gameGenres,rep(genres[i],num))
}

# Adding to database.
df["genre"] <- gameGenres

#Writing to a new csv file
write.csv(df,"EsportEarning.csv", row.names = FALSE)

