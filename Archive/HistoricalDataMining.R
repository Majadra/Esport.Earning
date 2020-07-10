#####################################
#Here I would be mining historical information from EsportsEarnings.com
#in order to upload add more information to my dataset
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

histDf <- data.frame()
for (i in 1998:2020) {
 

webpg <- read_html(paste0("https://www.esportsearnings.com/history/",i,"/games"))
games <- webpg %>% html_nodes(".detail_list_player a") %>% html_text()

prizepool <- webpg %>% html_nodes(".detail_list_player+ .detail_list_prize") %>% html_text()
prizepool <- as.numeric(gsub("[\\$,]","",prizepool))

players <- webpg %>% html_nodes(".detail_list_prize:nth-child(4)") %>% html_text()
players <- as.numeric(gsub("\\D","",players))

tournaments <- webpg %>% html_nodes(".detail_list_prize~ .detail_list_prize+ .detail_list_prize") %>% html_text()
tournaments <- as.numeric(gsub("\\D","",tournaments))

year <- webpg %>% html_nodes("h1") %>% html_text()
year <- gsub("\\D","",year)
year <- rep(year, length(games))
temp_df <- data.frame(Year = year, Earnings = prizepool, Players = players,
                      Tournaments = tournaments, Game = games)

histDf <- rbind(histDf,temp_df)

}
