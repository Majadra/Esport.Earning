#####################################
#Here I would be cleaning the dataset to make it more raedable
#
#Auther - Ran K.
#July 2020
#####################################


#Cleaning Environment
rm(list=ls())

#Installing Packages
library(tidyverse)

#Loading dataframe
df <- read.csv("EsportEarning.csv")

###################
#Treating ReleaseYear
###################

unique(df$ReleaseYear)
#11 is a wierd value, ill also check 1981 and 1989

#checking 11
df[which(df$ReleaseYear==11),1:3]
df[which(df$ReleaseYear==11),"ReleaseYear"] <- 2011 #checked online

#checking the others
df[which(df$ReleaseYear==1981),1:3]
df[which(df$ReleaseYear==1989),1:3]
#these are Ok - pacman and tetris

###################
#Treating LargestPrizePool
###################

#Changing the name
names(df)[names(df) == "LargestPrizePool"] <- "BiggestPrizePool"
#Removing $ and , sign and chaning format to numeric
df$BiggestPrizePool <- as.numeric(gsub("[$,]","",df$BiggestPrizePool,perl = TRUE))

###################
#Treating LargestTournamentName
###################

#Changing the name
names(df)[names(df) == "LargestTournementName"] <- "BiggestTournament"

###################
#Treating DateOfTounement
###################

#small typo first
names(df)[names(df) == "DateOfTounement"] <- "TournamentDate"

#reformatting to date
df$TournamentDate <- as.Date(df$TournamentDate)

#checking for extreme values
table(df$TournamentDate)

###################
#Treating LocationOfTournement
###################
names(df)
#Small typo first
names(df)[names(df) == "LocationOfTournement"] <- "TournamentLocation"

#Changing to lowercase to help with matching
df$TournamentLocation <- stringr::str_to_lower(df$TournamentLocation)

#Capturing induvidual countries
df$TournamentLocation[which(grepl("usa\\s|usa$|\\sus\\s|united states",df$TournamentLocation))] <- "united states"
df$TournamentLocation[which(grepl("atlanta|philadel|ohio|los angeles|california|new york|florida",df$TournamentLocation))] <- "united states"
df$TournamentLocation[which(grepl("\\s[[:lower:]]{2}$",df$TournamentLocation))] <- "united states"
df$TournamentLocation[which(grepl("minneapolis|chicago|san diego|anaheim|richmond|santa ana",df$TournamentLocation))] <- "united states"
df$TournamentLocation[which(grepl("boston|\\sca$|las vegas|elmhurst|wisconsin",df$TournamentLocation))] <- "united states"
df$TournamentLocation[which(grepl("melbourne",df$TournamentLocation))] <- "australia"
df$TournamentLocation[which(grepl("china|shanghai",df$TournamentLocation))] <- "china"
df$TournamentLocation[which(grepl("japan",df$TournamentLocation))] <- "japen"
df$TournamentLocation[which(grepl("canada|montr.{1}al|toronto",df$TournamentLocation))] <- "canada"
df$TournamentLocation[which(grepl("france",df$TournamentLocation))] <- "france"
df$TournamentLocation[which(grepl("germany",df$TournamentLocation))] <- "germany"
df$TournamentLocation[which(grepl("south korea|korea|seoul",df$TournamentLocation))] <- "south korea"
df$TournamentLocation[which(grepl("london|uk|united kingdom|england",df$TournamentLocation))] <- "united kingdom"
df$TournamentLocation[which(grepl("brazil",df$TournamentLocation))] <- "brazil"
df$TournamentLocation[which(grepl("italy",df$TournamentLocation))] <- "italy"
df$TournamentLocation[which(grepl("spain",df$TournamentLocation))] <- "spain"
df$TournamentLocation[which(grepl("poland",df$TournamentLocation))] <- "poland"
df$TournamentLocation[which(grepl("singapore",df$TournamentLocation))] <- "singapore"
df$TournamentLocation[which(grepl("malaysia",df$TournamentLocation))] <- "malaysia"
df$TournamentLocation[which(grepl("thailand",df$TournamentLocation))] <- "thailand"
df$TournamentLocation[which(grepl("sweden",df$TournamentLocation))] <- "sweden"
df$TournamentLocation[which(grepl("turkey",df$TournamentLocation))] <- "turkey"
df$TournamentLocation[which(grepl("taiwan",df$TournamentLocation))] <- "taiwan"
df$TournamentLocation[which(grepl("finland",df$TournamentLocation))] <- "finland"
df$TournamentLocation[which(grepl("netherlands",df$TournamentLocation))] <- "netherlands"
df$TournamentLocation[which(grepl("switzerland",df$TournamentLocation))] <- "switzerland"
df$TournamentLocation[which(grepl("belarus",df$TournamentLocation))] <- "belarus"
df$TournamentLocation[which(grepl("denmark",df$TournamentLocation))] <- "denmark"
df$TournamentLocation[which(grepl("romania",df$TournamentLocation))] <- "romania"
df$TournamentLocation[which(grepl("qatar",df$TournamentLocation))] <- "qatar"
df$TournamentLocation[which(grepl("uae$",df$TournamentLocation))] <- "uae"
df$TournamentLocation[which(grepl("viet nam",df$TournamentLocation))] <- "vietnam"


#some specific corrections
df$TournamentLocation[which(grepl("n/a",df$TournamentLocation))] <- "united states" #forza 6 - I googled

df$TournamentLocation[which(grepl("online/off",df$TournamentLocation))] <- "online"
#Bloodline Champions, couldnt find

df$TournamentLocation[which(grepl("online \\(twitch\\)",df$TournamentLocation))] <- "online"
df$TournamentLocation[which(grepl("worldwide)",df$TournamentLocation))] <- "online" #osu world championship

# I used these two expressions to help me fish for inaccurate names
df$TournamentLocation[which(grepl("[[:lower:]]{2,}, [[:lower:]]{2,}",df$TournamentLocation))]
df$TournamentLocation[which(grepl("\\s[[:lower:]]{2}$",df$TournamentLocation))]

unique(df$TournamentLocation)
#seems ok

table(df$TotalTournements[which(is.na(df$TournamentLocation))]) #all na values are true na


#Variable is finally ready

###################
#Treating TotalPrizePool
###################

#Removing $ and , sign and chaning format to numeric
df$TotalPrizePool <- as.numeric(gsub("[$,]","",df$TotalPrizePool,perl = TRUE))


###################
#Treating TotalTournements
###################

table(df$TotalTournements)
# Ill check the two extreme values

df[which(df$TotalTournements==5083),1:3] #counter strike, makes sense
df[which(df$TotalTournements==5865),1:3] #starcraft2, makes sense

# correcting typo
names(df)[names(df) == "TotalTournements"] <- "TotalTournaments"

###################
#Treating genre
###################

# correcting typo
names(df)[names(df) == "genre"] <- "Genre"

# changing to factor
df$Genre <- as.factor(df$Genre)

###################
#Reordering columns
###################

col_order <- c("Name","ReleaseYear","Genre","TotalPrizePool","TotalTournaments","TotalPlayers",
               "BiggestTournament","BiggestPrizePool","TournamentDate","TournamentLocation")

df_ordered <- df[,col_order]

#Writing to a new csv file
write.csv(df_ordered,"EsportEarning_clean.csv", row.names = FALSE)
