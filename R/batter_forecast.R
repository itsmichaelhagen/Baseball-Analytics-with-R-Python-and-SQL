library(baseballr)
library(dplyr)
library(tidyr)
library(forecast)
library(ggplot2)

# SIMPLE LINEAR REGRESSION MODEL ANALYSIS OF NEXT 6 GAMES - HITTER FORECAST
## Still needs a lot of work

#Working Directory
setwd("~/Documents/Data Analytics/Baseball-Analytics-with-R-Python-and-SQL")

#Establish teams to select players

getMLBTeams <- function() {
  mlbTeams <<- mlb_teams(season = 2024)
  mlbTeams <<- mlbTeams[(mlbTeams$sport_id %in% 1),]
}

getRoster <- function (t) {
  currentRoster <<- mlb_rosters(team_id = t, season = 2024, roster_type = 'active')
}

getRoster(112)

#Select Player from currentRoster (use "person_id")
## Dansby Swanson: 621020

selectPlayer <- function(id) {
  p <<- id
}

selectPlayer(621020)

#Load PBP to find games played in
load("data/pbp.Rdata")

#Select games played
gamesPlayed <<- majorleague_pbp[(majorleague_pbp$matchup.batter.id %in% p),]

gamesPlayedIDs <<- as.vector(gamesPlayed$game_pk)
gamesPlayedIDs <- unique(gamesPlayedIDs)

#Get game stats for games played
fetch_player_stats <- function() {
  batter_stats <<- data.frame()
  
  for (i in gamesPlayedIDs) {
    batter_stats <<- bind_rows(batter_stats, mlb_player_game_stats(person_id = p, game_pk = i))
  }

  batter_stats <<- batter_stats[(batter_stats$group %in% "hitting"),]
  
  hitter_forecast <<- data.frame(
    game = c(1,2,3,4,5,6),
    ba = (batter_stats$hits)/(batter_stats$at_bats)
  )
  
  nwd <<- data.frame(game = c(7,8,9,10,11,12))
}

fetch_player_stats()

my_linear_model <- lm(ba~game, data = hitter_forecast)

my_forecast <- forecast(my_linear_model, nwd)

autoplot(my_forecast)
