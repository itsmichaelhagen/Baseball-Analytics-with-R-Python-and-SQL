library(baseballr)
library(dplyr)
library(tidyr)
library(forecast)
library(ggplot2)

# SIMPLE LINEAR REGRESSION MODEL ANALYSIS OF PAST WEEK GAMES - HITTER FORECAST
## Still needs a lot of work

#Working Directory
setwd("~/Documents/Data Analytics/Baseball-Analytics-with-R-Python-and-SQL")

#Establish teams to select players

getMLBTeams <- function() {
  allTeams <<- mlb_teams(season = 2024)
  mlbTeams <<- allTeams[(allTeams$sport_id %in% 1),]
  tripleaTeams <<- allTeams[(allTeams$sport_id %in% 11),]
  doubleaTeams <<- allTeams[(allTeams$sport_id %in% 12),]
}

getRoster <- function (t) {
  currentRoster <<- mlb_rosters(team_id = t, season = 2024, roster_type = 'active')
}

plot_forecast_10_game <- function() {
  my_linear_model <- lm(hits~game, data = hitter_forecast_10_games)
  
  my_forecast <- forecast(my_linear_model, nwd_10, lambda = 0, biasadj = FALSE, h = 10)
  
  plot(my_forecast, type = "l", main = games_10_title, xlab = "Hits", ylab = "Game")
}

fetch_mlb_player_stats <- function() {
  batter_stats <<- data.frame()
  
  #for (i in gamesPlayedIDs) {
  #batter_stats <<- bind_rows(batter_stats, mlb_player_game_stats(person_id = p, game_pk = i))
  #}
  
  batter_stats <<- fg_batter_game_logs(fangraphsId, 2024)
  
  hitter_forecast_avg <<- data.frame(
    set = c(1, 2, 3, 4, 5),
    hits = c(mean(batter_stats$H[21:25]), mean(batter_stats$H[16:20]), mean(batter_stats$H[11:15]), mean(batter_stats$H[6:10]), mean(batter_stats$H[1:5]))
  )
  
  hitter_forecast_10_games <<- data.frame(
    game = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30),
    hits = rev(batter_stats$H[1:30])
  )
  
  nwd_avg <<- data.frame(set = c(6, 7))
  nwd_10 <<- data.frame(game = c(31, 32, 33, 34, 35, 36, 37, 38, 39, 40))
  
  firstname <<- (selectedPlayer$name_first)
  lastname <<- (selectedPlayer$name_last)
  games_10_title <<- paste0(firstname," ", lastname, " - Batter Forecast")
}

selectPlayer <- function(id) {
  p <<- id
  
  selectedPlayer <<- players[(players$key_mlbam %in% p),]
  fangraphsId <<- selectedPlayer$key_fangraphs
  bbrefId <<- selectedPlayer$key_bbref
  bbrefminorsId <<- selectedPlayer$key_bbref_minors
}

#Set date
t <- "2024-07-26"

#Load PBP to find games played in
load("data/pbp.Rdata")

#Load Teams
getMLBTeams()

#Choose Team
getRoster(134)

#Select Player from currentRoster (use "person_id")
## Nick Gonzales: 693304
selectPlayer(693304)

#Get game stats for games played
fetch_mlb_player_stats()

#Using past 30 games to forecast hits for next 10 games
plot_forecast_10_game()
