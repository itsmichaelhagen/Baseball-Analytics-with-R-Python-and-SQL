library(baseballr)
library(dplyr)
library(tidyr)
library(forecast)
library(ggplot2)

# SIMPLE LINEAR REGRESSION MODEL ANALYSIS OF PAST WEEK GAMES - HITTER FORECAST
## Available for MLB, AAA and AA players

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

getGamesPlayedML <- function() {
  gamesPlayedML_df <<- majorleague_pbp[(majorleague_pbp$matchup.batter.id %in% mlbid),]
  gamesPlayedML <<- as.vector(gamesPlayedML_df$game_pk)
  gamesPlayedML <<- unique(gamesPlayedML)
}

getGamesPlayedAAA <- function() {
  gamesPlayedAAA_df <<- triplea_pbp[(triplea_pbp$matchup.batter.id %in% mlbid),]
  gamesPlayedAAA <<- as.vector(gamesPlayedAAA_df$game_pk)
  gamesPlayedAAA <<- unique(gamesPlayedAAA)
}

getGamesPlayedAA <- function() {
  gamesPlayedAA_df <<- doublea_pbp[(doublea_pbp$matchup.batter.id %in% mlbid),]
  gamesPlayedAA <<- as.vector(gamesPlayedAA_df$game_pk)
  gamesPlayedAA <<- unique(gamesPlayedAA)
}

plot_forecast_avg <- function() {
  my_linear_model <<- lm(bavg~game, data = hitter_forecast_avg)
  
  my_forecast <<- forecast(my_linear_model, nwd_avg, lambda = 0, biasadj = FALSE, h = 2)
  
  x_avg <<- my_linear_model[["coefficients"]][["(Intercept)"]]
  y_avg <<- my_linear_model[["coefficients"]][["game"]]
  
  ba_forecasted <<- data.frame(
    series = c(1, 2, 3, 4, 5, 6),
    bavg = c(sum(batter_stats$hits[16:20])/sum(batter_stats$at_bats[16:20]), sum(batter_stats$hits[11:15])/sum(batter_stats$at_bats[11:15]), sum(batter_stats$hits[6:10])/sum(batter_stats$at_bats[6:10]), sum(batter_stats$hits[1:5])/sum(batter_stats$at_bats[1:5]), (x_avg+y_avg*5), (x_avg+y_avg*6))
  )
  
  plot(my_forecast, type = "l", main = games_10_title, xlab = "Series", ylab = "Batting Average")
  
}

plot_forecast_next_game_hits <- function() {
  my_linear_model <<- lm(hits~game, data = hitter_forecast_next_game)
  
  my_forecast <<- forecast(my_linear_model, nwd_ng, lambda = 0, biasadj = FALSE, h = 10)

  x_avg <<- my_linear_model[["coefficients"]][["(Intercept)"]]
  y_avg <<- my_linear_model[["coefficients"]][["game"]]
  
  next_game_forecasted <<- data.frame(
    game = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21),
    hits = c(batter_stats$hits[1],batter_stats$hits[2],batter_stats$hits[3],batter_stats$hits[4],batter_stats$hits[5],batter_stats$hits[6],batter_stats$hits[7],batter_stats$hits[8],batter_stats$hits[9],batter_stats$hits[10],batter_stats$hits[11],batter_stats$hits[12],batter_stats$hits[13],batter_stats$hits[14],batter_stats$hits[15],batter_stats$hits[16],batter_stats$hits[17],batter_stats$hits[18],batter_stats$hits[19],batter_stats$hits[20],(x_avg+y_avg*5))
  )
  
  plot(my_forecast, type = "l", main = games_ng_title, xlab = "Game", ylab = "Hits")
}

fetch_mlb_player_stats <- function() {
  batter_stats <<- data.frame()
  
  for (i in gamesPlayedML) {
    batter_stats <<- bind_rows(batter_stats, mlb_player_game_stats(person_id = p, game_pk = i))
  }  
  
  batter_stats <<- batter_stats[(batter_stats$group %in% "hitting"),]
  
  row.names(batter_stats) <<- 1:nrow(batter_stats)
  
  hitter_forecast_avg <<- data.frame(
    game = c(1, 2, 3, 4),
    bavg = c(sum(batter_stats$hits[16:20])/sum(batter_stats$at_bats[16:20]), sum(batter_stats$hits[11:15])/sum(batter_stats$at_bats[11:15]), sum(batter_stats$hits[6:10])/sum(batter_stats$at_bats[6:10]), sum(batter_stats$hits[1:5])/sum(batter_stats$at_bats[1:5]))
  )
  
  hitter_forecast_next_game <<- data.frame(
    game = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
    hits = batter_stats$hits[1:20],
    abs = batter_stats$at_bats[1:20]
  )
  
  nwd_avg <<- data.frame(game = c(5, 6))
  nwd_ng <<- data.frame(game = c(21))
  
  ba <- sum(hitter_forecast_next_game$hits)/sum(batter_stats$at_bats[1:20])
  firstname <<- (selectedPlayer$name_first)
  lastname <<- (selectedPlayer$name_last)
  games_ng_title <<- paste0(firstname," ", lastname, " - Batter Forecast: BA ", round(ba, digits=3))
}

fetch_triplea_player_stats <- function() {
  batter_stats <<- data.frame()
  
  for (i in gamesPlayedAAA) {
    batter_stats <<- bind_rows(batter_stats, mlb_player_game_stats(person_id = p, game_pk = i))
  }  
  
  batter_stats <<- batter_stats[(batter_stats$group %in% "hitting"),]
  
  row.names(batter_stats) <<- 1:nrow(batter_stats)
  
  hitter_forecast_avg <<- data.frame(
    set = c(1, 2, 3, 4),
    hits = c(mean(batter_stats$hits[16:20]), mean(batter_stats$hits[11:15]), mean(batter_stats$hits[6:10]), mean(batter_stats$hits[1:5]))
  )
  
  hitter_forecast_10_games <<- data.frame(
    game = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
    hits = batter_stats$hits[1:20]
  )
  
  nwd_avg <<- data.frame(set = c(5, 6))
  nwd_10 <<- data.frame(game = c(21, 22, 23, 24, 25, 26, 27, 28, 29, 30))
  
  ba <- sum(hitter_forecast_10_games$hits)/sum(batter_stats$at_bats[1:20])
  firstname <<- (selectedPlayer$name_first)
  lastname <<- (selectedPlayer$name_last)
  games_10_title <<- paste0(firstname," ", lastname, " - Batter Forecast: BA ", round(ba, digits=3))
}

fetch_doublea_player_stats <- function() {
  batter_stats <<- data.frame()
  
  for (i in gamesPlayedAA) {
  batter_stats <<- bind_rows(batter_stats, mlb_player_game_stats(person_id = p, game_pk = i))
  }  
  
  batter_stats <<- batter_stats[(batter_stats$group %in% "hitting"),]
  
  row.names(batter_stats) <<- 1:nrow(batter_stats)

  hitter_forecast_avg <<- data.frame(
    set = c(1, 2, 3, 4),
    hits = c(mean(batter_stats$hits[16:20]), mean(batter_stats$hits[11:15]), mean(batter_stats$hits[6:10]), mean(batter_stats$hits[1:5]))
  )
  
  hitter_forecast_10_games <<- data.frame(
    game = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
    hits = batter_stats$hits[1:20]
  )
  
  nwd_avg <<- data.frame(set = c(5, 6))
  nwd_10 <<- data.frame(game = c(21, 22, 23, 24, 25, 26, 27, 28, 29, 30))
  
  ba <- sum(hitter_forecast_10_games$hits)/sum(batter_stats$at_bats[1:20])
  firstname <<- (selectedPlayer$name_first)
  lastname <<- (selectedPlayer$name_last)
  games_10_title <<- paste0(firstname," ", lastname, " - Batter Forecast: BA ", round(ba, digits=3))
}

fetch_player_stats <- function(level) {
  if (level == 1) {
    getGamesPlayedML()
    fetch_mlb_player_stats()
  } else if (level == 2) {
    getGamesPlayedAAA()
    fetch_triplea_player_stats()
  } else if (level == 3) {
    getGamesPlayedAA()
    fetch_doublea_player_stats()
  } else {
    print("Invalid level entered.")
  }
}

selectPlayer <- function(id) {
  p <<- id
  
  selectedPlayer <<- players[(players$key_mlbam %in% p),]
  fangraphsId <<- selectedPlayer$key_fangraphs
  bbrefId <<- selectedPlayer$key_bbref
  bbrefminorsId <<- selectedPlayer$key_bbref_minors
  mlbid <<- selectedPlayer$key_mlbam
}

#Load PBP to find games played in
load("data/pbp.Rdata")

#Load Teams
getMLBTeams()

#Choose Team
getRoster(134)

#Select Player from currentRoster (use "person_id")
selectPlayer(663647)

#Get game stats for games played
fetch_player_stats(1)

#Forecast hit total for next game
plot_forecast_next_game_hits()

#Forecast batting average trend for next 2 series
plot_forecast_avg()
