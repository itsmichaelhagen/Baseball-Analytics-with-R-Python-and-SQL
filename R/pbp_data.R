library(baseballr)
library(dplyr)
library(tidyr)

load_schedules <- function() {
    majorleague_schedule <<- mlb_schedule(season = 2024, level_ids = 1)
    triplea_schedule <<- mlb_schedule(season = 2024, level_ids = 11)
    doublea_schedule <<- mlb_schedule(season = 2024, level_ids = 12)
}

load_completedgames <- function() {
    mlb_completedgames <<- majorleague_schedule[majorleague_schedule$game_type %in% "R",]
    triplea_completedgames <<- triplea_schedule[triplea_schedule$game_type %in% "R",]
    doublea_completedgames <<- doublea_schedule[doublea_schedule$game_type %in% "R",]
}

filter_finalized <- function() {
    mlb_completedgames <<- mlb_completedgames[(mlb_completedgames$status_abstract_game_state %in% "Final"),]
    triplea_completedgames <<- triplea_completedgames[(triplea_completedgames$status_abstract_game_state %in% "Final"),]
    doublea_completedgames <<- doublea_completedgames[(doublea_completedgames$status_abstract_game_state %in% "Final"),]
}

drop_postponed <- function() {
    mlb_completedgames <<- mlb_completedgames[!(mlb_completedgames$status_detailed_state %in% "Postponed"),]
    triplea_completedgames <<- triplea_completedgames[!(triplea_completedgames$status_detailed_state %in% "Postponed"),]
    doublea_completedgames <<- doublea_completedgames[!(doublea_completedgames$status_detailed_state %in% "Postponed"),]
}

drop_cancelled <- function() {
    mlb_completedgames <<- mlb_completedgames[!(mlb_completedgames$status_detailed_state %in% "Cancelled"),]
    triplea_completedgames <<- triplea_completedgames[!(triplea_completedgames$status_detailed_state %in% "Cancelled"),]
    doublea_completedgames <<- doublea_completedgames[!(doublea_completedgames$status_detailed_state %in% "Cancelled"),]
}

yesterday_games <- function(d) {
  yesterday_mlb_games_pbp <<- mlb_completedgames[(mlb_completedgames$date %in% d),]
  yesterday_triplea_games_pbp <<- triplea_completedgames[(triplea_completedgames$date %in% d),]
  yesterday_doublea_games_pbp <<- doublea_completedgames[(doublea_completedgames$date %in% d),]
}

find_gameids_yesterday <- function() {
  mlb_gameids_yesterday <<- as.vector(yesterday_mlb_games_pbp$game_pk)
  triplea_gameids_yesterday <<- as.vector(yesterday_triplea_games_pbp$game_pk)
  doublea_gameids_yesterday <<- as.vector(yesterday_doublea_games_pbp$game_pk)
}

create_pbp <- function() {
  majorleague_pbp <<- data.frame()
  triplea_pbp <<- data.frame()
  doublea_pbp <<- data.frame()
}

load_pbp_csv <- function() {
  setwd("~/Documents/Data Analytics/Baseball-Analytics-with-R-Python-and-SQL")
  mlb_pbp_export <<- read.csv("CSV/mlb_pbp.csv", header = TRUE, sep = ",")
  triplea_pbp_export <<- read.csv("CSV/triplea_pbp.csv", header = TRUE, sep = ",")
  doublea_pbp_export <<- read.csv("CSV/doublea_pbp.csv", header = TRUE, sep = ",")
}

fetch_mlb_pbp <- function() {
  for (i in mlb_gameids_yesterday) {
    majorleague_pbp <<- bind_rows(majorleague_pbp, mlb_pbp(i))
  }
}

fetch_triplea_pbp <- function() {
  for (i in triplea_gameids_yesterday) {
    triplea_pbp <<- bind_rows(triplea_pbp, mlb_pbp(i))
  }
}

fetch_doublea_pbp <- function() {
  for (i in doublea_gameids_yesterday) {
    doublea_pbp <<- bind_rows(doublea_pbp, mlb_pbp(i))
  }
}

fetch_pbp <- function() {
  print("Load MLB Pitch-by-Pitch...")
  fetch_mlb_pbp()
  print("Done!")
  print("Load Triple-A Pitch-by-Pitch...")
  fetch_triplea_pbp()
  print("Done!")
  print("Load Double-A Pitch-by-Pitch...")
  fetch_doublea_pbp()
  print("Finished!")
  majorleague_pbp <<- majorleague_pbp[order(majorleague_pbp$game_date),]
  triplea_pbp <<- triplea_pbp[order(triplea_pbp$game_date),]
  doublea_pbp <<- doublea_pbp[order(doublea_pbp$game_date),]
}

setup_pbp <- function() {
  load_schedules()
  load_completedgames()
  filter_finalized()
  drop_postponed()
  drop_cancelled()
}

prepare_exports <- function() {
  mlb_pbp_export <<- apply(majorleague_pbp,2,as.character)
  triplea_pbp_export <<- apply(triplea_pbp,2,as.character)
  doublea_pbp_export <<- apply(majorleague_pbp,2,as.character)
}

save_data <- function () {
  setwd("~/Documents/Data Analytics/Baseball-Analytics-with-R-Python-and-SQL")
  save(majorleague_pbp, triplea_pbp, doublea_pbp, file="data/pbp.Rdata")
}

export_pbp <- function() {
  setwd("~/Documents/Data Analytics/Baseball-Analytics-with-R-Python-and-SQL")
  write.csv(mlb_pbp_export, file="data/mlb_pbp.csv")
  write.csv(triplea_pbp_export, file="data/triplea_pbp.csv")
  write.csv(doublea_pbp_export, file="data/doublea_pbp.csv")
}

#Working Directory
setwd("~/Documents/Data Analytics/Baseball-Analytics-with-R-Python-and-SQL")

#Setup Data (Schedules, Finished Games, etc.)
setup_pbp()

#Load stored pitch-by-pitch data
load("data/pbp.Rdata")

#Pull pitch-by-pitch information from date
yesterday_games(d = "2024-07-24")
find_gameids_yesterday()

#Fetch pitch-by-pitch data using gameids
fetch_pbp()

#Save data
save_data()