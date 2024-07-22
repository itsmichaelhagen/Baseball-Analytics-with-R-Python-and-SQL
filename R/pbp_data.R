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

find_gameids <- function() {
  mlb_gameids <<- as.vector(yesterday_mlb_games_pbp$game_pk)
  triplea_gameids <<- as.vector(yesterday_triplea_games_pbp$game_pk)
  doublea_gameids <<- as.vector(yesterday_doublea_games_pbp$game_pk)
}

create_pbp <- function() {
  majorleague_pbp <<- data.frame()
  triplea_pbp <<- data.frame()
  doublea_pbp <<- data.frame()
}

fetch_mlb_pbp <- function() {
  for (i in mlb_gameids) {
    majorleague_pbp <<- bind_rows(majorleague_pbp, mlb_pbp(i))
  }
}

fetch_triplea_pbp <- function() {
  for (i in triplea_gameids) {
    triplea_pbp <<- bind_rows(triplea_pbp, mlb_pbp(i))
  }
}

fetch_doublea_pbp <- function() {
  for (i in doublea_gameids) {
    doublea_pbp <<- bind_rows(doublea_pbp, mlb_pbp(i))
  }
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

export_pbp <- function() {
  write.csv(mlb_pbp_export, file="Downloads/mlb_pbp.csv")
  write.csv(triplea_pbp_export, file="Downloads/triplea_pbp.csv")
  write.csv(doublea_pbp_export, file="Downloads/doublea_pbp.csv")
}

setup_pbp()

create_pbp()

yesterday_games(d = "2024-07-21")

find_gameids()

fetch_mlb_pbp()
fetch_triplea_pbp()
fetch_doublea_pbp()

prepare_exports()

export_pbp()