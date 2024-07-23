library(baseballr)
library(dplyr)
library(tidyr)

#Hit probability using available pbp data

## Add a function that imports pbp data from csv file as "pbp_data_import"

hit_probability <- pbp_data_import[ , c(48, 126:128)]
hit_probability <- na.omit(hit_probability)

hit_probability$result.event <- as.character(hit_probability$result.event)

changetoHit <- function() {
  hit_probability$result.event[hit_probability$result.event == "Single"] <<- 1
  hit_probability$result.event[hit_probability$result.event == "Double"] <<- 1
  hit_probability$result.event[hit_probability$result.event == "Triple"] <<- 1
  hit_probability$result.event[hit_probability$result.event == "Home Run"] <<- 1
}

changetoOut <- function() {
  hit_probability$result.event[hit_probability$result.event != 1] <<- 0
}

changetoHit()
changetoOut()

hit_probability$result.event <- as.integer(hit_probability$result.event)

#Begin Multiple Regression Analysis

plot(hit_probability)

multiple.regression <- lm(result.event ~ hitData.launchSpeed + hitData.launchAngle + hitData.totalDistance, data=hit_probability)
summary(multiple.regression)