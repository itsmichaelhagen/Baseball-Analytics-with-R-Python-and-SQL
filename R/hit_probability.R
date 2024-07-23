library(baseballr)
library(dplyr)
library(tidyr)

#Hit probability using available pbp data

load_csv <- function() {
  setwd("~/Documents/Data Analytics/Baseball-Analytics-with-R-Python-and-SQL")
  pbp_data_import <<- read.csv("CSV/mlb_pbp.csv", header = TRUE, sep = ",")
}

load_csv()

hit_probability <- pbp_data_import[ , c(49, 127:129)]
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

# Predict Hit Probability based on ExitVelo, LaunchAngle, and HitDistance

intr <- multiple.regression[["coefficients"]][["(Intercept)"]]
x1 <- multiple.regression[["coefficients"]][["hitData.launchSpeed"]]
x2 <- multiple.regression[["coefficients"]][["hitData.launchAngle"]]
x3 <- multiple.regression[["coefficients"]][["hitData.totalDistance"]]

hitProbability <- function(e, l, d) {
  xHit <<- (intr + x1*e + x2*l + x3*d)
  xHit
}

hitProbability(104, 12, 290)
hitProbability(73, 52, 145)
hitProbability(92, 83, 310)
hitProbability(56, 3, 87)
hitProbability(88, -20, 114)
