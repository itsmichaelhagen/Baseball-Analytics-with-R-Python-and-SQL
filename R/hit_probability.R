library(baseballr)
library(dplyr)
library(tidyr)

#Working Directory
setwd("~/Documents/Data Analytics/Baseball-Analytics-with-R-Python-and-SQL")

#Hit probability using available pbp data

load("data/pbp.Rdata")

chooseData <- function(level) {
  if (level == 1) {
    hit_probability <<- majorleague_pbp[ , c(48, 126:128)]
    hit_probability <<- na.omit(hit_probability)
  } else if (level == 11) {
    hit_probability <<- triplea_pbp[ , c(48, 126:128)]
    hit_probability <<- na.omit(hit_probability)
  } else {
    print("Invalid input for data. Enter a valid level. [1: Major League PBP][11: Triple-A PBP]")
  }
}


# Select DataSet: 1 (Major League Data), 11 (Triple-A Data)
chooseData(11)

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

#Finding Hit Probability Average

veloAvg <- mean(hit_probability$hitData.launchSpeed)
angleAvg <- mean(hit_probability$hitData.launchAngle)
distAvg <- mean(hit_probability$hitData.totalDistance)

hitProbability(veloAvg, angleAvg, distAvg)
