## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(imuf)
library(purrr)
library(ggplot2)

## -----------------------------------------------------------------------------
head(walking_shin_1)

## -----------------------------------------------------------------------------
lst_ned_in <- as.list(as.data.frame(t(walking_shin_1))) %>% unname
head(lst_ned_in, 2)

## -----------------------------------------------------------------------------
myCompUpdate <- function(initQ, accgyr) {
  acc <- accgyr[1:3]
  gyr <- accgyr[4:6]
  dt <- 1/50
  gain <- 0.1
  orientation <- compUpdate(acc, gyr, dt, initQ, gain)
  orientation
}

## -----------------------------------------------------------------------------
(q1 <- myCompUpdate(c(1, 0, 0, 0), lst_ned_in[[1]]))
(q2 <- myCompUpdate(q1, lst_ned_in[[2]]))

## -----------------------------------------------------------------------------
orientations <- purrr::accumulate(lst_ned_in, myCompUpdate, .init = c(1, 0, 0, 0))
head(orientations, 5)

## -----------------------------------------------------------------------------
q <- c(cos(pi/4), sin(pi/4), 0, 0)
vin <- c(0, 1, 0)
rotV(q, vin)

## -----------------------------------------------------------------------------
getTurnAngle <- function(quat) {
  # a function to rotate c(1, 0, 0) by quat
  # and then compute the angle between (1, 0, 0) and the rotated vector
  # projected onto the n-e plane and 
  # this construct is to detect turns
  rotVec <- rotV(quat, c(1, 0, 0))
  theta <- atan2(rotVec[2], rotVec[1]) * 180 / pi
  theta
}

## -----------------------------------------------------------------------------
turnAngles <- orientations %>% purrr::map(getTurnAngle) %>% unlist()
head(turnAngles)

## -----------------------------------------------------------------------------
#
# create a vector of time stamps in minutes
# note that sampling frequency is 50 Hz
x <- 1:length(turnAngles) / 50 / 60
#
ggplot2::ggplot(mapping = aes(x = x, y = turnAngles)) + ggplot2::geom_line()

## -----------------------------------------------------------------------------
#
# a function to remove artificial jumps in turn angles
rmJumps <- function(theta) {
  firstDiffs <- diff(theta)
  bigDiffIdx <- which(abs(firstDiffs) > 100)
  #
  # fix #1
  theta[(bigDiffIdx[1]+1):bigDiffIdx[2]] <- theta[(bigDiffIdx[1]+1):bigDiffIdx[2]] + 360
  #
  # fix #2
  theta[(bigDiffIdx[3]+1):bigDiffIdx[4]] <- theta[(bigDiffIdx[3]+1):bigDiffIdx[4]] + 360
  #
  # fix #3
  theta[(bigDiffIdx[4]+1):length(theta)] <- theta[(bigDiffIdx[4]+1):length(theta)] + 2*360
  theta
}
#
# remove artificial jumps
turnAnglesNoJumps <- rmJumps(turnAngles)
#
# plot it
ggplot2::ggplot(mapping = aes(x = x, y = turnAnglesNoJumps)) + ggplot2::geom_line()

## -----------------------------------------------------------------------------
#
# zero in on +/- 10 sec around 5 minute mark
idx_5min <- c(14800:15750)
x_5min <- x[idx_5min]
turn_5min <- turnAnglesNoJumps[idx_5min]
#
# plot it
ggplot2::ggplot(mapping = aes(x = x_5min, y = turn_5min)) + ggplot2::geom_line()

