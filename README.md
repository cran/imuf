
<!-- README.md is generated from README.Rmd. Please edit that file -->

# imuf <a href="https://gitboosting.github.io/imuf/"><img src="man/figures/logo.png" align="right" height="139" alt="imuf website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/gitboosting/imuf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gitboosting/imuf/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

‘imuf’ uses complementary filtering to estimate the orientation of an
inertial measurement unit (IMU) with a 3-axis accelerometer and a 3-axis
gyroscope. It takes the IMU’s accelerometer and gyroscope readings, time
duration, its initial orientation, and a ‘gain’ factor as inputs, and
provides an estimate of the final orientation as outputs.

‘imuf’ adopts the north-east-down (NED) coordinate system. The initial
and final orientations are expressed as quaternions in (w, x, y, z)
convention.

## Installation

``` r
install.packages("imuf")
```

You can install the development version of imuf from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("gitboosting/imuf")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(imuf)
#
acc <- c(0, 0, -1)         # accelerometer NED readings in g (~ 9.81 m/s^2) 
gyr <- c(1, 0, 0)          # gyroscope NED readings in radians per second
deltat <- 0.1              # time duration in seconds
initq <- c(1, 0, 0, 0)     # initial orientation expressed as a quaternion
gain <- 0.1                # a weight (0-1) given to the accelerometer readings
#
# final orientation expressed as a quaternion
(final <- compUpdate(acc, gyr, deltat, initq, gain))
#> [1] 0.99898767 0.04498481 0.00000000 0.00000000
#
#
# rotate a vector pointing east by 90 deg about north
q <- c(cos(pi/4), sin(pi/4), 0, 0)   # quaternion to rotate 90 deg about north
vin <- c(0, 1, 0)                    # vector in east direction
(vout <- rotV(q, vin))               # after rotation, vector is in down direction
#> [1] 0.000000e+00 2.220446e-16 1.000000e+00
```
