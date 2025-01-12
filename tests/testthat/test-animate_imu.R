library(purrr)
#
lst_ned_in <- as.list(as.data.frame(t(walking_shin_1))) %>% unname
#
# sampling rate (hz)
rate <- 50
myCompUpdate <- function(initQ, accgyr) {
  acc <- accgyr[1:3]
  gyr <- accgyr[4:6]
  dt <- 1/rate
  gain <- 0.1
  orientation <- compUpdate(acc, gyr, dt, initQ, gain)
  orientation
}
orientations <- purrr::accumulate(lst_ned_in, myCompUpdate, .init = c(1, 0, 0, 0))
#
# duration between samples in millis
dur <- 1/rate * 1000

test_that("animate_imu has correct data", {
  a <- animate_imu(orientations, dur)

  # data is equal to the list of orientations
  expect_equal(a$x$data, orientations)

  # dt is an numeric equals to dur
  expect_equal(a$x$dt, dur)

  # widget does not work without either data or dt
  expect_error(animate_imu())
  expect_error(animate_imu(data = orientation))
  expect_error(animate_imu(dt = dur))
})
