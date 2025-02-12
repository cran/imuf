test_that("imu_object has correct data", {
  a <- imu_object()

  # data is expected to be the default c(1, 0, 0, 0)
  expect_equal(a$x$data, c(1, 0, 0, 0))

  q <- c(cos(pi/4), 0, sin(pi/4), 0, 0)
  a <- imu_object(q)

  # data is expected to be the same as q
  expect_equal(a$x$data, q)
})
