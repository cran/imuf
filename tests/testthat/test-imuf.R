test_that("compUpdate works on real data", {
  initQ <- c(0.9838937550736144, -0.008503796943710444, -0.0026035201363168387, -0.17853287049611438)
  acc <- c(-0.005615234, 0.019042969, -1.004150391)
  gyr <- c(0, -0.009587379919921934, -0.00745685105658519)
  dt <- 0.024
  gain <- 0.1
  qout <- compUpdate(acc, gyr, dt, initQ, gain)
  expect_equal(qout, c(0.9838771143246333, -0.008654456431112963, -0.0025514591206403673, -0.17861806837067223))
})

test_that("compUpdate works for a 90 deg right hand turn", {
  #
  # use a zero gain to stop influence of acc
  # a 90 deg right hand turn around the x-axis turns a (1, 0, 0, 0) quat to
  # (1/sqrt(2), 1/sqrt(2), 0, 0)
  # need a gyro (pi/2, 0, 0) for 1 sec
  #
  initQ <- c(1, 0, 0, 0)
  acc <- c(0, 0, -1)
  gyr <- c(pi/2, 0, 0)
  dt <- 1
  gain <- 0
  qout <- compUpdate(acc, gyr, dt, initQ, gain)
  expect_equal(qout, c(1/sqrt(2), 1/sqrt(2), 0, 0))
})

test_that("compUpdate works for consecutive 45 deg right hand turn", {
  #
  # use a zero gain to stop influence of acc
  # two 45 deg right hand turn around the x-axis turns a (1, 0, 0, 0) quat to
  # (1/sqrt(2), 1/sqrt(2), 0, 0)
  # need a gyro (pi/2, 0, 0) for 0.5 sec
  #
  initQ <- c(1, 0, 0, 0)
  acc <- c(0, 0, -1)
  gyr <- c(pi/2, 0, 0)
  dt <- 0.5
  gain <- 0
  qout <- compUpdate(acc, gyr, dt, initQ, gain)   # 1st rotation
  qout <- compUpdate(acc, gyr, dt, qout, gain)    # 2nd rotation, using result from 1st
  expect_equal(qout, c(1/sqrt(2), 1/sqrt(2), 0, 0))
})

test_that("compUpdate works for gain = 1", {
  #
  # use a 1 gain to give acc full control
  # a acc = c(0, -1, 0) means a pi/2 rotation around x-axis
  #
  initQ <- c(1, 0, 0, 0)
  acc <- c(0, -1, 0)
  gyr <- c(0, 0, 0)
  dt <- 0.5
  gain <- 1
  #
  qout <- compUpdate(acc, gyr, dt, initQ, gain)
  expect_equal(qout, c(1/sqrt(2), 1/sqrt(2), 0, 0))
})

test_that("rotV works for rotating a east vector by 90 deg about north", {
  q <- c(cos(pi/4), sin(pi/4), 0, 0)
  vin <- c(0, 1, 0)
  vout <- rotV(q, vin)
  expect_equal(vout, c(0, 0, 1))
})
