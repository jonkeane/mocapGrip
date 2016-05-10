library(mocapGrip)
context("Time conversions")

test_that("timeToSecs returns the right number", {
  expect_equal(timeToSecs("01:01:01:00"), 60*60+60+1+(1*(1/30)))
  expect_equal(timeToSecs("02:02:02:01"), 2*60*60+2*60+2+(2*(1/30)))
  expect_equal(timeToSecs("03:03:03:02"), 3*60*60+3*60+3+(3*(1/30)))
  expect_equal(timeToSecs("01:01:01:15", fps = 60), 60*60+60+1+(16*(1/60)))
})
