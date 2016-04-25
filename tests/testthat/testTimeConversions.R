library(mocapGrip)
context("Time conversions")

test_that("timeToSecs returns the right number", {
  expect_equal(timeToSecs("01:01:01"), 60*60+1)
})
