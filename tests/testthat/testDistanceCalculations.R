library(mocapGrip)
context("distance calculationss")

load(file.path('extractedMarkerData.RData')) # markerDataHead
load(file.path('dist57.RData')) # dist57head
load(file.path('meanData.RData')) # meanDataHead

test_that("calculateDistances returns the correct distances", {
  expect_equal(calculateDistances(markerDataHead, c(5,7)), dist57head)
})

test_that("meanOnAxis returns the correct distances", {
  expect_equal(meanOnAxis(markerDataHead, c(0, 1, 2, 3, 4), axis ="Y"), meanDataHead)
})
