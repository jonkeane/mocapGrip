library(mocapGrip)
context("distance calculationss")

load("extractedMarkerData.Rdata") # markerDataHead
load("dist57.RData") # dist57head

test_that("calculateDistances returns the correct distances", {
  expect_equal(mocapGrip:::calculateDistances(markerDataHead, c(5,7)), dist57head)
})

load("meanData.Rdata") # meanDataHead

test_that("meanOnAxis returns the correct distances", {
  expect_equal(mocapGrip:::meanOnAxis(markerDataHead, c(0, 1, 2, 3, 4), axis ="Y"), meanDataHead)
})
