library(mocapGrip)
context("distance calculationss")

load(system.file("tests","testthat","extractedMarkerData.Rdata", package="mocapGrip")) # markerDataHead
load(system.file("tests","testthat","dist57.RData", package="mocapGrip")) # dist57head

test_that("calculateDistances returns the correct distances", {
  expect_equal(mocapGrip:::calculateDistances(markerDataHead, c(5,7)), dist57head)
})

load(system.file("tests","testthat","meanData.Rdata", package="mocapGrip")) # meanDataHead

test_that("meanOnAxis returns the correct distances", {
  expect_equal(mocapGrip:::meanOnAxis(markerDataHead, c(0, 1, 2, 3, 4), axis ="Y"), meanDataHead)
})
